#!/usr/bin/env python3
"""Auto-approve approval dialogs of the agy (Antigravity) agent in a herdr or tmux pane.

Hooks cannot approve prompts an organization's admin policy forces, so this
watches the pane instead: when an approval dialog is on screen it reads the
dialog text and answers it. Yes/no questions get "y"; option menus get "always
allow in this conversation" when offered, otherwise the recommended option (an
explicit label if present, else the highlighted default). A subagent's
approval shows as an alert above the prompt instead of a menu, while the main
agent keeps working; it is approved with the alert's ctrl+k.

Inside agy, `! autoapprove on|off|status` controls the pane agy runs in. A
watcher started that way exits with its agy session. `autoapprove default on`
makes the plugin's SessionStart hook start one in every new agy session.
"""

import argparse
import datetime
import fcntl
import json
import os
import re
import signal
import subprocess
import sys
import time
from pathlib import Path

SCRIPT = Path(__file__).resolve()
CONFIG = SCRIPT.parent.parent / "autoapprove.json"
PANE = os.environ.get("AGY_PANE", "w7:p4N")
LOG = os.environ.get(
    "AGY_AUTOAPPROVE_LOG", os.path.expanduser("~/.local/state/agy-autoapprove.log")
)
POLL_SECS = float(os.environ.get("AGY_POLL_SECS", "2"))
AGENT = "agy"
TMUX = False  # pane adapter: herdr (default) or tmux
UNTIL_PID = None  # exit once this agy process is gone
POST_KEY_WAIT = 1.0
UNBLOCK_TIMEOUT = 8.0
MAX_FAILURES = 5
TMUX_KEYS = {"enter": "Enter", "up": "Up", "down": "Down", "ctrl+k": "C-k"}

APPROVAL_RE = re.compile(
    r"allow|approve|permission|proceed|trust|execute|run (?:the )?command|\byes\b",
    re.I,
)
YN_RE = re.compile(r"\((?:y/n)\)|\[(?:y/n|Y/n)\]|\by/n\b", re.I)
NUM_OPT_RE = re.compile(r"^\s*([❯>•*➜●○])?\s*([0-9])\s*[).:\-]\s*(.+?)\s*$")
RADIO_RE = re.compile(r"^\s*([❯>•*➜●○])\s*(.+?)\s*$")
MENU_HINT_RE = re.compile(r"↑/↓\s*Navigate")
ALWAYS_CONVO_RE = re.compile(r"always allow|allow all|don'?t ask", re.I)
PERSIST_RE = re.compile(r"persist|settings\.json", re.I)
RECOMMEND_RE = re.compile(r"recommend", re.I)
NO_RE = re.compile(r"\bno\b|deny|reject|cancel|never", re.I)
TRANSCRIPT_PREFIX_RE = re.compile(r"^[┃│⎿●○·…↓⣾⣿⣟⢁⡀>]")
BOX_PREFIX_RE = re.compile(r"^\s*[┃│]\s?")
SKIP_OPT_RE = re.compile(r"^[/┃│⎿●○·…↓⣾⣿⣟⢁⡀\[]")
# Subagent alert: "┃ <agent> needs approval for <tool>" ... "┃ ctrl+k approve · alt+j manage"
SUBAGENT_ASK_RE = re.compile(r"^[ \t]*┃[ \t]*(\S.* needs approval for \S.*)$", re.M)
SUBAGENT_HINT_RE = re.compile(r"^[ \t]*┃[ \t]*ctrl\+k approve\b", re.M)


def strip_box(line):
    while True:
        stripped = BOX_PREFIX_RE.sub("", line, count=1)
        if stripped == line:
            return line
        line = stripped


def log(msg):
    line = "%s [%s] %s" % (
        datetime.datetime.now().isoformat(timespec="seconds"),
        PANE,
        msg,
    )
    print(line, flush=True)
    with open(LOG, "a") as f:
        f.write(line + "\n")


def run(cmd, timeout=20):
    try:
        return subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
    except Exception as e:
        log("%s failed: %s" % (" ".join(cmd), e))
        return None


def herdr(*args, timeout=20):
    return run(["herdr", *args], timeout=timeout)


# --- Pane adapter: herdr agent API, or tmux capture-pane / send-keys ---------------


def agent_status(pane):
    if TMUX:
        # tmux has no agent state; "idle" limits answers to full menus with a question.
        return "idle" if pane_exists(pane) else None
    r = herdr("agent", "get", pane)
    if r is None or r.returncode != 0:
        return None
    try:
        return json.loads(r.stdout)["result"]["agent"]["agent_status"]
    except Exception:
        return None


def find_agent_panes():
    if TMUX or UNTIL_PID:  # a bound watcher never wanders to another pane
        return []
    r = herdr("agent", "list")
    if r is None or r.returncode != 0:
        return []
    try:
        return [
            a["pane_id"]
            for a in json.loads(r.stdout)["result"]["agents"]
            if a.get("agent") == AGENT
        ]
    except Exception:
        return []


def find_agent_pane():
    panes = find_agent_panes()
    return panes[0] if panes else None


def pane_exists(target):
    if TMUX:
        r = run(["tmux", "display-message", "-p", "-t", target, "#{pane_id}"])
    else:
        r = herdr("pane", "get", target)
    return r is not None and r.returncode == 0


def read_detection(pane, lines=40):
    if TMUX:
        # Visible screen only: scrollback can hold dialogs that were already answered.
        r = run(["tmux", "capture-pane", "-p", "-J", "-t", pane])
    else:
        r = herdr("agent", "read", pane, "--source", "detection", "--lines", str(lines))
    if r is None or r.returncode != 0:
        return ""
    return r.stdout


def send(pane, *keys):
    log("send-keys %s" % " ".join(keys))
    if TMUX:
        r = run(["tmux", "send-keys", "-t", pane, *(TMUX_KEYS.get(k, k) for k in keys)])
    else:
        r = herdr("agent", "send-keys", pane, *keys)
    if r is None or r.returncode != 0:
        log("send-keys failed: %s" % (r.stderr.strip() if r else "no result"))
        return False
    return True


def wait_unblocked(pane, timeout=UNBLOCK_TIMEOUT):
    deadline = time.time() + timeout
    while time.time() < deadline:
        if TMUX:
            # No agent state in tmux: wait for the dialog to leave the screen, or a slow
            # redraw gets the same dialog answered twice (a stray key in the prompt).
            if not dialog_pending(read_detection(pane), "idle"):
                return True
        elif agent_status(pane) != "blocked":
            return True
        time.sleep(0.5)
    return False


# --- Dialog recognition -------------------------------------------------------


def parse_numbered_options(text):
    opts = []
    for line in text.splitlines():
        m = NUM_OPT_RE.match(strip_box(line))
        if m and not NO_RE.search(m.group(3)) and not SKIP_OPT_RE.match(m.group(3).lstrip()):
            opts.append((m.group(2), m.group(3), m.group(1) in ("❯", ">", "➜")))
    return opts


def parse_radio_options(text):
    opts = []
    for line in text.splitlines():
        m = RADIO_RE.match(strip_box(line))
        if m and not SKIP_OPT_RE.match(m.group(2).lstrip()):
            opts.append((m.group(1), m.group(2)))
    return opts


def dialog_region(text):
    """(options block, has_question_header) for the dialog above the key hint."""
    lines = text.splitlines()
    end = len(lines)
    for i in range(len(lines) - 1, -1, -1):
        if MENU_HINT_RE.search(lines[i]):
            end = i
            break
    start = end
    while start > 0 and not lines[start - 1].strip():
        start -= 1
    question = False
    while start > 0:
        line = lines[start - 1]
        # A multi-line command quoted in an option can hold single blank lines;
        # two in a row, or a rule, end the dialog.
        if not line.strip() and (start < 2 or not lines[start - 2].strip()):
            break
        if line.strip().startswith("─"):
            break
        s = strip_box(line).strip()
        if not TRANSCRIPT_PREFIX_RE.match(s) and (
            s.endswith("?") or "Requesting permission" in s
        ):
            question = True
            break
        start -= 1
    if not question:
        for line in lines[max(0, start - 6) : start]:
            s = strip_box(line).strip()
            if TRANSCRIPT_PREFIX_RE.match(s):
                continue
            if s.endswith("?") or "Requesting permission" in s:
                question = True
    return "\n".join(lines[start:end]), question


def choose_option(opts):
    """opts: (key, text, marked). In-conversation always-allow wins; then a
    recommended label; then the highlighted default."""
    for key, txt, _ in opts:
        if ALWAYS_CONVO_RE.search(txt) and not PERSIST_RE.search(txt):
            return key
    for key, txt, _ in opts:
        if RECOMMEND_RE.search(txt):
            return key
    for key, txt, marked in opts:
        if marked:
            return key
    return None


def respond(pane, snap, status):
    """Answer one approval dialog. Returns True when keys were sent."""
    tail = "\n".join(snap.splitlines()[-40:])
    region, _ = dialog_region(tail)

    numbered = parse_numbered_options(region)
    if numbered:
        choice = choose_option(numbered)
        if choice is not None:
            txt = next(t for d, t, _ in numbered if d == choice)
            log("dialog: numbered options, picking %s) %s" % (choice, txt))
            send(pane, choice)
        else:
            log("dialog: numbered options, accepting highlighted default")
            send(pane, "enter")
        return True

    radio = parse_radio_options(region)
    if radio:
        marked = [i for i, (m, _) in enumerate(radio) if m in "❯>➜"]
        current = marked[0] if marked else 0
        target = choose_option(
            [(i, t, i == current) for i, (_, t) in enumerate(radio)]
        )
        if target is None:
            target = current
        log("dialog: option list, picking %r (move %d)" % (radio[target][1], target - current))
        key = "down" if target > current else "up"
        for _ in range(abs(target - current)):
            send(pane, key)
            time.sleep(0.15)
        send(pane, "enter")
        return True

    if status == "blocked":
        if YN_RE.search(region) or YN_RE.search(tail):
            log("dialog: yes/no confirmation")
            send(pane, "y")
            time.sleep(POST_KEY_WAIT)
            if agent_status(pane) == "blocked":
                send(pane, "enter")
            return True
        if APPROVAL_RE.search(tail):
            log("dialog: approval-like prompt, accepting default")
            send(pane, "enter")
            return True

    log("no approval dialog recognized; no keys sent")
    return False


def dialog_pending(snap, status):
    tail = "\n".join(snap.splitlines()[-40:])
    region, question = dialog_region(tail)
    if parse_numbered_options(region) or parse_radio_options(region):
        return question and MENU_HINT_RE.search(tail) is not None
    return status == "blocked" and bool(YN_RE.search(tail))


def subagent_pending(snap):
    tail = "\n".join(snap.splitlines()[-40:])
    return bool(SUBAGENT_ASK_RE.search(tail) and SUBAGENT_HINT_RE.search(tail))


def subagent_alert(snap):
    """The alert's box lines (ask, tool call, hint): differs per step, so a new one is told apart."""
    tail = "\n".join(snap.splitlines()[-40:])
    return "\n".join(l.strip() for l in tail.splitlines() if l.lstrip().startswith("┃"))


def approve_subagent(pane):
    """Approve a subagent's step with the alert's ctrl+k. True once that alert is gone or replaced."""
    before = read_detection(pane)
    m = SUBAGENT_ASK_RE.search(before)
    if not m:  # gone since the last read: a stray ctrl+k would cut the typed prompt
        return True
    sent = subagent_alert(before)
    log("dialog: %s; ctrl+k" % m.group(1).strip())
    if not send(pane, "ctrl+k"):
        return False
    deadline = time.time() + UNBLOCK_TIMEOUT
    while time.time() < deadline:
        time.sleep(0.5)
        # A busy subagent raises its next alert at once: a changed alert is progress, not a
        # failure. shortcut: an identical consecutive alert (same tool call) reads as stuck.
        after = read_detection(pane)
        if not subagent_pending(after) or subagent_alert(after) != sent:
            return True
    return False


def watch():
    """Watch PANE until killed, or until UNTIL_PID exits. Returns an exit code."""
    global PANE
    if not pane_exists(PANE):
        candidates = find_agent_panes()
        if len(candidates) == 1:
            log("target %s is not a pane id; watching %s instead" % (PANE, candidates[0]))
            PANE = candidates[0]
        elif len(candidates) > 1:
            sys.exit(
                "error: %r is not a pane id and agent %r is live in several panes: %s\n"
                "pass one of those pane ids explicitly" % (PANE, AGENT, ", ".join(candidates))
            )
        else:
            sys.exit("error: %r is not a pane id and no live %r agent was found" % (PANE, AGENT))
    os.makedirs(os.path.dirname(LOG), exist_ok=True)
    lock_path = "%s.%s.lock" % (LOG, re.sub(r"[^A-Za-z0-9_.-]", "_", PANE))
    with open(lock_path, "w") as lockf:
        try:
            fcntl.flock(lockf, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            log("another instance is already running; exiting")
            return 1

        log("monitoring %s pane %s for agy approval dialogs" % ("tmux" if TMUX else "herdr", PANE))
        pane = PANE
        failures = 0
        while True:
            if UNTIL_PID and not pid_alive(UNTIL_PID):
                log("agy session %d ended; exiting" % UNTIL_PID)
                return 0
            status = agent_status(pane)
            if status is None:
                found = find_agent_pane()
                if found:
                    log("pane rediscovered: %s" % found)
                    pane = found
                else:
                    log("agy agent not found; retrying")
                time.sleep(max(POLL_SECS, 5))
                continue

            snap = read_detection(pane)
            if subagent_pending(snap) and not dialog_pending(snap, status):
                if approve_subagent(pane):
                    log("subagent approved")
                    failures = 0
                else:
                    failures += 1
                    log("subagent alert still pending after ctrl+k (failure %d)" % failures)
                    # Evidence for why ctrl+k was ignored: status, and both screen sources.
                    r = herdr("agent", "read", pane, "--source", "visible")
                    log("=== failure state=%s detection ===\n%s\n=== visible ===\n%s\n=== end ===" % (
                        agent_status(pane), read_detection(pane), r.stdout if r else ""))
                    if failures >= MAX_FAILURES:
                        log("too many failures; backing off 60s")
                        time.sleep(60)
                        failures = 0
                time.sleep(POLL_SECS)
                continue

            if status in ("blocked", "idle", "done"):
                if not dialog_pending(snap, status):
                    time.sleep(POLL_SECS)
                    continue
                # re-read immediately before acting to shrink the race window
                snap = read_detection(pane, lines=80)
                if not dialog_pending(snap, status):
                    time.sleep(POLL_SECS)
                    continue
                log("=== dialog detected (state=%s) ===\n%s\n=== end ===" % (status, snap))
                if respond(pane, snap, status) and wait_unblocked(pane):
                    log("dialog approved; agent resumed")
                    failures = 0
                else:
                    failures += 1
                    log("dialog still pending after response (failure %d)" % failures)
                    if failures >= MAX_FAILURES:
                        log("too many failures; backing off 60s")
                        time.sleep(60)
                        failures = 0
                        continue
                time.sleep(POLL_SECS)
                continue

            failures = 0
            time.sleep(POLL_SECS)


# --- Control: on / off / status / default, and the SessionStart hook ----------------


def pid_alive(pid):
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def agy_ancestor():
    """PID of the nearest `agy` process above this one (Linux /proc), or None."""
    pid = os.getppid()
    while pid > 1:
        try:
            stat = Path("/proc/%d/stat" % pid).read_text()
        except OSError:
            return None
        comm, rest = stat[stat.index("(") + 1 : stat.rindex(")")], stat[stat.rindex(")") + 2 :]
        if comm == AGENT:
            return pid
        pid = int(rest.split()[1])
    return None


def this_pane():
    """(pane, is_tmux) for the pane agy runs in; innermost multiplexer wins."""
    if os.environ.get("TMUX_PANE"):
        return os.environ["TMUX_PANE"], True
    if os.environ.get("HERDR_PANE_ID"):
        return os.environ["HERDR_PANE_ID"], False
    return None, False


def watchers(pane):
    """PIDs of watcher processes (this script or the old agy-autoapprove.py) for `pane`."""
    # shortcut: tmux pane ids (%N) repeat across tmux servers, so a second server's %N
    # looks already watched; key by socket too if you run several tmux servers.
    pids = []
    for proc in Path("/proc").glob("[0-9]*"):
        try:
            argv = (proc / "cmdline").read_bytes().split(b"\0")
        except OSError:
            continue
        args = [a.decode(errors="replace") for a in argv]
        if pane in args and any(a.endswith("autoapprove.py") for a in args):
            pids.append(int(proc.name))
    return pids


def read_auto_start():
    try:
        return bool(json.loads(CONFIG.read_text()).get("autoStart", False))
    except (OSError, ValueError):
        return False


def start(pane, tmux, until_pid):
    cmd = [sys.executable, str(SCRIPT), "watch", pane, "--log", LOG]
    if tmux:
        cmd.append("--tmux")
    if until_pid:
        cmd += ["--until-pid", str(until_pid)]
    subprocess.Popen(
        cmd,
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
    )


def control(cmd):
    """Run on/off/status/default for the pane this agy session runs in."""
    if cmd[0] == "default":
        if len(cmd) == 2 and cmd[1] in ("on", "off"):
            CONFIG.write_text(json.dumps({"autoStart": cmd[1] == "on"}, indent=2) + "\n")
        elif len(cmd) != 1:
            sys.exit("usage: autoapprove default [on|off]")
        print("autoapprove: start in every new agy session: %s" % ("on" if read_auto_start() else "off"))
        return 0

    pane, tmux = this_pane()
    if pane is None:
        sys.exit(
            "autoapprove: not inside a herdr or tmux pane; a plain terminal gives no way "
            "to read or type into agy from outside"
        )
    where = "%s pane %s" % ("tmux" if tmux else "herdr", pane)
    if cmd[0] == "on" and not watchers(pane):
        agy = agy_ancestor()
        if agy is None:
            sys.exit("autoapprove: run it inside agy as `! autoapprove on` so the watcher stops with agy")
        start(pane, tmux, agy)
        time.sleep(0.3)
    elif cmd[0] == "off":
        for pid in watchers(pane):
            os.kill(pid, signal.SIGTERM)
        time.sleep(0.3)
    running = watchers(pane)
    print("autoapprove: %s for %s%s" % (
        "on" if running else "off", where, " (pid %s)" % ", ".join(map(str, running)) if running else ""))
    return 0


def session_start():
    """SessionStart hook: start a watcher bound to this agy session when autoStart is on."""
    try:
        pane, tmux = this_pane()
        agy = agy_ancestor()  # unbound watchers could wander to another agy pane
        if read_auto_start() and pane and agy and not watchers(pane):
            start(pane, tmux, agy)
    except Exception as exc:  # never block session start
        sys.stderr.write("autoapprove hook error: %s\n" % exc)
    print("{}")
    return 0


def parse_args(argv=None):
    parser = argparse.ArgumentParser(
        prog="autoapprove",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description="Auto-approve agy approval dialogs in a herdr or tmux pane.",
        epilog="""commands:
  status (default)   is a watcher running for this pane?
  on / off           start / stop the watcher for this pane; inside agy: `! autoapprove on`
  default [on|off]   start a watcher in every new agy session (SessionStart hook)
  watch [PANE]       watch PANE in the foreground (default: $AGY_PANE or w7:p4N)

policy:
  yes/no questions            answer "y" (herdr only; tmux reports no agent state)
  option menus (questions)    pick "Yes, and always allow in this conversation"
                              when offered, else the "(Recommended)" label, else
                              the highlighted default
  "Persist to settings.json"  skipped (one-shot default is used instead)
  subagent approval alerts    ctrl+k, whatever the main agent's state
  slash-command palettes and other non-question menus are never touched

environment fallbacks: AGY_PANE, AGY_AUTOAPPROVE_LOG, AGY_POLL_SECS""",
    )
    parser.add_argument("--session-start", action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("command", nargs="*", default=["status"])
    parser.add_argument("--tmux", action="store_true", help="PANE is a tmux pane (watch)")
    parser.add_argument("--until-pid", type=int, help="exit when this process ends (watch)")
    parser.add_argument("--agent", default="agy", help="agent name for herdr rediscovery (default: %(default)s)")
    parser.add_argument("--log", default=LOG, help="log file shared by all instances (default: %(default)s)")
    parser.add_argument("--poll", type=float, default=POLL_SECS, help="seconds between polls (default: %(default)s)")
    return parser.parse_args(argv)


def main(argv=None):
    global PANE, LOG, POLL_SECS, AGENT, TMUX, UNTIL_PID
    args = parse_args(argv)
    LOG, POLL_SECS, AGENT, TMUX, UNTIL_PID = args.log, args.poll, args.agent, args.tmux, args.until_pid
    if args.session_start:
        return session_start()
    cmd = args.command
    if cmd[0] == "watch" and len(cmd) <= 2:
        PANE = cmd[1] if len(cmd) == 2 else PANE
        return watch()
    if cmd[0] in ("status", "on", "off") and len(cmd) == 1 or cmd[0] == "default":
        return control(cmd)
    sys.exit("usage: autoapprove [status|on|off|default [on|off]|watch [PANE]]")


if __name__ == "__main__":
    sys.exit(main())
