"""Run: python3 -m unittest discover -s config/agents/antigravity/plugins/autoapprove/hooks"""

import os
import shutil
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path

import autoapprove as aa

HERE = Path(__file__).resolve().parent
DIALOG = """\
Run command?
❯ 1. Yes
  2. Yes, and always allow in this conversation for commands that start with 'ls'
  3. No
↑/↓ Navigate · Enter Select
"""

SUBAGENT_ALERT = """\
  Spawned subagent Shell Command Runner (self) to execute date > stamp.txt.
 ┃ self needs approval for Bash
 ┃ ─────────────────────────────
 ┃
 ┃ ● Bash(date > stamp.txt)
 ┃
 ┃ ctrl+k approve · alt+j manage
──────────────────────────────────
>
──────────────────────────────────
  ● Agent(self: Shell Command Runner) · Gemini 4 Carbon  Blocked · Bash · 14s
"""

# Option text quoting a multi-line command, blank lines included.
MULTILINE_DIALOG = """\
Requesting permission for:
   python3 -c '
   import shutil

   ⋯ (35 lines hidden)

Run this command?
> 1. Yes, run command
  2. Yes, and always allow in this conversation for commands that start with 'python3 -c '
import shutil

files = [
    "...'
  3. Yes, and always allow for commands that start with 'python3 -c '
import shutil

files = [
    "...' (Persist to settings.json)
  4. No, cancel

  ↑/↓ Navigate · tab Amend · ctrl+g edit/expand command
"""


class DetectionTest(unittest.TestCase):
    def test_menu_quoting_a_multiline_command_is_answered(self):
        self.assertTrue(aa.dialog_pending(MULTILINE_DIALOG, "blocked"))
        region, _ = aa.dialog_region(MULTILINE_DIALOG)
        self.assertEqual(aa.choose_option(aa.parse_numbered_options(region)), "2")

    def test_subagent_alert_is_pending_but_not_a_main_dialog(self):
        self.assertTrue(aa.subagent_pending(SUBAGENT_ALERT))
        self.assertFalse(aa.dialog_pending(SUBAGENT_ALERT, "working"))
        self.assertFalse(aa.subagent_pending(DIALOG))

    def test_subagent_question_and_transcript_mentions_are_ignored(self):
        self.assertFalse(aa.subagent_pending(SUBAGENT_ALERT.replace("needs approval for Bash", "has a question")))
        self.assertFalse(aa.subagent_pending("self needs approval for Bash\nctrl+k approve · alt+j manage\n"))


class ControlTest(unittest.TestCase):
    def test_watchers_finds_old_and_new_scripts_by_pane(self):
        procs = [
            subprocess.Popen([sys.executable, "-c", "import time; time.sleep(30)", script, "w0:p0"])
            for script in ("/x/agy-autoapprove.py", "/x/autoapprove.py")
        ]
        try:
            time.sleep(0.2)
            self.assertEqual(sorted(aa.watchers("w0:p0")), sorted(p.pid for p in procs))
            self.assertEqual(aa.watchers("w0:p1"), [])
        finally:
            for p in procs:
                p.kill()
                p.wait()

    def test_agy_ancestor_finds_the_agy_process(self):
        with tempfile.TemporaryDirectory() as d:
            agy = Path(d) / "agy"
            agy.write_text(
                f'#!/bin/sh\necho $$\nPYTHONPATH={HERE} {sys.executable} -c '
                '"import autoapprove; print(autoapprove.agy_ancestor())"\n'
            )
            agy.chmod(0o755)
            shell_pid, found = subprocess.run([str(agy)], capture_output=True, text=True).stdout.split()
        self.assertEqual(shell_pid, found)


@unittest.skipUnless(shutil.which("tmux"), "tmux not installed")
class TmuxWatcherTest(unittest.TestCase):
    env = None

    def tmux(self, *args):
        return subprocess.run(["tmux", *args], env=self.env, capture_output=True, text=True)

    def test_answers_menu_once_and_exits_with_agy(self):
        with tempfile.TemporaryDirectory() as d:
            env = self.env = {**os.environ, "TMUX_TMPDIR": d}
            env.pop("TMUX", None)
            screen = Path(d) / "dialog.txt"
            screen.write_text(DIALOG)
            # Fake agy: show the dialog, read one key, then clear it after a slow redraw.
            fake = f"cat {screen}; read -rsn1 k; sleep 2; clear; echo GOT:$k; sleep 30"
            self.tmux("new-session", "-d", "-s", "t", "-x", "120", "-y", "20", "bash", "-c", fake)
            watcher = agy = None
            try:
                pane = self.tmux("display-message", "-p", "-t", "t", "#{pane_id}").stdout.strip()
                agy = subprocess.Popen(["sleep", "30"])
                log = Path(d) / "log"
                watcher = subprocess.Popen(
                    [sys.executable, str(HERE / "autoapprove.py"), "watch", pane, "--tmux",
                     "--until-pid", str(agy.pid), "--log", str(log), "--poll", "0.2"],
                    env=env, stdout=subprocess.DEVNULL,
                )
                deadline = time.time() + 10
                while "GOT:" not in self.tmux("capture-pane", "-p", "-t", pane).stdout and time.time() < deadline:
                    time.sleep(0.2)
                time.sleep(1)  # a second answer would land here
                self.assertIn("GOT:2", self.tmux("capture-pane", "-p", "-t", pane).stdout)
                self.assertEqual(log.read_text().count("send-keys"), 1)

                agy.kill()
                agy.wait()  # reaped, as agy's parent shell would
                self.assertEqual(watcher.wait(timeout=5), 0)
            finally:
                for proc in (watcher, agy):
                    if proc and proc.poll() is None:
                        proc.kill()
                        proc.wait()
                self.tmux("kill-server")


if __name__ == "__main__":
    unittest.main()
