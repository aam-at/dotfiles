#!/usr/bin/env python3
"""Configurable context-compaction threshold gate for Antigravity (agy).

Runs as a `PreInvocation` plugin hook (`--hook`, hook JSON on stdin). The same
command words work on the command line, linked as `compaction` (in the agy CLI:
`! compaction 750k`, since `!` is its shell escape), and as a chat message
`!compaction 750k` where the client passes `!` through to the model; see `USAGE`.
"""

from __future__ import annotations

import argparse
import contextlib
import json
import os
import re
import sys
from pathlib import Path
from typing import Any

DEFAULTS: dict[str, Any] = {
    "enabled": True,
    "mode": "gate",
    "threshold": "500k",
    "warnThreshold": None,
}
MODES = {"gate", "warn"}
CHUNK_SIZE = 65_536
USAGE = (
    "Usage: `! compaction [status | on | off | gate | warn | mode <gate|warn> | "
    "<500k|off> | set <500k|off> | warn-at <400k|off>]`."
)

_TOKEN_RE = re.compile(
    r"^\s*([0-9]+(?:\.[0-9]+)?)\s*([km]?)\s*(?:tokens?)?\s*$", re.IGNORECASE
)
_USER_REQUEST_RE = re.compile(r"<USER_REQUEST>\s*(.*?)\s*</USER_REQUEST>", re.DOTALL)
_DISABLE_WORDS = {"0", "off", "false", "none", "disable", "disabled"}


# --- Token values ---------------------------------------------------------------


def parse_token_count(raw: float | str | None) -> int | None:
    """Parse `500k`, `0.5m`, `500_000`, or an int; `None`/`off`/`0` mean disabled (None)."""
    if raw is None:
        return None
    if isinstance(raw, bool):
        raise ValueError(f"Invalid token value {raw!r}.")
    if isinstance(raw, (int, float)):
        if raw < 0:
            raise ValueError(f"Token value must be non-negative, got {raw!r}.")
        return int(raw) or None

    cleaned = str(raw).strip().replace(",", "").replace("_", "")
    if not cleaned or cleaned.lower() in _DISABLE_WORDS:
        return None
    match = _TOKEN_RE.match(cleaned)
    if not match:
        raise ValueError(
            f"Invalid token value {raw!r}. Expected an integer or shorthand like '500k' or '1m'."
        )
    multiplier = {"k": 1_000, "m": 1_000_000}.get(match.group(2).lower(), 1)
    return round(float(match.group(1)) * multiplier) or None


def format_tokens(tokens: int | None) -> str:
    """Format a token count with both exact and compact notation."""
    if tokens is None:
        return "disabled"
    if tokens % 1_000_000 == 0:
        short = f"{tokens // 1_000_000}m"
    elif tokens % 1_000 == 0:
        short = f"{tokens // 1_000}k"
    else:
        short = f"{tokens / 1_000:.1f}k"
    return f"{tokens:,} ({short})"


def display_path(path: Path) -> str:
    """Format a config path concisely with `~` when under `$HOME`."""
    with contextlib.suppress(ValueError):
        return f"~/{path.relative_to(Path.home())}"
    return str(path)


# --- Gate config: the only code that knows the file format and its defaults -------


def config_path(
    workspace_paths: list[str] | None = None, cwd: str | None = None
) -> Path:
    """Resolve `compaction.json`: `$AGY_COMPACTION_CONFIG` -> workspace `.agents/` -> plugin."""
    env_path = os.environ.get("AGY_COMPACTION_CONFIG")
    if env_path:
        return Path(env_path).expanduser().resolve()
    for root in [*(workspace_paths or []), cwd]:
        if root:
            candidate = Path(root) / ".agents" / "compaction.json"
            if candidate.exists():
                return candidate
    return Path(__file__).resolve().parent.parent / "compaction.json"


def _read_raw(path: Path) -> dict[str, Any]:
    if not path.exists():
        return {}
    data = json.loads(path.read_text(encoding="utf-8-sig"))
    if not isinstance(data, dict):
        raise ValueError("top-level JSON value must be an object")
    return data


def _normalize(raw: dict[str, Any]) -> dict[str, Any]:
    """Validate raw settings into `{enabled, mode, threshold, warnThreshold}`; raise ValueError."""
    merged = {**DEFAULTS, **raw}
    if not isinstance(merged["enabled"], bool):
        raise ValueError(f"'enabled' must be true or false, got {merged['enabled']!r}")
    mode = str(merged["mode"] or DEFAULTS["mode"]).strip().lower()
    if mode not in MODES:
        raise ValueError(f"mode must be 'gate' or 'warn', got {merged['mode']!r}")
    threshold = parse_token_count(merged["threshold"])
    return {
        "enabled": merged["enabled"] and threshold is not None,
        "mode": mode,
        "threshold": threshold,
        "warnThreshold": parse_token_count(merged["warnThreshold"]),
    }


def read_config(path: Path) -> tuple[dict[str, Any], str | None]:
    """Return `(config, warning)`. An invalid file falls back to defaults entirely; env overrides apply last."""
    warning = None
    try:
        cfg = _normalize(_read_raw(path))
    except (OSError, ValueError) as exc:  # JSONDecodeError is a ValueError
        cfg = _normalize({})
        warning = f"Invalid {path.name} ({exc}); using defaults ({format_tokens(cfg['threshold'])} gate)."

    env_threshold = os.environ.get("AGY_COMPACTION_THRESHOLD", "").strip()
    if env_threshold:
        try:
            # Overrides the threshold only; a gate disabled in the file stays disabled.
            cfg["threshold"] = parse_token_count(env_threshold)
            cfg["enabled"] = cfg["enabled"] and cfg["threshold"] is not None
        except ValueError as exc:
            warning = f"Invalid AGY_COMPACTION_THRESHOLD ({exc}); ignoring override."

    env_mode = os.environ.get("AGY_COMPACTION_MODE", "").strip().lower()
    if env_mode in MODES:
        cfg["mode"] = env_mode
    elif env_mode:
        warning = f"Invalid AGY_COMPACTION_MODE {env_mode!r}; ignoring override."
    return cfg, warning


def update_config(path: Path, **change: Any) -> None:
    """Merge `change` into the file, validate, then write. Refuses to overwrite an unreadable file."""
    try:
        raw = _read_raw(path)
    except (OSError, ValueError) as exc:
        raise ValueError(
            f"Cannot update {display_path(path)} ({exc}); fix or delete it first."
        ) from exc
    updated = {**DEFAULTS, **raw, **change}
    _normalize(updated)  # raises before anything is written
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(updated, indent=2) + "\n", encoding="utf-8")


# --- Transcript ----------------------------------------------------------------


def iter_jsonl_reverse(path: Path):
    """Yield parsed JSON objects from the end of a JSONL file without loading it all."""
    with path.open("rb") as fh:
        fh.seek(0, os.SEEK_END)
        pos = fh.tell()
        carry = b""
        while pos > 0:
            read_size = min(CHUNK_SIZE, pos)
            pos -= read_size
            fh.seek(pos)
            data = fh.read(read_size) + carry
            lines = data.split(b"\n")
            carry = lines[0]
            for raw_line in reversed(lines[1:]):
                raw_line = raw_line.strip()
                if not raw_line:
                    continue
                try:
                    yield json.loads(raw_line.decode("utf-8", errors="replace"))
                except json.JSONDecodeError:
                    continue
        if carry.strip():
            with contextlib.suppress(json.JSONDecodeError):
                yield json.loads(carry.strip().decode("utf-8", errors="replace"))


def read_active_context_tokens(transcript_path: Path) -> tuple[int, str | None]:
    """Return `(active_tokens, last_step_type)` since the most recent compaction checkpoint."""
    if not transcript_path.exists():
        return 0, None

    last_step_type: str | None = None
    for entry in iter_jsonl_reverse(transcript_path):
        if not isinstance(entry, dict):
            continue
        step_type = entry.get("type")
        if last_step_type is None and isinstance(step_type, str):
            last_step_type = step_type

        if step_type == "CHECKPOINT":
            # Conversation was just compacted and no post-compaction model step has run yet.
            return int(entry.get("output_tokens") or 0), last_step_type

        if step_type == "PLANNER_RESPONSE":
            total = sum(
                int(entry.get(k) or 0)
                for k in ("input_tokens", "cache_read_tokens", "output_tokens")
            )
            if total > 0:
                return total, last_step_type

    return 0, last_step_type


# --- Commands: shared by `!compaction` in chat and the command line ---------------

_NO_ARG = {"status", "on", "enable", "enabled", "off", "disable", "disabled", *MODES}
_ONE_ARG = {"mode", "set", "warn-at", "warn_at"}


def run_command(words: list[str], path: Path, active_tokens: int | None = None) -> str:
    """Apply one command (see `USAGE`) and return a one-line status report. Raises ValueError."""
    words = words or ["status"]
    if len(words) == 1 and words[0].lower() not in _NO_ARG | _ONE_ARG:
        words = ["set", words[0]]  # bare `!compaction 750k`
    sub, args = words[0].lower(), words[1:]
    if not (sub in _NO_ARG and not args or sub in _ONE_ARG and len(args) == 1):
        raise ValueError(USAGE)

    if sub in {"on", "enable", "enabled"}:
        update_config(path, enabled=True)
    elif sub in {"off", "disable", "disabled"}:
        update_config(path, enabled=False)
    elif sub in MODES:
        update_config(path, mode=sub)
    elif sub == "mode":
        update_config(path, mode=args[0])
    elif sub in {"warn-at", "warn_at"}:
        update_config(path, warnThreshold=args[0])
    elif sub == "set" and parse_token_count(args[0]) is None:
        update_config(
            path, enabled=False
        )  # keep the old threshold on file so `on` restores it
    elif sub == "set":
        update_config(path, threshold=args[0], enabled=True)

    cfg, warning = read_config(path)
    msg = (
        f"Compaction gate ({display_path(path)}): enabled={cfg['enabled']}, mode={cfg['mode']}, "
        f"threshold={format_tokens(cfg['threshold'])}, warnThreshold={format_tokens(cfg['warnThreshold'])}"
    )
    if active_tokens is not None:
        msg += f", activeContext={active_tokens:,} tokens"
    return msg + "." + (f" Warning: {warning}" if warning else "")


def parse_chat_command(user_input: str) -> list[str] | None:
    """Return the words after `!compaction`, or None if the input is not a gate command."""
    match = _USER_REQUEST_RE.search(user_input)
    words = (match.group(1) if match else user_input).split()
    return words[1:] if words and words[0].lower() == "!compaction" else None


# --- Verdict: pure threshold rules ------------------------------------------------


def verdict(
    cfg: dict[str, Any], active_tokens: int, invocation_num: int, where: str
) -> tuple[str, str] | None:
    """Return `("block", message)`, `("note", markdown)`, or None to pass. Notes only on invocation 0."""
    threshold, warn_at = cfg["threshold"], cfg["warnThreshold"]
    if not cfg["enabled"]:
        return None
    if threshold is not None and active_tokens >= threshold:
        if cfg["mode"] == "gate":
            return "block", (
                f"Compaction gate: active context is {active_tokens:,} tokens, "
                f"which exceeds the configured {format_tokens(threshold)} limit. "
                f"Run `/compact` to summarize the conversation before continuing, "
                f"or raise/disable the limit via `! compaction 750k` (`! compaction off`) or `{where}`."
            )
        if invocation_num == 0:
            return "note", (
                f"> [!WARNING]\n> **Compaction threshold reached:** Active context is "
                f"**{active_tokens:,} tokens** (threshold: **{format_tokens(threshold)}**). "
                f"Run `/compact` to free up context."
            )
    elif warn_at is not None and active_tokens >= warn_at and invocation_num == 0:
        return "note", (
            f"> [!NOTE]\n> **Approaching compaction gate:** Active context is **{active_tokens:,} tokens** "
            f"(warn: **{format_tokens(warn_at)}**, gate: **{format_tokens(threshold)}**). "
            f"Consider running `/compact` soon."
        )
    return None


# --- Antigravity hook adapter: the only code that knows the hook JSON shapes ------


def _block(msg: str) -> dict[str, Any]:
    return {
        "injectSteps": [{"errorMessage": {"userMessage": msg}}],
        "terminationBehavior": "terminate",
    }


def _note(markdown: str) -> dict[str, Any]:
    return {
        "systemMessage": {"systemMessage": "", "metadata": {"user_markdown": markdown}}
    }


def hook_response(payload: dict[str, Any]) -> dict[str, Any]:
    """Map one `PreInvocation` payload to the hook's JSON response."""
    workspace_paths, cwd = payload.get("workspacePaths"), payload.get("cwd")
    path = config_path(
        workspace_paths if isinstance(workspace_paths, list) else None,
        cwd if isinstance(cwd, str) else None,
    )
    transcript = payload.get("transcriptPath")
    active_tokens, last_step_type = (
        read_active_context_tokens(Path(transcript))
        if isinstance(transcript, str) and transcript
        else (0, None)
    )
    invocation_num = int(payload.get("invocationNum") or 0)

    user_input = payload.get("lastUserInput")
    if (
        invocation_num == 0
        and last_step_type == "USER_INPUT"
        and isinstance(user_input, str)
    ):
        words = parse_chat_command(user_input)
        if words is not None:  # handled even when gated
            try:
                return _block(run_command(words, path, active_tokens))
            except ValueError as exc:
                return _block(f"Compaction config error: {exc}")

    cfg, warning = read_config(path)
    notes = (
        [_note(f"> [!WARNING]\n> {warning}")] if warning and invocation_num == 0 else []
    )
    result = verdict(cfg, active_tokens, invocation_num, display_path(path))
    if result and result[0] == "block":
        return _block(result[1])
    if result:
        notes.append(_note(result[1]))
    return {"injectSteps": notes} if notes else {}


def run_hook() -> int:
    try:
        raw = sys.stdin.read()
        payload = json.loads(raw) if raw.strip() else {}
        response = hook_response(payload if isinstance(payload, dict) else {})
    except Exception as exc:
        # Fail open with valid empty hook JSON so an unexpected error never bricks the CLI.
        sys.stderr.write(f"compaction_gate hook error: {exc}\n")
        response = {}
    sys.stdout.write(json.dumps(response) + "\n")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Inspect or configure the Antigravity compaction gate."
    )
    parser.add_argument(
        "--hook",
        action="store_true",
        help="Run as a PreInvocation hook (JSON on stdin).",
    )
    parser.add_argument(
        "--config",
        type=Path,
        help="compaction.json path (default: workspace .agents/ or plugin).",
    )
    parser.add_argument(
        "--transcript",
        type=Path,
        help="transcript.jsonl to report active context tokens.",
    )
    parser.add_argument(
        "words",
        nargs="*",
        help="Same words as `!compaction`, e.g. status, 750k, off, warn-at 400k.",
    )
    args = parser.parse_args(argv)
    if args.hook:
        return run_hook()

    path = (
        args.config.expanduser().resolve()
        if args.config
        else config_path(cwd=os.getcwd())
    )
    tokens = read_active_context_tokens(args.transcript)[0] if args.transcript else None
    try:
        print(run_command(args.words, path, tokens))
    except ValueError as exc:
        sys.stderr.write(f"Error: {exc}\n")
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
