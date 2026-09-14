#!/usr/bin/env bash
# Toggle global push-to-talk-by-toggle dictation: first press starts recording
# from the default mic and remembers the destination terminal pane; second
# press stops recording and transcribes in the background.  The transcript is
# delivered to that remembered HerdR or tmux pane, so focus may move while
# Whisper is working.  Ordinary applications retain the focused-window
# ydotool fallback.

set -euo pipefail

STATE_DIR="${XDG_RUNTIME_DIR:-/tmp}/dictation"
PID_FILE="$STATE_DIR/pid"
WAV_FILE="$STATE_DIR/recording.wav"
LOG_FILE="$STATE_DIR/whisper.log"
LOCK_FILE="$STATE_DIR/lock"
TARGET_FILE="$STATE_DIR/target"
TRANSCRIBE_PID_FILE="$STATE_DIR/transcribe.pid"
FAILED_TRANSCRIPT_FILE="$STATE_DIR/last-failed-transcript.txt"
TRANSCRIPT_BASE="$STATE_DIR/transcript"
TRANSCRIPT_FILE="${TRANSCRIPT_BASE}.txt"
MODEL="${DICTATION_MODEL:-$HOME/.local/share/whisper-cpp/ggml-large-v3-turbo-q8_0.bin}"
THREADS="${DICTATION_THREADS:-8}"
MODEL_NAME="$(basename "$MODEL")"
MODEL_URL="${DICTATION_MODEL_URL:-https://huggingface.co/ggerganov/whisper.cpp/resolve/main/$MODEL_NAME?download=true}"

umask 077
mkdir -p "$STATE_DIR"

# Prefer whichever build setup/install_whisper_cpp_gpu.sh made for this
# machine's hardware. A GPU build still works fine with no GPU present
# (ggml falls back to its own CPU backend at runtime), so no separate
# CPU-retry logic is needed here — just pick the best binary available.
WHISPER_BIN="${DICTATION_WHISPER_BIN:-}"
if [[ -z "$WHISPER_BIN" ]]; then
  for candidate in whisper-cli-cuda whisper-cli-sycl whisper-cli-vulkan whisper-cli-cpu whisper-cli; do
    if command -v "$candidate" >/dev/null 2>&1; then
      WHISPER_BIN="$candidate"
      break
    fi
  done
fi

# Serialize toggle invocations: a key-repeat or double-press firing this
# script again while a previous run is still starting/stopping/transcribing
# just no-ops instead of racing on $PID_FILE/$WAV_FILE.
exec 9>"$LOCK_FILE"
flock -n 9 || exit 0

notify() {
  notify-send -h string:x-canonical-private-synchronous:dictation "Dictation" "$1" || true
}

ensure_model() {
  [[ -s "$MODEL" ]] && return 0

  if ! command -v curl >/dev/null 2>&1; then
    notify "Model missing and curl is unavailable"
    echo "curl is required to download $MODEL" >>"$LOG_FILE"
    return 1
  fi

  mkdir -p "$(dirname "$MODEL")"
  local tmp
  tmp="$(mktemp "${MODEL}.tmp.XXXXXX")"
  notify "Downloading Whisper model: $MODEL_NAME"
  if curl --fail --location --retry 3 --retry-delay 2 --progress-bar \
    "$MODEL_URL" -o "$tmp" 2>>"$LOG_FILE" && [[ -s "$tmp" ]]; then
    mv -f "$tmp" "$MODEL"
    notify "Whisper model downloaded: $MODEL_NAME"
    return 0
  fi

  rm -f "$tmp"
  notify "Model download failed (see $LOG_FILE)"
  return 1
}

is_recording() {
  [[ -f "$PID_FILE" ]] || return 1
  local pid
  pid="$(cat "$PID_FILE")"
  [[ -n "$pid" ]] && [[ "$(cat "/proc/$pid/comm" 2>/dev/null)" == "pw-record" ]]
}

is_transcribing() {
  [[ -f "$TRANSCRIBE_PID_FILE" ]] || return 1

  local pid
  pid="$(cat "$TRANSCRIBE_PID_FILE")"
  if [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null; then
    return 0
  fi

  rm -f "$TRANSCRIBE_PID_FILE"
  return 1
}

capture_herdr_target() {
  command -v herdr >/dev/null 2>&1 || return 1

  local panes pane_id
  panes="$(herdr pane list 2>>"$LOG_FILE")" || return 1

  if command -v jq >/dev/null 2>&1; then
    pane_id="$(printf '%s' "$panes" | jq -er \
      '[.result.panes[] | select(.focused == true) | .pane_id] | if length == 1 then .[0] else empty end' \
      2>>"$LOG_FILE")" || return 1
  else
    # HerdR emits compact JSON; retain a dependency-free fallback for systems
    # where jq is intentionally absent.
    pane_id="$(printf '%s\n' "$panes" | sed -nE \
      's/.*"pane_id":"([^"]+)"[^}]*"focused":true.*/\1/p')"
    [[ "$(printf '%s\n' "$pane_id" | sed '/^$/d' | wc -l)" -eq 1 ]] || return 1
  fi

  [[ -n "$pane_id" ]] || return 1
  printf 'herdr\t%s\n' "$pane_id"
}

capture_target() {
  # A tmux binding can pass its pane explicitly, even though this script is
  # launched outside that pane's environment.
  if [[ -n "${DICTATION_TMUX_PANE:-}" ]]; then
    printf 'tmux\t%s\n' "$DICTATION_TMUX_PANE"
  elif [[ -n "${TMUX_PANE:-}" ]]; then
    printf 'tmux\t%s\n' "$TMUX_PANE"
  elif capture_herdr_target; then
    :
  else
    printf 'focus\t\n'
  fi
}

save_failed_transcript() {
  local text="$1"
  printf '%s\n' "$text" >"$FAILED_TRANSCRIPT_FILE"
}

notify_delivery_complete() {
  local target_kind="$1"
  local target_id="$2"
  local text="$3"

  case "$target_kind" in
  herdr)
    notify "Transcript ready in HerdR pane $target_id — check it and press Enter"
    ;;
  tmux)
    notify "Transcript ready in tmux pane $target_id — check it and press Enter"
    ;;
  *)
    notify "Typed: $text"
    ;;
  esac
}

deliver_to_target() {
  local target_kind="$1"
  local target_id="$2"
  local text="$3"

  case "$target_kind" in
  herdr)
    herdr pane send-text "$target_id" -- "$text" 2>>"$LOG_FILE"
    ;;
  tmux)
    # A named buffer avoids touching the user's default tmux paste buffer.
    local buffer_name="dictation-target"
    printf '%s' "$text" | tmux load-buffer -b "$buffer_name" - 2>>"$LOG_FILE" &&
      tmux paste-buffer -d -b "$buffer_name" -t "$target_id" 2>>"$LOG_FILE"
    ;;
  focus)
    ydotool type -- "$text" 2>>"$LOG_FILE"
    ;;
  *)
    return 1
    ;;
  esac
}

start_recording() {
  if is_transcribing; then
    notify "Still transcribing the previous recording"
    return 0
  fi

  capture_target >"$TARGET_FILE"

  # Close our lock fd in the backgrounded child before it execs pw-record —
  # otherwise pw-record inherits it and keeps the lock held for as long as
  # it keeps recording, causing the next (stop) invocation to no-op.
  pw-record --rate 16000 --channels 1 --format s16 "$WAV_FILE" 9>&- 2>"$LOG_FILE" &
  local pid=$!
  sleep 0.2
  if ! kill -0 "$pid" 2>/dev/null; then
    notify "Failed to start recording (see $LOG_FILE)"
    rm -f "$TARGET_FILE"
    wait "$pid" || true
    return 1
  fi
  echo "$pid" >"$PID_FILE"
  notify "Recording... press the hotkey again to stop"
}

transcribe_and_deliver() {
  trap 'rm -f "$TRANSCRIBE_PID_FILE" "$TARGET_FILE"' EXIT

  local target_kind target_id
  if [[ -f "$TARGET_FILE" ]]; then
    IFS=$'\t' read -r target_kind target_id <"$TARGET_FILE"
  else
    target_kind="focus"
    target_id=""
  fi

  notify "Transcribing..."

  if ! ensure_model; then
    rm -f "$WAV_FILE"
    return 1
  fi

  # Read Whisper's dedicated text artifact rather than stdout.  With -np,
  # stdout still contains the recognizer's result framing on some builds,
  # including a leading `---` separator.
  rm -f "$TRANSCRIPT_FILE"
  # ponytail: no CPU-retry here — a GPU build already falls back to ggml's
  # own CPU backend when the GPU is simply absent (verified with
  # CUDA_VISIBLE_DEVICES=""), so a second binary invocation would just
  # duplicate that. A GPU that's present but broken (bad driver, etc) still
  # fails outright; if that turns out to happen in practice, override with
  # DICTATION_WHISPER_BIN=whisper-cli-cpu rather than reintroducing a retry.
  if ! "$WHISPER_BIN" -m "$MODEL" -t "$THREADS" -l en -np -nt -otxt \
    -of "$TRANSCRIPT_BASE" -f "$WAV_FILE" >/dev/null 2>"$LOG_FILE"; then
    notify "Transcription failed (see $LOG_FILE)"
    rm -f "$WAV_FILE"
    return 1
  fi
  rm -f "$WAV_FILE"

  local text
  if [[ ! -s "$TRANSCRIPT_FILE" ]]; then
    notify "No transcript produced (see $LOG_FILE)"
    return 1
  fi
  text="$(tr '\n' ' ' <"$TRANSCRIPT_FILE" | sed -E \
    -e 's/^[^[:alnum:]]+//' \
    -e 's/^[[:space:]]+|[[:space:]]+$//g')"
  rm -f "$TRANSCRIPT_FILE"

  if [[ -z "$text" ]]; then
    notify "No speech detected"
    return
  fi

  if deliver_to_target "$target_kind" "$target_id" "$text"; then
    notify_delivery_complete "$target_kind" "$target_id" "$text"
  else
    save_failed_transcript "$text"
    notify "Target unavailable; transcript saved in $FAILED_TRANSCRIPT_FILE"
    return 1
  fi
}

stop_recording_and_transcribe() {
  local pid
  pid="$(cat "$PID_FILE")"
  rm -f "$PID_FILE"
  kill -TERM "$pid" 2>/dev/null || true
  for _ in $(seq 1 40); do
    kill -0 "$pid" 2>/dev/null || break
    sleep 0.05
  done
  if kill -0 "$pid" 2>/dev/null; then
    kill -KILL "$pid" 2>/dev/null || true
    sleep 0.2
  fi

  # Do not let the worker inherit our toggle lock: subsequent presses should
  # report that transcription is in progress instead of silently no-oping.
  transcribe_and_deliver 9>&- &
  echo "$!" >"$TRANSCRIBE_PID_FILE"
  notify "Transcribing in background..."
}

if is_recording; then
  stop_recording_and_transcribe
else
  start_recording
fi
