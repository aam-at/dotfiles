#!/usr/bin/env sh

# Copy stdin to the best available clipboard helper.
if [ "${XDG_SESSION_TYPE:-}" = "wayland" ] && command -v wl-copy >/dev/null 2>&1; then
  exec wl-copy "$@"
elif command -v xclip >/dev/null 2>&1; then
  exec xclip -selection clipboard "$@"
elif command -v xsel >/dev/null 2>&1; then
  exec xsel --clipboard --input "$@"
elif command -v clip.exe >/dev/null 2>&1; then
  exec clip.exe "$@"
else
  printf '%s\n' "pbcopy.sh: no clipboard helper found" >&2
  exit 1
fi
