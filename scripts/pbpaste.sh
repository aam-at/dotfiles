#!/usr/bin/env sh

# Read the clipboard from the best available helper.
if [ "${XDG_SESSION_TYPE:-}" = "wayland" ] && command -v wl-paste >/dev/null 2>&1; then
  exec wl-paste -n "$@"
elif command -v xclip >/dev/null 2>&1; then
  exec xclip -selection clipboard -o "$@"
elif command -v xsel >/dev/null 2>&1; then
  exec xsel --clipboard --output "$@"
elif command -v powershell.exe >/dev/null 2>&1; then
  exec powershell.exe Get-Clipboard "$@"
else
  printf '%s\n' "pbpaste.sh: no clipboard helper found" >&2
  exit 1
fi
