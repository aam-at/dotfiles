#!/usr/bin/env sh

# Read the clipboard from the best available helper.
# Gate on the actual display socket vars, not $XDG_SESSION_TYPE: that label
# often doesn't survive into tmux/detached/ssh-reattached shells even though
# the Wayland/X socket is still live, which would otherwise skip a working
# wl-paste/xclip and fall through to "no clipboard helper found".
if [ -n "${WAYLAND_DISPLAY:-}" ] && command -v wl-paste >/dev/null 2>&1; then
  exec wl-paste -n "$@"
elif [ -n "${DISPLAY:-}" ] && command -v xclip >/dev/null 2>&1; then
  exec xclip -selection clipboard -o "$@"
elif [ -n "${DISPLAY:-}" ] && command -v xsel >/dev/null 2>&1; then
  exec xsel --clipboard --output "$@"
elif command -v powershell.exe >/dev/null 2>&1; then
  exec powershell.exe Get-Clipboard "$@"
else
  printf '%s\n' "pbpaste.sh: no clipboard helper found" >&2
  exit 1
fi
