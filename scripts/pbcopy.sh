#!/usr/bin/env sh

# Copy stdin to the best available clipboard helper.
# Gate on the actual display socket vars, not $XDG_SESSION_TYPE: that label
# often doesn't survive into tmux/detached/ssh-reattached shells even though
# the Wayland/X socket is still live, which would otherwise skip a working
# wl-copy/xclip and fall through to "no clipboard helper found".
if [ -n "${WAYLAND_DISPLAY:-}" ] && command -v wl-copy >/dev/null 2>&1; then
  exec wl-copy "$@"
elif [ -n "${DISPLAY:-}" ] && command -v xclip >/dev/null 2>&1; then
  exec xclip -selection clipboard "$@"
elif [ -n "${DISPLAY:-}" ] && command -v xsel >/dev/null 2>&1; then
  exec xsel --clipboard --input "$@"
elif command -v clip.exe >/dev/null 2>&1; then
  exec clip.exe "$@"
else
  printf '%s\n' "pbcopy.sh: no clipboard helper found" >&2
  exit 1
fi
