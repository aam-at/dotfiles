#!/bin/bash
# Jump to the scratch terminal if one is running, else spawn one.
# No hide/show, no parking workspace — just quick access to one floating
# terminal from anywhere. Bound to Mod+Grave in dms/binds.kdl.

WIN_ID=$(niri msg -j windows | jq -r '.[] | select(.app_id=="scratch-terminal") | .id' | head -1)

if [ -n "$WIN_ID" ]; then
  exec niri msg action focus-window --id "$WIN_ID"
else
  TERM_BIN="${TERMINAL:-kitty}"
  case "$TERM_BIN" in
  kitty) exec kitty --class scratch-terminal ;;
  wezterm) exec wezterm start --class scratch-terminal ;;
  ghostty) exec ghostty --class=scratch-terminal ;;
  alacritty) exec alacritty --class scratch-terminal,scratch-terminal ;;
  *) exec "$TERM_BIN" ;;
  esac
fi
