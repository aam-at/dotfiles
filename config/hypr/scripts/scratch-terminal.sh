#!/usr/bin/env bash
# Focus the existing scratch terminal or launch one.  This avoids a dependency
# on a drop-down-terminal helper and works with both DMS and Noctalia.
set -euo pipefail

scratch_class="scratch-terminal"
address="$(hyprctl clients -j | jq -r --arg class "$scratch_class" \
  '.[] | select(.class == $class) | .address' | head -n1)"

if [[ -n "$address" && "$address" != "null" ]]; then
  exec hyprctl dispatch focuswindow "address:$address"
fi

terminal="${TERMINAL:-kitty}"
case "$terminal" in
  kitty) exec kitty --class "$scratch_class" --title "$scratch_class" ;;
  wezterm) exec wezterm start --class "$scratch_class" ;;
  ghostty) exec ghostty --class="$scratch_class" ;;
  alacritty) exec alacritty --class "$scratch_class","$scratch_class" ;;
  *) exec "$terminal" ;;
esac
