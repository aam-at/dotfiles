#!/usr/bin/env bash

set -euo pipefail

mapfile -t entries < <(cliphist list)

if ((${#entries[@]} == 0)); then
  zenity --info --title="Clipboard" --text="Clipboard history is empty."
  exit 0
fi

selection="$(
  zenity --list \
    --title="Clipboard" \
    --text="Choose an item to copy" \
    --column="History" \
    --width=720 \
    --height=520 \
    "${entries[@]}"
)" || exit 0

[[ -n "$selection" ]] || exit 0
printf '%s\n' "$selection" | cliphist decode | wl-copy
