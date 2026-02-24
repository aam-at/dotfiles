#!/bin/bash
# Toggle a named workspace: focus it, or go back if already on it.
# Mimics Hyprland's togglespecialworkspace behavior.
#
# Usage: toggle-named-workspace.sh <workspace-name>

TARGET="$1"

if [ -z "$TARGET" ]; then
  echo "Usage: toggle-named-workspace.sh <workspace-name>" >&2
  exit 1
fi

CURRENT=$(niri msg -j workspaces | jq -r '.[] | select(.is_focused) | .name // empty')

if [ "$CURRENT" = "$TARGET" ]; then
  niri msg action focus-workspace-previous
else
  niri msg action focus-workspace "$TARGET"
fi
