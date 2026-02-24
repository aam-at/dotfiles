#!/bin/bash
# Toggle focus to a named workspace.
# Press once to go there, press again to go back.
#
# Usage: toggle-named-workspace.sh <workspace-name>

TARGET="$1"
[ -z "$TARGET" ] && exit 1

CURRENT=$(niri msg -j workspaces | jq -r '.[] | select(.is_focused) | .name // empty')

if [ "$CURRENT" = "$TARGET" ]; then
  niri msg action focus-workspace-previous
else
  niri msg action focus-workspace "$TARGET"
fi
