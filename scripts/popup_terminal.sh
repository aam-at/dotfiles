#!/usr/bin/env bash

# Check if environment variables are set
if [ -z "$XDG_SESSION_TYPE" ]; then
  echo "Error: XDG_SESSION_TYPE is not set"
  exit 1
fi

if [ "$XDG_SESSION_TYPE" = "x11" ]; then
  popup_terminal_x11.sh
elif [ "$XDG_SESSION_TYPE" = "wayland" ]; then
  if [ -z "$XDG_SESSION_DESKTOP" ]; then
    echo "Error: XDG_SESSION_DESKTOP is not set"
    exit 1
  fi

  if [ "$XDG_SESSION_DESKTOP" = "Hyprland" ]; then
    popup_terminal_hyperland.sh
  else
    echo "Unsupported Wayland session type: $XDG_SESSION_DESKTOP"
    exit 1
  fi
else
  echo "Unsupported session type: $XDG_SESSION_TYPE"
  exit 1
fi
