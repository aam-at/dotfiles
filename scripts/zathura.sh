#!/usr/bin/env bash

# The file to open
FILE=$1

# Escape special characters in filename
ESCAPED_FILE=$(printf '%q' "$FILE")

# Check if Zathura is already running with the file open
EXISTING_WINDOW=$(xdotool search --name "$ESCAPED_FILE")

if [ -z "$EXISTING_WINDOW" ]; then
    # If the window does not exist, open the file with Zathura
    zathura "$FILE" &
else
    # If the window exists, focus it
    xdotool windowactivate "$EXISTING_WINDOW"
fi

