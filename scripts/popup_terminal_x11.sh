#!/usr/bin/env bash

# Dropdown Terminal Script using tdrop for X11

# Check if tdrop is installed
if ! command -v tdrop &>/dev/null; then
    echo "tdrop could not be found. Please install tdrop (https://github.com/noctuid/tdrop)."
    exit 1
fi

# Check if wmctrl is installed
if ! command -v wmctrl &>/dev/null; then
    echo "wmctrl could not be found. Please install wmctrl."
    exit 1
fi

# Check if kitty is installed
if ! command -v kitty &>/dev/null; then
    echo "kitty could not be found. Please install kitty."
    exit 1
fi

# Set default values for width and height
WIDTH="100%"
HEIGHT="50%"

# Parse command-line arguments for custom width and height
while getopts w:h: flag; do
    case "${flag}" in
        w) WIDTH=${OPTARG} ;;
        h) HEIGHT=${OPTARG} ;;
        *)
            echo "Usage: $0 [-w width] [-h height]"
            exit 1
            ;;
    esac
done

# Function to get the window ID of the existing dropdown terminal
get_window_id() {
    wmctrl -l -x | grep -i "kitty.Kitty" | awk '{print $1}'
}

# Check if the dropdown terminal is already open
WINDOW_ID=$(get_window_id)

if [ -z "$WINDOW_ID" ]; then
    # If not open, launch the dropdown terminal
    tdrop -P 'wmctrl -i -r $wid -b add,sticky,above' -am -w "$WIDTH" -h "$HEIGHT" -y 0 kitty
    if [ $? -ne 0 ]; then
        echo "Failed to launch dropdown terminal using tdrop."
        exit 1
    fi
    echo "Dropdown terminal launched successfully with width $WIDTH and height $HEIGHT."
else
    # If open, toggle its visibility
    IS_HIDDEN=$(xprop -id "$WINDOW_ID" _NET_WM_STATE | grep '_NET_WM_STATE_HIDDEN')

    if [ -z "$IS_HIDDEN" ]; then
        # If the window is not hidden, hide it
        xdotool windowunmap "$WINDOW_ID"
        echo "Dropdown terminal hidden."
    else
        # If the window is hidden, show it
        xdotool windowmap "$WINDOW_ID"
        echo "Dropdown terminal shown."
    fi
fi
