#!/bin/sh
# This will wait one second and then steal focus and make the Zenity dialog box always-on-top (aka. 'above').

TITLE="What am I doing?"

(sleep 1 && wmctrl -F -a "$TITLE" -b add,above) &
(zenity --entry --title="$TITLE" --text="Record the task I am working on" --entry-text="Lazying..." --width=400 --height=200)
