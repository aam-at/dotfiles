#!/usr/bin/env bash

set -euo pipefail

choice="$(
  zenity --list \
    --title="Session" \
    --text="Choose a session action" \
    --column="Action" \
    --width=360 \
    --height=360 \
    "Lock" \
    "Suspend" \
    "Log out" \
    "Restart" \
    "Shut down"
)" || exit 0

case "$choice" in
  Lock)
    loginctl lock-session
    ;;
  Suspend)
    systemctl suspend
    ;;
  "Log out")
    cosmic-osd log-out
    ;;
  Restart)
    cosmic-osd restart
    ;;
  "Shut down")
    cosmic-osd shutdown
    ;;
esac
