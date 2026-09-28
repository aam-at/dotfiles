#!/usr/bin/env bash
# Starts ActivityWatch: aw-server and the Wayland window/idle watcher
# (awatcher), as systemd user units (config/activitywatch; installed by
# setup/install_activitywatch.sh). They also start at login.

systemctl --user start aw-server.service aw-awatcher.service &&
  notify-send "ActivityWatch started"
