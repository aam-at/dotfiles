#!/usr/bin/env bash
# Stops ActivityWatch: the watcher and aw-server (config/activitywatch).

systemctl --user stop aw-awatcher.service aw-server.service
notify-send "ActivityWatch stopped"
