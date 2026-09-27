#!/usr/bin/env sh
# Has aw-server serve the wellbeing dashboard: adds
#   wellbeing = '<dashboard dir>'
# under [server.custom_static] in aw-server.toml. aw-server reads it on
# start, so restart ActivityWatch after a change.
#
# Usage: serve-dashboard.sh <aw-server.toml> <dashboard dir>
set -eu

config=$1
entry="wellbeing = '$2'"

mkdir -p "$(dirname "$config")"
[ -f "$config" ] || printf '[server]\n\n[server.custom_static]\n' >"$config"
grep -qxF "$entry" "$config" && exit 0

awk -v entry="$entry" '
    /^wellbeing[ \t]*=/ { next }
    { print }
    /^\[server\.custom_static\][ \t]*$/ { print entry; added = 1 }
    END { if (!added) { print ""; print "[server.custom_static]"; print entry } }
' "$config" >"$config.tmp" && mv "$config.tmp" "$config"
echo "aw-server serves the wellbeing dashboard: restart ActivityWatch to load it."
