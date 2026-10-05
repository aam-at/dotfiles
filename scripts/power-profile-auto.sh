#!/usr/bin/env bash
# Prefer performance on external power and power-saver while discharging.
# power-profiles-daemon owns the actual platform tuning; this only picks its
# existing profiles as the machine's power source changes.

set -euo pipefail

readonly POWER_SUPPLY_ROOT="${POWER_SUPPLY_ROOT:-/sys/class/power_supply}"
readonly POLL_INTERVAL_SECONDS="${POLL_INTERVAL_SECONDS:-30}"

on_battery() {
  local status
  for status in "$POWER_SUPPLY_ROOT"/BAT*/status; do
    [[ -r "$status" ]] || continue
    grep -qx Discharging "$status" && return 0
  done
  return 1
}

select_profile() {
  local requested="$1" selected="$1"

  if ! powerprofilesctl set "$selected"; then
    selected=balanced
    powerprofilesctl set "$selected" || return 1
  fi
  logger --tag power-profile-auto "selected ${selected} (${requested} requested)"
}

last=""
while true; do
  wanted=performance
  on_battery && wanted=power-saver
  if [[ "$wanted" != "$last" ]] && select_profile "$wanted"; then
    last="$wanted"
  fi
  sleep "$POLL_INTERVAL_SECONDS"
done
