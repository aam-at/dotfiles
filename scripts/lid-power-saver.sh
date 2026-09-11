#!/usr/bin/env bash
# logind ignores the lid switch here so long-running work survives a closed lid.
# Select power-saver once each time the laptop becomes "lid closed and on
# battery"; otherwise never touch the user's power profile.

set -euo pipefail

lids=(/proc/acpi/button/lid/*/state)
readonly LID_STATE_PATH="${LID_STATE_PATH:-${lids[0]}}"
readonly POWER_SUPPLY_ROOT="${POWER_SUPPLY_ROOT:-/sys/class/power_supply}"
readonly POLL_INTERVAL_SECONDS="${POLL_INTERVAL_SECONDS:-2}"

closed_on_battery() {
  local _ state
  read -r _ state <"$LID_STATE_PATH"
  # Battery status, not AC type: USB-C chargers report type USB, not Mains.
  # ponytail: BAT* glob skips HID peripheral batteries; widen if a laptop names it otherwise (e.g. CMB0).
  [[ $state == "closed" ]] && grep -qx Discharging "$POWER_SUPPLY_ROOT"/BAT*/status 2>/dev/null
}

# Needed: closed_on_battery runs in an && list, where set -e can't catch a failed read.
[[ -r $LID_STATE_PATH ]] || {
  echo "Cannot read lid state at $LID_STATE_PATH" >&2
  exit 1
}

was=0
while true; do
  now=0
  closed_on_battery && now=1
  if ((now && !was)); then
    # && keeps a daemon hiccup from killing the loop (Restart=always would spin every 2s).
    powerprofilesctl set power-saver &&
      logger --tag lid-power-saver "lid closed on battery; selected power-saver profile"
  fi
  was=$now
  sleep "$POLL_INTERVAL_SECONDS"
done
