#!/usr/bin/env bash

set -euo pipefail

use_group=0
if [[ $# -gt 0 && ${1-} == "-g" ]]; then
  use_group=1
  shift
fi

if [[ $# -ne 2 ]]; then
  echo 'Wrong number of arguments. Usage: ./wsaction.fish [-g] <dispatcher> <workspace>' >&2
  exit 1
fi

dispatcher=$1
requested_ws=$2
active_ws=$(hyprctl activeworkspace -j | jq -r '.id')

if [[ -z $active_ws ]]; then
  exit 0
fi

if ((use_group)); then
  target_ws=$(((requested_ws - 1) * 10 + active_ws % 10))
else
  target_ws=$((((active_ws - 1) / 10) * 10 + requested_ws))
fi

state_dir="${HOME}/.cache/caelestia"
state_file="${state_dir}/hypr-last-workspace"
last_ws=""
stored_current_ws=""
state_lines=()

if [[ -r $state_file ]]; then
  mapfile -t -n 2 state_lines <"$state_file" || true
  if [[ ${state_lines[0]+set} ]]; then
    last_ws=${state_lines[0]}
  fi
  if [[ ${state_lines[1]+set} ]]; then
    stored_current_ws=${state_lines[1]}
  fi
fi

if [[ -n $stored_current_ws ]]; then
  if [[ $stored_current_ws != "$active_ws" ]]; then
    last_ws=$stored_current_ws
    stored_current_ws=$active_ws
  fi
else
  stored_current_ws=$active_ws
fi

if [[ -n ${target_ws-} ]] && ((target_ws == active_ws)) && [[ -n $last_ws ]] && [[ $last_ws != "$active_ws" ]]; then
  # Pressing the current workspace again returns to the previously active workspace.
  target_ws=$last_ws
fi

if [[ -z ${target_ws-} ]] || ((target_ws == active_ws)); then
  exit 0
fi

if hyprctl dispatch "$dispatcher" "$target_ws"; then
  mkdir -p "$state_dir"
  printf "%s\n%s\n" "$active_ws" "$target_ws" >"$state_file"
  exit 0
fi

exit 1
