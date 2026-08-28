#!/usr/bin/env bash

set -euo pipefail

pids=()

cleanup() {
  ((${#pids[@]} == 0)) || kill "${pids[@]}" 2>/dev/null || true
}

trap cleanup EXIT INT TERM

wl-paste --type text --watch cliphist store &
pids+=("$!")
wl-paste --type image --watch cliphist store &
pids+=("$!")

wait
