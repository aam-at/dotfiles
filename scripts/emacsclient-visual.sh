#!/usr/bin/env bash

set -euo pipefail

# Mirror the emacsclient.desktop Exec behavior:
# - with file args: reuse existing frame
# - without args: create a new frame
if [[ $# -gt 0 ]]; then
  exec /usr/bin/emacsclient --alternate-editor= --reuse-frame "$@"
else
  exec /usr/bin/emacsclient --alternate-editor= --create-frame
fi
