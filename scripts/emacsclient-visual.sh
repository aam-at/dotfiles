#!/usr/bin/env bash

set -euo pipefail

# Use the profile selected by `emacs-daemon switch` instead of Emacs's legacy
# default socket.  EMACS_PROFILE remains available for one-off selection.
state_root="${XDG_STATE_HOME:-$HOME/.local/state}/emacs"
selected_profile_file="$state_root/default-profile"
profile="${EMACS_PROFILE:-}"
if [[ -z "$profile" && -r "$selected_profile_file" ]]; then
  read -r profile <"$selected_profile_file"
fi
profile="${profile:-doom}"
exec emacs-daemon open "$profile" "$@"
