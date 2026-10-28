#!/usr/bin/env bash
# Update system and user applications installed by ../install_arch.sh.
set -uo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
REPO_DIR=$(cd -- "$SCRIPT_DIR/.." && pwd)
source "$SCRIPT_DIR/update-common.sh"
parse_update_args "$0" "$@" || exit $?

if ((SKIP_SYSTEM)); then
  skip_step 'System packages and Flatpak' '--skip-system'
else
  if command -v yay >/dev/null 2>&1; then
    run_step 'Arch repository and AUR packages' yay -Syu
  elif command -v pacman >/dev/null 2>&1; then
    printf 'yay not found; AUR packages will not be updated.\n' >&2
    run_step 'Arch repository packages' sudo pacman -Syu
  else
    printf 'This updater requires an Arch-based system with pacman.\n' >&2
    exit 2
  fi

  if command -v flatpak >/dev/null 2>&1; then
    run_step 'Flatpak applications and runtimes' flatpak update
  fi
fi

update_editor_targets
update_common_targets
finish_updates
