#!/usr/bin/env bash

# Debian/Ubuntu installation script
# This script installs packages and tools specific to Debian/Ubuntu

set -euo pipefail

# Source common functions
REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$REPO_DIR/setup/common.sh"
source "$REPO_DIR/setup/deb_packages.sh"

declare -a ADDITIONAL_INSTALL_STEPS=(
  install_additional_tools
  install_pyenv
  install_snap_packages
)

run_installation "$@"
