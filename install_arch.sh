#!/usr/bin/env bash

# Arch Linux installation script
# This script installs packages and tools specific to Arch Linux

set -euo pipefail

# Source common functions
REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$REPO_DIR/setup/common.sh"
source "$REPO_DIR/setup/arch_packages.sh"

# Detect distribution
if [ -f /etc/os-release ]; then
  . /etc/os-release
  echo "This script is running on $NAME"
else
  echo "Unable to detect distribution."
  exit 1
fi

run_installation "$@"
