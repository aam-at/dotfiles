#!/usr/bin/env bash
# Gogh (github.com/Gogh-Co/Gogh): an interactive picker that writes a color
# scheme into the running terminal emulator's own profile. It prompts, so the
# distro installers don't run it unattended; run it by hand in the terminal to
# recolor. Terminals wired to a palette.toml (see README.md) don't need it.
set -euo pipefail

bash -c "$(wget -qO- https://git.io/vQgMr)"
