#!/usr/bin/env bash

set -euo pipefail

if ! command -v cargo >/dev/null 2>&1; then
  echo "cargo is not installed. Install Rust toolchain before running this script." >&2
  exit 1
fi

if ! command -v rustup >/dev/null 2>&1; then
  echo "rustup is not installed. Install Rust toolchain before running this script." >&2
  exit 1
fi

# Ensure cargo-binstall and other helper crates are present.
cargo install --locked cargo-binstall cargo-edit cargo-outdated

# Reuse cargo-binstall to install the wider toolchain.
if command -v cargo-binstall >/dev/null 2>&1; then
  cargo binstall -y \
    aichat argc atuin bottom broot eza gitu gitui gping kanata lsd ouch \
    sd tealdeer television texlab viu watchexec-cli yazi-cli yazi-fm
else
  echo "cargo-binstall not found on PATH; skipping cargo binstall packages." >&2
fi

# Additional tooling that requires git sources.
cargo install --git https://github.com/blahgeek/emacs-lsp-booster

# Ensure the key rustup components are available.
rustup component add rustfmt
rustup component add clippy
rustup component add rust-analyzer
