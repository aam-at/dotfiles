#!/usr/bin/env bash

set -euo pipefail

if ! command -v uv >/dev/null 2>&1; then
  echo "uv is not installed. Install uv before running this script." >&2
  exit 1
fi

uv_tools=(
  aider-chat
  autoflake
  autopep8
  basedpyright
  black
  cmake-language-server
  docformatter
  flake9
  git+https://github.com/bcbernardo/aw-watcher-ask.git
  gpustat
  isort
  marker-pdf
  nvitop
  poetry
  pre-commit
  proselint
  pylint
  ruff
  semgrep
  textLSP
  yapf
)

for tool in "${uv_tools[@]}"; do
  uv tool install "$tool"
done
