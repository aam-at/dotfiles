#!/usr/bin/env bash

set -euo pipefail

if ! command -v uv >/dev/null 2>&1; then
  echo "uv is not installed. Install uv before running this script." >&2
  exit 1
fi

uv_tools=(
  autoflake
  autopep8
  basedpyright
  black
  cmake-language-server
  docformatter
  flake8
  git+https://github.com/bcbernardo/aw-watcher-ask.git
  gpustat
  isort
  docling
  nvitop
  poetry
  pre-commit
  proselint
  pylint
  pyrefly
  ruff
  semgrep
  textLSP
  ty
  yapf
)

for tool in "${uv_tools[@]}"; do
  uv tool install -U "$tool"
done

uv tool update-shell
