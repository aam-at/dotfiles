#!/usr/bin/env bash

set -euo pipefail

# Ensure npm is available before proceeding.
if ! command -v npm >/dev/null 2>&1; then
  echo "npm is not installed. Please install Node.js before running this script." >&2
  exit 1
fi

# Use sudo when available; otherwise fall back to running npm directly.
SUDO_BIN=${SUDO_BIN:-}
if [[ -z "${SUDO_BIN}" ]] && command -v sudo >/dev/null 2>&1; then
  SUDO_BIN="sudo"
fi

run_global_npm() {
  if [[ -n "${SUDO_BIN}" ]]; then
    "${SUDO_BIN}" npm install -g "$@"
  else
    npm install -g "$@"
  fi
}

# Keep npm itself up to date.
run_global_npm npm

# Common set of global npm packages used across supported distributions.
npm_packages=(
  "@anthropic-ai/claude-code@latest"
  "@github/copilot@latest"
  "@google/gemini-cli@latest"
  "@openai/codex@latest"
  "@qwen-code/qwen-code@latest"
  "@vibe-kit/grok-cli@latest"
  "opencode-ai@latest"
  "bibtex-tidy"
  "bash-language-server"
  "js-beautify"
  "prettier"
  "tslint"
  "typescript"
  "typescript-formatter"
  "typescript-language-server"
  "vim-language-server"
  "vscode-json-languageserver"
)

run_global_npm "${npm_packages[@]}"
