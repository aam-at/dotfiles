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
  "openclaw"
  "bash-language-server"
  "bibtex-tidy"
  "js-beautify"
  "@marp-team/marp-cli"
  "prettier"
  "tslint"
  "typescript"
  "typescript-formatter"
  "typescript-language-server"
  "vim-language-server"
  "vscode-json-languageserver"
  "yaml-language-server"
)

run_global_npm "${npm_packages[@]}"

# AI skills integration
add_skills() {
  local repo="$1"
  shift
  local -a skills=("$@")

  # Build: --skill a --skill b --skill c ...
  local -a skill_args=()
  for s in "${skills[@]}"; do
    skill_args+=(--skill "$s")
  done

  npx skills add "$repo" "${skill_args[@]}" --global --agent "*" -y
}

# --- K-Dense-AI/claude-scientific-skills ---
KDENSE_SKILLS=(
  venue-templates
  scholar-evaluation
  scientific-brainstorming
  scientific-critical-thinking
  scientific-schematics
  scientific-slides
  scientific-visualization
  scientific-writing
  research-grants
  research-lookup
  pufferlib
  pymoo
  pptx-posters
  dask
  latex-posters
  literature-review
  matplotlib
  openalex-database
  paper-2-web
  peer-review
  plotly
  pytorch-lightning
  scikit-learn
  seaborn
  torch-geometric
  transformers
  umap-learn
)

add_skills "K-Dense-AI/claude-scientific-skills" "${KDENSE_SKILLS[@]}"
add_skills "anthropics/skills" docx pdf pptx xlsx
add_skills "alirezarezvani/claude-skills" \
  senior-backend \
  senior-computer-vision \
  senior-data-scientist \
  senior-ml-engineer \
  senior-prompt-engineer

npx skills add PleasePrompto/notebooklm-skill --all
npx skills add blader/humanizer --all

# Install manually superpowers: https://github.com/obra/superpowers
# Install manually claude plugins: /plugin marketplace add anthropics/claude-plugins-official
