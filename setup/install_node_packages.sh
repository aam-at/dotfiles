#!/usr/bin/env bash
set -euo pipefail

warn() { echo "WARN: $*" >&2; }
die() {
  echo "ERROR: $*" >&2
  exit 1
}

# -----------------------------
# Detect package manager
# -----------------------------
HAVE_BUN=false
HAVE_NPM=false

command -v bun >/dev/null 2>&1 && HAVE_BUN=true
command -v npm >/dev/null 2>&1 && HAVE_NPM=true

if [[ "${HAVE_BUN}" != "true" && "${HAVE_NPM}" != "true" ]]; then
  die "Neither bun nor npm is installed. Please install Bun or Node.js/npm first."
fi

if [[ "${HAVE_BUN}" != "true" ]]; then
  warn "bun is not available; falling back to npm."
fi

# -----------------------------
# User-local global install dirs
# -----------------------------
# NPM user-global prefix (no sudo)
NPM_PREFIX="${NPM_PREFIX:-$HOME/.npm-global}"
export NPM_CONFIG_PREFIX="${NPM_PREFIX}"
export PATH="${NPM_PREFIX}/bin:${PATH}"

# Bun user-global dir (no sudo)
# bun installs globals to BUN_INSTALL_GLOBAL_DIR/bin
BUN_GLOBAL_DIR="${BUN_GLOBAL_DIR:-$HOME/.bun-global}"
export BUN_INSTALL_GLOBAL_DIR="${BUN_GLOBAL_DIR}"
export PATH="${BUN_INSTALL_GLOBAL_DIR}/bin:${PATH}"

# -----------------------------
# Installer wrapper
# -----------------------------
install_globals() {
  if [[ "${HAVE_BUN}" == "true" ]]; then
    # bun add -g supports multiple packages
    bun add -g "$@"
  else
    # npm i -g supports multiple packages
    npm install -g "$@"
  fi
}

# Make sure we can run npx later (comes with npm). If using bun-only machines,
# prefer `bunx` as a fallback.
run_npx() {
  if command -v npx >/dev/null 2>&1; then
    npx "$@"
  elif command -v bunx >/dev/null 2>&1; then
    bunx "$@"
  else
    die "Neither npx nor bunx is available to run: $*"
  fi
}

# -----------------------------
# Optional: keep npm itself updated (only if npm is used/installed)
# -----------------------------
if [[ "${HAVE_NPM}" == "true" ]]; then
  # This updates npm in the user prefix; harmless if already current.
  npm install -g npm >/dev/null 2>&1 || warn "Could not update npm (continuing)."
fi

# -----------------------------
# Global CLIs
# -----------------------------
npm_packages=(
  "@anthropic-ai/claude-code@latest"
  "@github/copilot@latest"
  "@google/gemini-cli@latest"
  "@mariozechner/pi-coding-agent"
  "@marp-team/marp-cli"
  "@openai/codex@latest"
  "@qwen-code/qwen-code@latest"
  "@sylphx/pdf-reader-mcp"
  "@th0rgal/ralph-wiggum"
  "@vibe-kit/grok-cli@latest"
  "bash-language-server"
  "bibtex-tidy"
  "js-beautify"
  "openclaw"
  "opencode-ai@latest"
  "prettier"
  "tslint"
  "typescript"
  "typescript-formatter"
  "typescript-language-server"
  "vim-language-server"
  "vscode-json-languageserver"
  "yaml-language-server"
)

install_globals "${npm_packages[@]}"

# -----------------------------
# AI skills integration
# -----------------------------
add_skills() {
  local repo="$1"
  shift
  local -a skills=("$@")

  local -a skill_args=()
  for s in "${skills[@]}"; do
    skill_args+=(--skill "$s")
  done

  run_npx skills add "$repo" "${skill_args[@]}" --global --agent "*" -y
}

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

run_npx skills add PleasePrompto/notebooklm-skill --all
run_npx skills add blader/humanizer --all

# Install manually superpowers: https://github.com/obra/superpowers
# Install manually claude plugins: /plugin marketplace add anthropics/claude-plugins-official

echo "Done. Ensure your shell PATH includes:"
echo "  - ${NPM_PREFIX}/bin"
echo "  - ${BUN_INSTALL_GLOBAL_DIR}/bin"
