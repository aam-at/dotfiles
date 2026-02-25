#!/usr/bin/env bash
set -euo pipefail

warn() { echo "WARN: $*" >&2; }
die() {
  echo "ERROR: $*" >&2
  exit 1
}

# -----------------------------
# Detect package manager (prefer bun)
# -----------------------------
if command -v bun >/dev/null 2>&1; then
  PM=bun
elif command -v npm >/dev/null 2>&1; then
  PM=npm
  warn "bun not found; falling back to npm."
else
  die "Neither bun nor npm found. Install Bun or Node.js first."
fi

# NPM user-global prefix
if [[ "$PM" == "npm" ]]; then
  NPM_PREFIX="${NPM_PREFIX:-$HOME/.npm-global}"
  export NPM_CONFIG_PREFIX="$NPM_PREFIX"
  export PATH="$NPM_PREFIX/bin:$PATH"
  npm install -g npm >/dev/null 2>&1 || warn "Could not update npm."
fi

if [[ "$PM" == "bun" ]]; then
  BUN_PREFIX="${BUN_PREFIX:-$HOME/.bun}"
  export BUN_CONFIG_PREFIX="$BUN_PREFIX"
  export PATH="$BUN_PREFIX/bin:$PATH"
fi

# -----------------------------
# Installer helpers
# -----------------------------
install_packages() {
  if [[ "$PM" == "bun" ]]; then
    bun add -g "$@"
  else
    npm install -g "$@"
  fi
}

run_x() {
  if [[ "$PM" == "bun" ]] && command -v bunx >/dev/null 2>&1; then
    bunx "$@"
  elif command -v npx >/dev/null 2>&1; then
    npx "$@"
  else
    die "Neither bunx nor npx available to run: $*"
  fi
}

# -----------------------------
# Global packages
# -----------------------------
packages=(
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
  "typescript"
  "typescript-formatter"
  "typescript-language-server"
  "vim-language-server"
  "vscode-json-languageserver"
  "yaml-language-server"
)

install_packages "${packages[@]}"

# -----------------------------
# AI skills integration
# -----------------------------
add_skills() {
  local repo="$1"
  shift
  local -a args=()
  for s in "$@"; do args+=(--skill "$s"); done
  run_x skills add "$repo" "${args[@]}" --global --agent "*" -y
}

add_skills "K-Dense-AI/claude-scientific-skills" \
  dask latex-posters literature-review matplotlib openalex-database \
  paper-2-web peer-review plotly pptx-posters pufferlib pymoo \
  pytorch-lightning research-grants research-lookup scholar-evaluation \
  scikit-learn scientific-brainstorming scientific-critical-thinking \
  scientific-schematics scientific-slides scientific-visualization \
  scientific-writing seaborn torch-geometric transformers umap-learn \
  venue-templates

add_skills "anthropics/skills" docx pdf pptx xlsx

add_skills "alirezarezvani/claude-skills" \
  senior-backend senior-computer-vision senior-data-scientist \
  senior-ml-engineer senior-prompt-engineer

run_x skills add PleasePrompto/notebooklm-skill --all --global
run_x skills add blader/humanizer --all --global

# Install manually: superpowers — https://github.com/obra/superpowers
# Install manually: claude plugins — /plugin marketplace add anthropics/claude-plugins-official

echo "Done."
if [[ "$PM" == "npm" ]]; then
  echo "Ensure PATH includes: ${NPM_PREFIX}/bin"
fi
if [[ "$PM" == "bun" ]]; then
  echo "Ensure PATH includes: ${BUN_PREFIX}/bin"
fi
