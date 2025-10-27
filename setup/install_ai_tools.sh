#!/usr/bin/env bash

# AI Tools Installation Script
# This script installs all AI-related packages and tools

set -euo pipefail

# Source common functions
REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$REPO_DIR/setup/common.sh"

# AI tools installation flags
INSTALL_OLLAMA=${INSTALL_OLLAMA:-false}
INSTALL_AI_NODE_PACKAGES=${INSTALL_AI_NODE_PACKAGES:-true}
INSTALL_AI_PYTHON_PACKAGES=${INSTALL_AI_PYTHON_PACKAGES:-true}

# Parse AI-specific command line arguments
parse_ai_args() {
  while [[ $# -gt 0 ]]; do
    case $1 in
    --ollama)
      INSTALL_OLLAMA=true
      shift
      ;;
    --no-ai-node)
      INSTALL_AI_NODE_PACKAGES=false
      shift
      ;;
    --no-ai-python)
      INSTALL_AI_PYTHON_PACKAGES=false
      shift
      ;;
    *)
      # Unknown option, let the calling script handle it
      return 1
      ;;
    esac
  done
  return 0
}

# Install AI-related Node.js packages
install_ai_node_packages() {
  if ! $INSTALL_AI_NODE_PACKAGES; then
    return 0
  fi

  echo "Installing AI-related Node.js packages..."
  
  # Ensure npm is available
  if ! command -v npm >/dev/null 2>&1; then
    echo "npm is not installed. Please install Node.js before running this script." >&2
    return 1
  fi

  # Use sudo when available; otherwise fall back to running npm directly
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

  # AI-related npm packages
  local ai_npm_packages=(
    "@anthropic-ai/claude-code@latest"
    "@github/copilot@latest"
    "@google/gemini-cli@latest"
    "@openai/codex@latest"
    "opencode-ai@latest"
  )

  run_global_npm "${ai_npm_packages[@]}"
}

# Install AI-related Python packages
install_ai_python_packages() {
  if ! $INSTALL_AI_PYTHON_PACKAGES; then
    return 0
  fi

  echo "Installing AI-related Python packages..."
  
  # Install uv if not available
  if ! command -v uv >/dev/null 2>&1; then
    pipx install uv
  fi

  # AI-related Python tools
  local ai_python_tools=(
    "aider-chat"
    "gpustat"
    "nvitop"
  )

  for tool in "${ai_python_tools[@]}"; do
    uv tool install "$tool"
  done
}

# Install Ollama and models
install_ollama() {
  if ! $INSTALL_OLLAMA; then
    return 0
  fi

  if ! command -v ollama &>/dev/null; then
    echo "Installing ollama..."
    curl -fsSL https://ollama.com/install.sh | sh
  fi
  
  echo "Downloading ollama models"
  local ollama_models=(
    # coding models
    "qwen2.5-coder:3b" 
    "qwen2.5-coder:7b"
    # general language models
    "gemma3:4b" 
    "gemma3:12b" 
    "phi4:mini"
    # embedding models
    "granite-embedding:278m" 
    "mxbai-embed-large:latest" 
    "nomic-embed-text:latest"
  )
  
  for ollama_model in "${ollama_models[@]}"; do
    echo "Pulling $ollama_model..."
    ollama pull "$ollama_model"
  done
}

# Install GitHub Copilot CLI extension
install_github_copilot_cli() {
  if command -v gh >/dev/null 2>&1; then
    echo "Installing GitHub Copilot CLI extension..."
    gh extension install github/gh-copilot
  else
    echo "GitHub CLI not found, skipping Copilot CLI installation"
  fi
}

# Install aichat (if not already installed)
install_aichat() {
  if ! command -v aichat >/dev/null 2>&1; then
    echo "Installing aichat..."
    if command -v cargo >/dev/null 2>&1; then
      cargo install aichat
    else
      echo "Cargo not found, skipping aichat installation"
    fi
  fi
}

# Main AI tools installation function
install_ai_tools() {
  echo "Installing AI tools and packages..."
  
  install_ai_node_packages
  install_ai_python_packages
  install_ollama
  install_github_copilot_cli
  install_aichat
  
  echo "AI tools installation complete!"
}

# If script is run directly, install AI tools
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  parse_ai_args "$@"
  install_ai_tools
fi