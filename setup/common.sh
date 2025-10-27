#!/usr/bin/env bash

# Common functions and variables for install scripts
# This file should be sourced by distribution-specific install scripts

set -euo pipefail

# Global variables
REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TOOLS_DIR=${TOOLS_DIR:-"$HOME/local/tools"}

# Default installation flags
GUI=${GUI:-false}
INSTALL_PYTHON=${INSTALL_PYTHON:-true}
INSTALL_RUST=${INSTALL_RUST:-true}
INSTALL_GO=${INSTALL_GO:-true}
INSTALL_LUA=${INSTALL_LUA:-true}
INSTALL_NODE=${INSTALL_NODE:-true}
INSTALL_EMACS=${INSTALL_EMACS:-true}
INSTALL_FONTS=${INSTALL_FONTS:-true}

# Parse common command line arguments
parse_common_args() {
  while [[ $# -gt 0 ]]; do
    case $1 in
    --gui)
      GUI=true
      shift
      ;;
    --no-python)
      INSTALL_PYTHON=false
      shift
      ;;
    --no-rust)
      INSTALL_RUST=false
      shift
      ;;
    --no-go)
      INSTALL_GO=false
      shift
      ;;
    --no-node)
      INSTALL_NODE=false
      shift
      ;;
    --no-emacs)
      INSTALL_EMACS=false
      shift
      ;;
    --no-fonts)
      INSTALL_FONTS=false
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

# Common setup functions
setup_common() {
  echo "Setting up common environment..."
  
  # Source .bashrc
  source "$HOME/.bashrc" || true
  
  # Install Gogh Color theme
  bash -c "$(wget -qO- https://git.io/vQgMr)" || true
  
  # Create tools directory
  mkdir -p "$TOOLS_DIR"
}

# Font installation function
install_fonts() {
  if ! $INSTALL_FONTS; then
    return 0
  fi

  install_font_package() {
    local repo_url="$1"
    local dir_name="$2"
    local font_subdir="$3"

    if [ ! -d "$TOOLS_DIR/$dir_name" ]; then
      echo "Installing $font_subdir..."
      git clone --depth=1 "$repo_url" "$TOOLS_DIR/$dir_name"
    fi
    if [ ! -d "$font_subdir" ]; then
      install_fonts "$TOOLS_DIR/$dir_name" "" "$font_subdir"
    else
      echo "Skipping $font_subdir installation as it is already installed"
    fi
  }

  # Define font packages
  local font_packages=(
    "https://github.com/JetBrains/JetBrainsMono/|jetbrains-fonts|JetBrainsFonts"
    "https://github.com/adobe-fonts/source-code-pro|adobe-source-code-pro-fonts|AdobeFonts"
    "https://github.com/domtronn/all-the-icons.el|all-icons-fonts|AllIconsFonts"
    "https://github.com/iaolo/iA-Fonts|iawriter-fonts|iAWriterFonts"
    "https://github.com/powerline/fonts|powerline-fonts|PowerlineFonts"
    "https://github.com/ryanoasis/nerd-fonts|nerd-fonts|NerdFonts"
    "https://github.com/sebastiencs/icons-in-terminal|icons-fonts|IconsFonts"
  )

  # Install each font package
  for package in "${font_packages[@]}"; do
    IFS='|' read -r repo_url dir_name font_subdir <<<"$package"
    install_font_package "$repo_url" "$dir_name" "$font_subdir"
  done
}

# Python installation function
install_python_tools() {
  if ! $INSTALL_PYTHON; then
    return 0
  fi

  echo "Installing Python tools..."
  
  # Install uv and plugins
  pipx install uv

  local tools=(
    "autoflake"
    "autopep8"
    "basedpyright"
    "black"
    "cmake-language-server"
    "docformatter"
    "flake9"
    "git+https://github.com/bcbernardo/aw-watcher-ask.git"
    "isort"
    "marker-pdf"
    "poetry"
    "pre-commit"
    "proselint"
    "pylint"
    "ruff"
    "semgrep"
    "yapf"
  )

  for tool in "${tools[@]}"; do
    uv tool install "$tool"
  done
}

# Node.js installation function
install_node_tools() {
  if ! $INSTALL_NODE; then
    return 0
  fi

  echo "Installing Node.js tools..."
  "$REPO_DIR/setup/install_node_packages.sh"
}

# Go installation function
install_go_tools() {
  if ! $INSTALL_GO; then
    return 0
  fi

  echo "Installing Go tools..."
  "$REPO_DIR/setup/install_go_packages.sh"
}

# Rust installation function
install_rust_tools() {
  if ! $INSTALL_RUST; then
    return 0
  fi

  echo "Installing Rust tools..."
  source "$HOME/.cargo/env"
  "$REPO_DIR/setup/install_rust_packages.sh"
}

# Lua installation function
install_lua_tools() {
  if ! $INSTALL_LUA; then
    return 0
  fi

  echo "Installing Lua tools..."
  luarocks install --local tiktoken_core
}

# Spacemacs installation function
install_spacemacs() {
  if ! $INSTALL_EMACS; then
    return 0
  fi

  if [ ! -d "$HOME/.emacs.d" ]; then
    echo "Installing Spacemacs..."
    git clone https://github.com/aam-at/spacemacs ~/.emacs.d
  fi
}

# Intellimacs installation function
install_intellimacs() {
  if ! $GUI; then
    return 0
  fi

  if [ ! -d "$HOME/.intellimacs" ]; then
    echo "Installing Intellimacs..."
    git clone https://github.com/MarcoIeni/intellimacs ~/.intellimacs
  fi
}

# fzf installation function
install_fzf() {
  if [ ! -d "$HOME/.fzf" ]; then
    echo "Installing fzf..."
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    ~/.fzf/install --all
  fi
}



# Main installation function that calls all common installers
install_common_tools() {
  install_python_tools
  install_node_tools
  install_go_tools
  install_rust_tools
  install_lua_tools
  install_spacemacs
  install_intellimacs
  install_fzf
  install_fonts
}

# Orchestrate distro-specific installation flow using shared steps. Distribution
# scripts may optionally define ADDITIONAL_INSTALL_STEPS with extra functions to
# run after the language/toolchain installers.
run_installation() {
  parse_common_args "$@"
  setup_common

  local required_steps=(
    install_core_packages
    install_gui_packages
    install_nodejs
    install_rust
    install_go
    install_lua
  )

  local step
  for step in "${required_steps[@]}"; do
    if ! declare -f "$step" >/dev/null; then
      echo "Error: required function '$step' is not defined" >&2
      return 1
    fi
  done

  install_core_packages
  install_gui_packages
  install_nodejs
  install_rust
  install_go
  install_lua

  if declare -p ADDITIONAL_INSTALL_STEPS &>/dev/null; then
    local extra_step
    for extra_step in "${ADDITIONAL_INSTALL_STEPS[@]}"; do
      if declare -f "$extra_step" >/dev/null; then
        "$extra_step"
      else
        echo "Warning: additional install step '$extra_step' is not defined; skipping" >&2
      fi
    done
  fi

  install_common_tools
  "$REPO_DIR/setup/install_ai_tools.sh"
  print_completion
}

# Print completion message
print_completion() {
  echo "Setup complete!"
}
