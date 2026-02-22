#!/usr/bin/env bash
#
# health-check.sh - Verify all dotfiles tools are working correctly
#
# Usage: ./health-check.sh [options]
#   -v, --verbose    Show verbose output
#   -q, --quiet      Show only errors
#   -h, --help       Show this help message
#

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# Configuration
VERBOSE=false
QUIET=false

# Counters
TOTAL=0
PASSED=0
FAILED=0
WARNINGS=0

# Parse arguments
while [[ $# -gt 0 ]]; do
  case $1 in
  -v | --verbose)
    VERBOSE=true
    shift
    ;;
  -q | --quiet)
    QUIET=true
    shift
    ;;
  -h | --help)
    echo "Health check script for dotfiles"
    echo ""
    echo "Usage: $0 [options]"
    echo ""
    echo "Options:"
    echo "  -v, --verbose    Show verbose output"
    echo "  -q, --quiet      Show only errors"
    echo "  -h, --help       Show this help message"
    exit 0
    ;;
  *)
    echo -e "${RED}Unknown option: $1${NC}"
    exit 1
    ;;
  esac
done

# Helper functions
log_info() {
  [ "$QUIET" = true ] && return
  echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
  [ "$QUIET" = true ] && return
  echo -e "${GREEN}[PASS]${NC} $1"
}

log_warning() {
  echo -e "${YELLOW}[WARN]${NC} $1"
  ((++WARNINGS))
}

log_error() {
  echo -e "${RED}[FAIL]${NC} $1"
  ((++FAILED))
}

log_check() {
  [ "$QUIET" = true ] && return
  echo -e "${CYAN}[CHECK]${NC} $1"
}

check() {
  local name="$1"
  local cmd="$2"
  ((++TOTAL))

  if eval "$cmd" &>/dev/null; then
    log_success "$name"
    ((++PASSED))
    return 0
  else
    log_error "$name"
    return 1
  fi
}

check_with_output() {
  local name="$1"
  local cmd="$2"
  ((++TOTAL))

  local output
  if output=$(eval "$cmd" 2>&1); then
    log_success "$name"
    [ "$VERBOSE" = true ] && echo -e "  ${MAGENTA}Output:${NC} $output"
    ((++PASSED))
    return 0
  else
    log_error "$name - $output"
    return 1
  fi
}

section() {
  echo ""
  echo -e "${CYAN}========================================${NC}"
  echo -e "${CYAN}  $1${NC}"
  echo -e "${CYAN}========================================${NC}"
}

# Check shell and terminal
check_shell() {
  section "Shell & Terminal"

  check "Fish shell" "command -v fish"
  check "Starship prompt" "command -v starship"
  check "Zoxide" "command -v zoxide"
  check "Atuin" "command -v atuin"

  # Check if fish is the current shell
  if [[ "$SHELL" == *"fish"* ]]; then
    log_success "Fish is the default shell"
  else
    log_warning "Fish is not the default shell (current: $SHELL)"
  fi

  # Check starship initialization
  if starship --version &>/dev/null; then
    log_success "Starship is working"
  fi
}

# Check editors
check_editors() {
  section "Editors"

  check "Neovim" "command -v nvim"
  check "Helix" "command -v helix"
  check "Emacs" "command -v emacs"

  # Check Neovim version
  if command -v nvim &>/dev/null; then
    local nvim_version=$(nvim --version | head -1)
    [ "$VERBOSE" = true ] && log_info "Neovim: $nvim_version"
  fi

  # Check LazyVim plugins
  if [ -d "$HOME/.local/share/nvim/lazy" ]; then
    local plugin_count=$(ls -1 "$HOME/.local/share/nvim/lazy" 2>/dev/null | wc -l)
    log_info "LazyVim plugins installed: $plugin_count"
  fi
}

# Check terminal multiplexers
check_multiplexers() {
  section "Terminal Multiplexers"

  check "Tmux" "command -v tmux"
  check "Zellij" "command -v zellij"

  # Check tmux version
  if command -v tmux &>/dev/null; then
    local tmux_version=$(tmux -V)
    [ "$VERBOSE" = true ] && log_info "Tmux: $tmux_version"
  fi

  # Check tmux plugins
  if [ -d "$HOME/.tmux/plugins" ]; then
    local plugin_count=$(ls -1 "$HOME/.tmux/plugins" 2>/dev/null | wc -l)
    log_info "Tmux plugins installed: $plugin_count"
  else
    log_warning "Tmux plugins directory not found"
  fi
}

# Check file tools
check_file_tools() {
  section "File Tools"

  check "Ripgrep (rg)" "command -v rg"
  check "fd" "command -v fd"
  check "fzf" "command -v fzf"
  check "bat" "command -v bat"
  check "eza" "command -v eza"
  check "yazi" "command -v yazi"
  check "jq" "command -v jq"
}

# Check git tools
check_git_tools() {
  section "Git Tools"

  check "Git" "command -v git"
  check "Delta" "command -v delta"
  check "Lazygit" "command -v lazygit"
  check "GitUI" "command -v gitui"

  # Check git configuration
  if command -v git &>/dev/null; then
    local git_user=$(git config --global user.name 2>/dev/null || echo "not set")
    local git_email=$(git config --global user.email 2>/dev/null || echo "not set")

    if [ "$git_user" != "not set" ] && [ "$git_email" != "not set" ]; then
      log_success "Git user configured: $git_user"
    else
      log_warning "Git user not configured"
    fi
  fi
}

# Check system tools
check_system_tools() {
  section "System Tools"

  check "btop" "command -v btop"
  check "dust" "command -v dust"
  check "ncdu" "command -v ncdu"
  check "procs" "command -v procs"
  check "duf" "command -v duf"
  check "watchexec" "command -v watchexec"
}

# Check development tools
check_dev_tools() {
  section "Development Tools"

  check "Python 3" "command -v python3"
  check "Node.js" "command -v node"
  check "npm" "command -v npm"
  check "Cargo" "command -v cargo"
  check "Rustc" "command -v rustc"
  check "Go" "command -v go"

  # Check mise/asdf
  if command -v mise &>/dev/null; then
    log_success "mise version manager installed"
  elif command -v asdf &>/dev/null; then
    log_success "asdf version manager installed"
  else
    log_warning "No version manager found (mise/asdf)"
  fi
}

# Check quality of life tools
check_qol_tools() {
  section "Quality of Life Tools"

  check "httpie" "command -v http"
  check "xh" "command -v xh"
  check "tldr" "command -v tldr"
  check "direnv" "command -v direnv"
}

# Check AI tools
check_ai_tools() {
  section "AI Tools"

  check "Claude" "command -v claude"
  check "Claude-desktop" "command -v claude-desktop"
  check "Codex" "command -v codex"
  check "Gemini" "command -v gemini"
  check "Opencode" "command -v opencode"
}

# Check performance
check_performance() {
  section "Performance"

  # Check shell startup time
  if command -v fish &>/dev/null; then
    local fish_time=$(fish -c "exit" 2>&1 | grep -oP '\d+\.\d+' || echo "N/A")
    log_info "Fish startup time: ${fish_time}s"

    if (($(echo "$fish_time > 0.5" | bc -l 2>/dev/null || echo 0))); then
      log_warning "Fish startup is slow (>0.5s)"
    else
      log_success "Fish startup is fast"
    fi
  fi

  # Check Neovim startup time
  if command -v nvim &>/dev/null; then
    local nvim_time=$(nvim --headless -c 'qa' 2>&1 | grep -oP '\d+\.\d+' || echo "N/A")
    log_info "Neovim startup time: ${nvim_time}s"
  fi
}

# Print summary
print_summary() {
  echo ""
  echo -e "${CYAN}========================================${NC}"
  echo -e "${CYAN}  Summary${NC}"
  echo -e "${CYAN}========================================${NC}"
  echo ""
  echo -e "Total checks:  ${TOTAL}"
  echo -e "${GREEN}Passed:${NC}        ${PASSED}"
  echo -e "${RED}Failed:${NC}        ${FAILED}"
  echo -e "${YELLOW}Warnings:${NC}      ${WARNINGS}"
  echo ""

  local success_rate=$((PASSED * 100 / TOTAL))
  echo -e "Success rate:  ${success_rate}%"
  echo ""

  if [ $FAILED -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✓ All checks passed!${NC}"
  elif [ $FAILED -eq 0 ]; then
    echo -e "${YELLOW}⚠ All critical checks passed, but there are warnings${NC}"
  else
    echo -e "${RED}✗ Some checks failed. Please review the output above.${NC}"
  fi
  echo ""
}

# Recommendations
show_recommendations() {
  if [ $WARNINGS -gt 0 ] || [ $FAILED -gt 0 ]; then
    echo -e "${CYAN}========================================${NC}"
    echo -e "${CYAN}  Recommendations${NC}"
    echo -e "${CYAN}========================================${NC}"
    echo ""

    if ! command -v fish &>/dev/null; then
      echo -e "• Install fish shell for the best experience"
    fi

    if [ "$SHELL" != *"fish"* ]; then
      echo -e "• Set fish as your default shell: chsh -s \$(which fish)"
    fi

    if ! command -v starship &>/dev/null; then
      echo -e "• Install starship prompt: curl -sS https://starship.rs/install.sh | sh"
    fi

    if ! command -v zoxide &>/dev/null; then
      echo -e "• Install zoxide for smarter cd: curl -sS https://raw.githubusercontent.com/ajeetdsouza/zoxide/main/install.sh | bash"
    fi

    if [ ! -d "$HOME/.tmux/plugins" ]; then
      echo -e "• Install tmux plugins: In tmux, press <prefix> + I (Ctrl-a + I)"
    fi

    echo ""
  fi
}

# Main
main() {
  echo -e "${GREEN}========================================${NC}"
  echo -e "${GREEN}   Dotfiles Health Check${NC}"
  echo -e "${GREEN}   $(date)${NC}"
  echo -e "${GREEN}========================================${NC}"

  check_shell
  check_editors
  check_multiplexers
  check_file_tools
  check_git_tools
  check_system_tools
  check_dev_tools
  check_qol_tools
  check_ai_tools
  check_performance

  print_summary
  show_recommendations

  # Exit with error code if any checks failed
  if [ $FAILED -gt 0 ]; then
    exit 1
  fi
}

main "$@"
