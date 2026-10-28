#!/usr/bin/env bash
# Shared update lifecycle and targets used by update-all-arch.sh and update-all-deb.sh.

UPDATE_FAILURES=0
UPDATE_SKIPS=0
UPDATE_SPACEMACS_PID=
UPDATE_SPACEMACS_LOG=
SKIP_SYSTEM=0
SKIP_TOOLCHAINS=0
SKIP_USER_APPS=0
SKIP_DOOM=0
SKIP_SPACEMACS=0

show_update_help() {
  local script_name="${1##*/}"

  cat <<EOF
Usage: $script_name [OPTIONS]

Update installed applications and tools through their existing update sources.
This includes system packages and user-installed tools; dotfile configuration
is managed separately.

Options:
  -h, --help        Show this help and exit.
  --skip-system     Skip distro packages and system-wide release installs.
  --skip-toolchains Skip Rust, Python, Node, Go, Ruby, and Lua updates.
  --skip-user-apps  Skip user-local apps and tools, including Emacs profiles.
  --skip-doom       Skip the Doom Emacs update.
  --skip-spacemacs  Skip the Spacemacs source and package update.
EOF
}

parse_update_args() {
  local script_name="$1"
  shift

  while (($# > 0)); do
    case "$1" in
      -h | --help)
        show_update_help "$script_name"
        exit 0
        ;;
      --skip-system) SKIP_SYSTEM=1 ;;
      --skip-toolchains) SKIP_TOOLCHAINS=1 ;;
      --skip-user-apps) SKIP_USER_APPS=1 ;;
      --skip-doom) SKIP_DOOM=1 ;;
      --skip-spacemacs) SKIP_SPACEMACS=1 ;;
      *)
        printf 'Unknown argument: %s\n' "$1" >&2
        show_update_help "$script_name"
        return 2
        ;;
    esac
    shift
  done
}

run_step() {
  local label="$1"
  shift

  printf '\n==> %s\n' "$label"
  if "$@"; then
    printf 'Done: %s\n' "$label"
    return 0
  else
    local status=$?
    printf 'Failed (%d): %s\n' "$status" "$label" >&2
    UPDATE_FAILURES=$((UPDATE_FAILURES + 1))
    return "$status"
  fi
}

skip_step() {
  local label="$1"
  shift

  printf '\n==> %s\nSkipped: %s\n' "$label" "$*"
  UPDATE_SKIPS=$((UPDATE_SKIPS + 1))
  return 0
}

update_git_checkout() {
  local label="$1"
  local directory="$2"
  local changes

  [[ -d "$directory/.git" ]] || return 0
  if ! changes=$(git -C "$directory" status --porcelain); then
    printf 'Could not inspect %s at %s\n' "$label" "$directory" >&2
    UPDATE_FAILURES=$((UPDATE_FAILURES + 1))
    return 1
  fi
  if [[ -n "$changes" ]]; then
    skip_step "$label" "local changes in $directory"
    return 2
  fi

  run_step "$label" git -C "$directory" pull --ff-only
}

update_ollama() {
  curl -fsSL https://ollama.com/install.sh | sh
}

start_spacemacs_package_update() {
  local data_dir="${XDG_DATA_HOME:-$HOME/.local/share}/emacs"
  local config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/emacs"
  local source_dir="$data_dir/spacemacs"
  local profile_dir="$config_dir/spacemacs"
  local log_dir="${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles"

  if ((SKIP_USER_APPS)); then
    return 0
  fi
  if ((SKIP_SPACEMACS)); then
    skip_step 'Spacemacs' '--skip-spacemacs'
    return 0
  fi
  [[ -d "$source_dir/.git" ]] || return 0
  update_git_checkout 'Spacemacs source' "$source_dir" || return 0
  command -v emacs >/dev/null 2>&1 || return 0
  [[ -f "$profile_dir/init.el" && -f "$source_dir/init.el" ]] || return 0

  if ! mkdir -p "$log_dir"; then
    printf 'Could not create Spacemacs update log directory: %s\n' "$log_dir" >&2
    UPDATE_FAILURES=$((UPDATE_FAILURES + 1))
    return 1
  fi
  if ! UPDATE_SPACEMACS_LOG=$(mktemp "$log_dir/spacemacs-update.XXXXXX"); then
    printf 'Could not create a Spacemacs update log in %s\n' "$log_dir" >&2
    UPDATE_FAILURES=$((UPDATE_FAILURES + 1))
    return 1
  fi

  env SPACEMACSDIR="$profile_dir" emacs --batch --init-directory="$source_dir" \
    -l "$source_dir/init.el" \
    --eval='(configuration-layer/update-packages t)' \
    >"$UPDATE_SPACEMACS_LOG" 2>&1 &
  UPDATE_SPACEMACS_PID=$!
  printf 'Started Spacemacs package update in background (pid %s; log %s).\n' \
    "$UPDATE_SPACEMACS_PID" "$UPDATE_SPACEMACS_LOG"
}

wait_for_spacemacs_package_update() {
  [[ -n "$UPDATE_SPACEMACS_PID" ]] || return 0

  if wait "$UPDATE_SPACEMACS_PID"; then
    printf '\nDone: Spacemacs packages\n'
    rm -f "$UPDATE_SPACEMACS_LOG"
  else
    local status=$?
    printf '\nFailed (%d): Spacemacs packages; see %s\n' \
      "$status" "$UPDATE_SPACEMACS_LOG" >&2
    UPDATE_FAILURES=$((UPDATE_FAILURES + 1))
  fi
  UPDATE_SPACEMACS_PID=
}

update_common_targets() {
  if ((SKIP_TOOLCHAINS)); then
    skip_step 'Language toolchains' '--skip-toolchains'
  else
    if command -v rustup >/dev/null 2>&1; then
      run_step 'Rust toolchains and components' rustup update
    fi
    if command -v cargo-install-update >/dev/null 2>&1; then
      run_step 'Cargo-installed applications' cargo install-update --all --git
    fi

    if command -v pipx >/dev/null 2>&1; then
      export PIPX_BIN_DIR="${PIPX_BIN_DIR:-$HOME/.local/bin}"
      export PATH="$PIPX_BIN_DIR:$PATH"
      run_step 'pipx applications' pipx upgrade-all
    fi
    if command -v uv >/dev/null 2>&1; then
      run_step 'uv tools' uv tool upgrade --all
    fi

    if command -v bun >/dev/null 2>&1; then
      BUN_PREFIX="${BUN_PREFIX:-$HOME/.bun}"
      export BUN_CONFIG_PREFIX="$BUN_PREFIX"
      export PATH="$BUN_PREFIX/bin:$PATH"
      run_step 'Bun global applications' bun update --global
    elif command -v npm >/dev/null 2>&1; then
      NPM_PREFIX="${NPM_PREFIX:-$HOME/.npm-global}"
      export NPM_CONFIG_PREFIX="$NPM_PREFIX"
      export PATH="$NPM_PREFIX/bin:$PATH"
      run_step 'npm global applications' npm update --global
      run_step 'npm itself' npm install --global npm@latest
    fi

    if command -v go >/dev/null 2>&1; then
      run_step 'Go applications installed by this repo' "$REPO_DIR/setup/install_go_packages.sh"
    fi
  fi

  if ((SKIP_USER_APPS)); then
    skip_step 'User applications' '--skip-user-apps'
  else
    if command -v codex >/dev/null 2>&1; then
      run_step 'Codex CLI' codex update
    fi
    if command -v herdr >/dev/null 2>&1; then
      run_step 'Herdr' herdr update
    fi
    if command -v ollama >/dev/null 2>&1; then
      run_step 'Ollama' update_ollama
    fi

    if command -v aw-awatcher >/dev/null 2>&1 ||
      [[ -x "$HOME/.local/opt/aw-awatcher/aw-awatcher" ]]; then
      run_step 'ActivityWatch Wayland watcher' "$REPO_DIR/setup/install_activitywatch.sh"
    fi

    if [[ -x "$HOME/.local/lib/emacs-tray/emacs-tray" ]]; then
      run_step 'emacs-tray' make -C "$REPO_DIR/tools/emacs-tray" install
    fi
    if [[ -x "$HOME/.local/bin/wellbeing" ]]; then
      run_step 'wellbeing' make -C "$REPO_DIR/tools/wellbeing" install
    fi

    local fzf_dir="$HOME/.fzf"
    if [[ -d "$fzf_dir/.git" ]] && update_git_checkout 'fzf source' "$fzf_dir"; then
      run_step 'fzf binary' "$fzf_dir/install" --bin
    fi

    local intellimacs_dir="$HOME/.intellimacs"
    if [[ -d "$intellimacs_dir/.git" ]]; then
      update_git_checkout 'Intellimacs' "$intellimacs_dir"
    fi
  fi
}

update_editor_targets() {
  local doom_dir="${XDG_DATA_HOME:-$HOME/.local/share}/emacs/doom"
  local emacs_config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/emacs"
  if ((SKIP_USER_APPS)); then
    return 0
  fi
  if ((SKIP_DOOM)); then
    skip_step 'Doom Emacs' '--skip-doom'
  elif [[ -x "$doom_dir/bin/doom" && -f "$emacs_config_dir/doom/init.el" ]]; then
    run_step 'Doom Emacs and packages' "$REPO_DIR/scripts/doom-profile" upgrade
  fi

  start_spacemacs_package_update
}

finish_updates() {
  wait_for_spacemacs_package_update

  if ((UPDATE_FAILURES > 0)); then
    printf '\nUpdates finished with %d failed step(s) and %d skipped part(s).\n' \
      "$UPDATE_FAILURES" "$UPDATE_SKIPS" >&2
    return 1
  fi

  if ((UPDATE_SKIPS > 0)); then
    printf '\nAll available update steps finished; %d part(s) skipped.\n' \
      "$UPDATE_SKIPS"
  else
    printf '\nAll available update steps finished.\n'
  fi
}
