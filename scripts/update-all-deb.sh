#!/usr/bin/env bash
# Update installed targets through their owning source; configuration is separate.
set -uo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
REPO_DIR=$(cd -- "$SCRIPT_DIR/.." && pwd)
source "$SCRIPT_DIR/update-common.sh"
parse_update_args "$0" "$@" || exit $?
WORK_DIR=$(mktemp -d)
trap 'rm -rf "$WORK_DIR"' EXIT

download_release_asset() {
  local repo="$1"
  local filter="$2"
  local destination="$3"
  local url

  if ! url=$(curl -fsSL "https://api.github.com/repos/$repo/releases/latest" |
    jq -r ".assets[] | select($filter) | .browser_download_url" | sed -n '1p'); then
    return 1
  fi
  if [[ -z "$url" || "$url" == null ]]; then
    printf 'No matching release asset found for %s.\n' "$repo" >&2
    return 1
  fi
  wget -q -O "$destination" "$url"
}

update_github_deb() {
  local command_name="$1"
  local repo="$2"
  local filter="$3"
  local deb="$WORK_DIR/$command_name.deb"
  local package latest_version installed_version

  download_release_asset "$repo" "$filter" "$deb" || return 1
  package=$(dpkg-deb -f "$deb" Package) || return 1
  latest_version=$(dpkg-deb -f "$deb" Version) || return 1
  if installed_version=$(dpkg-query -W -f='${Version}' "$package" 2>/dev/null); then
    if dpkg --compare-versions "$latest_version" le "$installed_version"; then
      printf '%s is current (%s).\n' "$command_name" "$installed_version"
      return 0
    fi
  fi
  sudo dpkg -i "$deb"
}

update_tmux() {
  local source_dir="$WORK_DIR/tmux"

  download_release_asset tmux/tmux '.name | endswith(".tar.gz")' \
    "$WORK_DIR/tmux.tar.gz" || return 1
  mkdir -p "$source_dir" || return 1
  tar -xzf "$WORK_DIR/tmux.tar.gz" -C "$source_dir" --strip-components=1 || return 1
  (
    cd "$source_dir" &&
      ./configure &&
      make -j "$(nproc)" &&
      sudo make install prefix=/usr/local/stow/tmux
  ) || return 1
  (cd /usr/local/stow && sudo stow -R tmux)
}

update_neovim() {
  local install_dir=/usr/local/stow/nvim
  local source_dir="$WORK_DIR/nvim"

  download_release_asset neovim/neovim '.name == "nvim-linux-x86_64.tar.gz"' \
    "$WORK_DIR/nvim.tar.gz" || return 1
  mkdir -p "$source_dir" || return 1
  tar -xzf "$WORK_DIR/nvim.tar.gz" -C "$source_dir" --strip-components=1 || return 1
  [[ -x "$source_dir/bin/nvim" ]] || return 1
  sudo cp -a "$source_dir/." "$install_dir/" || return 1
  (cd /usr/local/stow && sudo stow -R nvim)
}

update_pathpicker() {
  local source_dir="$WORK_DIR/PathPicker"

  git clone --depth=1 https://github.com/facebook/PathPicker.git "$source_dir" || return 1
  (cd "$source_dir/debian" && ./package.sh && sudo dpkg -i ../*.deb)
}

update_noisetorch() {
  local source_dir="$WORK_DIR/NoiseTorch"

  git clone --depth=1 https://github.com/noisetorch/NoiseTorch "$source_dir" || return 1
  (cd "$source_dir" && make -j "$(nproc)") || return 1
  install -Dm755 "$source_dir/bin/noisetorch" "$HOME/.local/bin/noisetorch" || return 1
  install -Dm644 "$source_dir/assets/noisetorch.desktop" \
    "$HOME/.local/share/applications/noisetorch.desktop" || return 1
  install -Dm644 "$source_dir/assets/icon/noisetorch.png" \
    "$HOME/.local/share/icons/hicolor/256x256/apps/noisetorch.png"
}

update_activitywatch_server() (
  local opt="$HOME/.local/opt"
  local install_dir="$opt/activitywatch"
  local tag archive stage_dir candidate backup_dir keep_stage=0

  if ! tag=$(curl -fsSL https://api.github.com/repos/ActivityWatch/activitywatch/releases/latest |
    jq -er '.tag_name'); then
    return 1
  fi
  if [[ "$(cat "$install_dir/VERSION" 2>/dev/null)" == "$tag" ]]; then
    printf 'ActivityWatch server %s is current.\n' "$tag"
    return 0
  fi

  archive="$WORK_DIR/activitywatch.zip"
  mkdir -p "$opt" "$HOME/.local/bin" || return 1
  stage_dir=$(mktemp -d "$opt/.activitywatch-update.XXXXXX") || return 1
  candidate="$stage_dir/extract/activitywatch"
  backup_dir="$stage_dir/previous"
  cleanup_stage() {
    if ((keep_stage)); then
      printf 'Previous ActivityWatch server preserved at %s\n' "$backup_dir" >&2
    else
      rm -rf "$stage_dir"
    fi
  }
  trap cleanup_stage EXIT

  mkdir -p "$stage_dir/extract" || return 1
  wget -q -O "$archive" \
    "https://github.com/ActivityWatch/activitywatch/releases/download/$tag/activitywatch-$tag-linux-x86_64.zip" || return 1
  if command -v unzip >/dev/null 2>&1; then
    unzip -oq "$archive" -d "$stage_dir/extract" || return 1
  else
    python3 -m zipfile -e "$archive" "$stage_dir/extract" || return 1
  fi
  [[ -x "$candidate/aw-server/aw-server" ]] || return 1
  chmod +x "$candidate"/aw-*/aw-* || return 1
  printf '%s\n' "$tag" >"$candidate/VERSION" || return 1
  ln -sf "$install_dir/aw-server/aw-server" "$HOME/.local/bin/aw-server" || return 1

  mv "$install_dir" "$backup_dir" || return 1
  if ! mv "$candidate" "$install_dir"; then
    if ! mv "$backup_dir" "$install_dir"; then
      keep_stage=1
      printf 'Could not restore the previous ActivityWatch server.\n' >&2
    fi
    return 1
  fi
)

if ((SKIP_SYSTEM)); then
  skip_step 'System packages and system-wide releases' '--skip-system'
else
  # System package sources own packages installed from distro repositories.
  if ! command -v apt-get >/dev/null 2>&1; then
    printf 'This updater requires Debian or Ubuntu with apt-get.\n' >&2
    exit 2
  fi

  APT_COMMAND=apt-get
  if command -v apt-fast >/dev/null 2>&1; then
    APT_COMMAND=apt-fast
  fi
  run_step 'APT package index' sudo "$APT_COMMAND" update
  run_step 'APT package upgrades' sudo "$APT_COMMAND" upgrade -y
  run_step 'APT distribution upgrades' sudo "$APT_COMMAND" dist-upgrade -y

  if command -v snap >/dev/null 2>&1; then
    run_step 'Snap applications' sudo snap refresh
  fi
  if command -v flatpak >/dev/null 2>&1; then
    run_step 'Flatpak applications and runtimes' flatpak update
  fi

  # Direct upstream releases and source builds installed outside distro repos.
  if command -v delta >/dev/null 2>&1; then
    run_step 'delta' update_github_deb delta dandavison/delta \
      '.name | endswith("amd64.deb") and contains("musl")'
  fi
  if command -v fastfetch >/dev/null 2>&1; then
    run_step 'fastfetch' update_github_deb fastfetch fastfetch-cli/fastfetch \
      '.name | endswith("amd64.deb")'
  fi

  # tmux and Neovim are installed under /usr/local/stow by install_deb.sh.
  if [[ -d /usr/local/stow/tmux ]]; then
    run_step 'tmux source build' update_tmux
  fi
  if [[ -d /usr/local/stow/nvim ]]; then
    run_step 'Neovim release' update_neovim
  fi
  if command -v fpp >/dev/null 2>&1; then
    run_step 'PathPicker' update_pathpicker
  fi
fi

update_editor_targets

update_common_targets

if ((!SKIP_TOOLCHAINS)); then
  if command -v gem >/dev/null 2>&1 && command -v anystyle >/dev/null 2>&1; then
    run_step 'AnyStyle Ruby gems' sudo gem update anystyle anystyle-cli
  fi
  if command -v luarocks >/dev/null 2>&1 && luarocks show tiktoken_core >/dev/null 2>&1; then
    run_step 'tiktoken_core Lua rock' luarocks install --local --force tiktoken_core
  fi
fi

if ((!SKIP_USER_APPS)); then
  if [[ -d "$HOME/.local/opt/activitywatch" ]]; then
    run_step 'ActivityWatch standalone server' update_activitywatch_server
  fi
  if [[ -x "$HOME/.local/bin/noisetorch" ]]; then
    run_step 'NoiseTorch' update_noisetorch
  fi
fi

finish_updates
