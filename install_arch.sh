#!/usr/bin/env bash
set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$REPO_DIR/setup/lib.sh"

# Default values
GUI=${GUI:-false}
TOOLS_DIR=${TOOLS_DIR:-"$HOME/local/tools"}
INSTALL_PYTHON=${INSTALL_PYTHON:-true}
INSTALL_RUST=${INSTALL_RUST:-true}
INSTALL_GO=${INSTALL_GO:-true}
INSTALL_LUA=${INSTALL_LUA:-true}
INSTALL_NODE=${INSTALL_NODE:-true}
INSTALL_OLLAMA=${INSTALL_OLLAMA:-false}
INSTALL_EMACS=${INSTALL_EMACS:-true}
INSTALL_FONTS=${INSTALL_FONTS:-true}

if [ -f /etc/os-release ]; then
  . /etc/os-release
  echo "This script is running on $NAME"
else
  echo "Unable to detect distribution."
  exit 1
fi

parse_common_args "$@"

# Install Gogh Color theme
bash -c "$(wget -qO- https://raw.githubusercontent.com/Gogh-Co/Gogh/master/apply-colors.sh)"

# Function to install packages
install_packages() {
  echo "Installing packages..."
  sudo pacman -S --needed "$@"
}

install_packages \
  age alsa-lib aspell aspell-en aspell-ru automake base-devel bat bibtool bison \
  btop bun bzip2 ca-certificates cava chafa clang cmake cscope curl dgop dust \
  emacs-wayland enchant enchant exo expat fasd fd ffmpeg file-roller fish \
  fontconfig freeglut freetype2 fuse3 fzy gcc gcc gettext ghostty giflib git \
  github-desktop glances global gmime3 gnupg gnutls gtk4 guile gvfs helix hexyl \
  hspell htop httpie hunspell hyprpolkitagent imagemagick iotop iputils jansson \
  jbig2dec jq keychain kitty lazygit leptonica libevent libffi libgccjit \
  libjpeg-turbo libmupdf libpng libtiff libvoikko libvterm libxcb libxcomposite \
  libxcursor libxfixes libxi libxkbcommon libxmu libxpm llvm lynx make mc meson \
  mosh mujs mupdf mupdf-tools ncdu ncurses neovim net-tools nnn notmuch nuspell \
  openblas openconnect openssh openssl p7zip pandoc parallel pass pdfgrep pdfpc \
  pdftk peco pinentry poppler poppler-glib powerline-fonts procs python \
  python-pip python-pipx python-pyopenssl qps ranger readline ripgrep ruby \
  screen sdl2 shellcheck sndio sqlite sqlite stow systemd texinfo \
  the_silver_searcher thunar thunar-archive-plugin tig tk tmux trash-cli \
  tree-sitter tree-sitter ttf-fira-code ttf-jetbrains-mono unrar vale webkit2gtk \
  wget wmctrl xapian-core xdg-desktop-portal-hyprland xdg-utils xfce4-settings \
  xh xz zathura zathura-djvu zathura-pdf-poppler zed zellij zenity zlib-ng \
  zoxide

install_packages \
  git git-annex git-crypt git-delta git-lfs github-cli

yay -S --needed \
  autojump dsearch-bin fpp-git fswatch git-hub git-secrets git-secrets \
  gitflow-avh hdrop-git insync mu noisetorch teams-for-linux \
  visual-studio-code-bin xremap-hypr-bin

# Install GUI packages
if $GUI; then
  echo "Installing packages for Wayland..."
  install_packages obsidian slack-desktop languagetool discord logseq-desktop-bin
fi

# Install Node.js
if $INSTALL_NODE; then
  echo "Installing Node.js..."
  install_packages nodejs npm
  "$REPO_DIR/setup/install_node_packages.sh"
fi

if $INSTALL_PYTHON; then
  echo "Installing uv and plugins..."
  pipx install uv

  "$REPO_DIR/setup/install_python_tools.sh"
fi

if $INSTALL_FONTS; then
  install_font_packages "$TOOLS_DIR"
fi

# Install Rust and cargo packages
if $INSTALL_RUST; then
  echo "Installing Rust and cargo packages..."
  install_packages rustup
  rustup default stable
  source "$HOME/.cargo/env"
  "$REPO_DIR/setup/install_rust_packages.sh"

  # for kanata
  sudo groupadd uinput
  sudo usermod -aG input "$USER"
  sudo usermod -aG uinput "$USER"

  if $GUI; then
    cargo install --git https://github.com/neovide/neovide
  fi
fi

# Install go packages
if $INSTALL_GO; then
  echo "Installing go and go packages..."
  install_packages go
  "$REPO_DIR/setup/install_go_packages.sh"
fi

# Install lua packages
if $INSTALL_LUA; then
  echo "Installing luarocks and tiktoken_core..."
  install_packages luarocks
  luarocks install --local tiktoken_core
fi

if $INSTALL_EMACS; then
  install_spacemacs
fi

if $GUI; then
  install_intellimacs
fi

if $INSTALL_OLLAMA; then
  install_ollama
fi

install_fzf
install_omf

echo "Setup complete!"
