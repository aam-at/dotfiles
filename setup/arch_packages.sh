#!/usr/bin/env bash

# Arch Linux specific package lists and installation functions

# Core packages for Arch Linux
ARCH_CORE_PACKAGES=(
  alsa-lib aspell aspell-en aspell-ru automake base-devel bat bibtool bison btop
  bzip2 ca-certificates chafa clang cmake cscope curl dust emacs-wayland enchant
  enchant expat fasd fd ffmpeg file-roller fish fontconfig freeglut freetype2
  fuse3 fzy gcc gcc gettext giflib git github-desktop glances global gmime3
  gnupg gnutls gtk4 guile helix hexyl hspell htop hunspell imagemagick iotop
  iputils jansson jbig2dec jq keychain kitty leptonica libevent libffi libgccjit
  libjpeg-turbo libmupdf libpng libtiff libvoikko libvterm libxcb libxcomposite
  libxcursor libxfixes libxi libxkbcommon libxmu libxpm llvm lynx make mc meson
  mosh mujs mupdf mupdf-tools ncdu ncurses neovim net-tools nnn nuspell openblas
  openconnect openssh openssl p7zip pandoc parallel pass pdfgrep pdfpc pdftk
  peco pinentry poppler poppler-glib powerline-fonts procs python python-pip
  python-pipx python-pyopenssl qps ranger readline ripgrep ruby screen sdl2
  shellcheck sndio sqlite sqlite stow systemd texinfo the_silver_searcher thunar
  thunar-archive-plugin tig tk tmux trash-cli tree-sitter tree-sitter
  ttf-fira-code ttf-jetbrains-mono unrar vale webkit2gtk wget wmctrl xdg-utils
  xz xz zathura zathura-djvu zathura-pdf-poppler zed zellij zenity
  zlib-ng zoxide
)

# Git packages for Arch Linux
ARCH_GIT_PACKAGES=(
  git git-annex git-crypt git-delta git-lfs github-cli
)

# AUR packages for Arch Linux
ARCH_AUR_PACKAGES=(
  autojump dropbox fpp-git git-hub git-secrets git-secrets gitflow-avh
  hdrop-git insync mu noisetorch teams-for-linux visual-studio-code-bin
)

# GUI packages for Arch Linux
ARCH_GUI_PACKAGES=(
  obsidian slack-desktop languagetool discord logseq-desktop-bin
)

# Function to install packages using pacman
install_packages() {
  echo "Installing packages..."
  sudo pacman -S --needed "$@"
}

# Function to install AUR packages using yay
install_aur_packages() {
  echo "Installing AUR packages..."
  yay -S --needed "$@"
}

# Function to install core packages
install_core_packages() {
  install_packages "${ARCH_CORE_PACKAGES[@]}"
  install_packages "${ARCH_GIT_PACKAGES[@]}"
  install_aur_packages "${ARCH_AUR_PACKAGES[@]}"
}

# Function to install GUI packages
install_gui_packages() {
  if $GUI; then
    echo "Installing packages for Wayland..."
    install_packages "${ARCH_GUI_PACKAGES[@]}"
  fi
}

# Function to install Node.js
install_nodejs() {
  if $INSTALL_NODE; then
    echo "Installing Node.js..."
    install_packages nodejs npm
  fi
}

# Function to install Rust
install_rust() {
  if $INSTALL_RUST; then
    echo "Installing Rust and cargo packages..."
    install_packages rustup
    rustup default stable
    source "$HOME/.cargo/env"
    
    # for kanata
    sudo groupadd uinput
    sudo usermod -aG input $USER
    sudo usermod -aG uinput $USER

    if $GUI; then
      cargo install --git https://github.com/neovide/neovide
    fi
  fi
}

# Function to install Go
install_go() {
  if $INSTALL_GO; then
    echo "Installing go and go packages..."
    install_packages go
  fi
}

# Function to install Lua
install_lua() {
  if $INSTALL_LUA; then
    echo "Installing luarocks and tiktoken_core..."
    install_packages luarocks
  fi
}