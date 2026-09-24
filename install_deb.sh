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

# Detect WSL
WSL=false
grep -qi microsoft /proc/version && WSL=true

# Keep Windows binaries off PATH (takes effect after `wsl --shutdown`)
if $WSL; then
  grep -q appendWindowsPath /etc/wsl.conf 2>/dev/null || printf '\n[interop]\nappendWindowsPath=false\n' | sudo tee -a /etc/wsl.conf >/dev/null
  PATH=$(tr ':' '\n' <<<"$PATH" | grep -v '^/mnt/' | paste -sd:)
fi

parse_common_args "$@"

# Install Gogh Color theme
$WSL || bash -c "$(wget -qO- https://git.io/vQgMr)"

# Function to install packages
install_packages() {
  echo "Installing packages..."
  sudo DEBIAN_FRONTEND=noninteractive apt-fast install -y "$@"
}

# Function to add PPA and install packages
add_ppa_and_install() {
  local ppa=$1
  shift
  sudo add-apt-repository "ppa:$ppa" -y
  sudo apt-fast update
  install_packages "$@"
}

# Install apt-fast
sudo add-apt-repository ppa:apt-fast/stable -y
sudo apt-get update
sudo apt-get install -y apt-fast

install_packages \
  apt-file autojump automake bat bison btop build-essential ca-certificates \
  checkinstall clang cmake cscope curl davmail fasd fd-find ffmpeg \
  ffmpegthumbnailer freeglut3-dev fswatch fzy g++-multilib gawk \
  gcc-multilib gettext glances global gnome-epub-thumbnailer gnupg htop \
  iotop iputils-arping jq kitty libadwaita-1-dev libasound2-dev libbz2-dev \
  libcld2-dev libenchant-2-dev libevent-dev libexpat1-dev libffi-dev \
  libfontconfig1-dev libfreetype6-dev libfuse-dev libgccjit-13-dev libgccjit0 \
  libgif-dev libgmime-3.0-dev libgnutls28-dev libgtk-4-dev libgumbo-dev \
  libjansson-dev libjansson4 libjbig2dec0-dev libjpeg-dev libleptonica-dev \
  liblzma-dev libmagick++-dev libmagickcore-dev libmujs-dev libmupdf-dev \
  libncurses-dev libncurses6 libncursesw6 libopenblas-dev libpng-dev \
  libpoppler-glib-dev libpoppler-private-dev libreadline-dev libsdl2-dev \
  libsndio-dev libsqlite3-dev libssl-dev libsystemd-dev libtiff-dev \
  libtree-sitter-dev libvterm-dev libwebkit2gtk-4.1-dev libxapian-dev \
  libxcb-composite0-dev libxcb-xfixes0-dev libxcursor-dev libxi-dev \
  libxkbcommon-dev libxmu-dev libxpm-dev llvm lynx make mc meson mosh ncdu \
  net-tools nnn openconnect openssh-server 7zip 7zip-rar pandoc \
  parallel pass pdfgrep pdfpc peco pipx pkg-config plocate powertop \
  protobuf-compiler pydf python-dev-is-python3 python3 python3-openssl \
  python3-pip ranger ripgrep ruby ruby-dev screen shellcheck \
  silversearcher-ag sqlite3 stow texinfo tig tk-dev tmux tmuxinator trash-cli \
  ubuntu-restricted-extras unrar wget wmctrl xdg-utils xz-utils zlib1g-dev \
  zoxide

install_packages \
  fonts-firacode fonts-jetbrains-mono fonts-powerline

# Ensure fd/bat commands are available under expected names.
command -v fdfind &>/dev/null && sudo ln -sf "$(command -v fdfind)" /usr/local/bin/fd
command -v batcat &>/dev/null && sudo ln -sf "$(command -v batcat)" /usr/local/bin/bat

# install git tools
add_ppa_and_install git-core/ppa \
  gh git git-annex git-crypt git-flow git-hub git-lfs git-secrets
add_ppa_and_install fish-shell/release-3 fish

# Install TLP for battery management and postfix (non-WSL only)
if ! $WSL; then
  add_ppa_and_install linrunner/tlp tlp
  install_packages postfix
fi

# Install GUI packages
if $GUI; then
  echo "Installing packages for X11..."
  install_packages \
    alacritty anki bibtool ditaa fbreader gnome-tweaks graphviz html2text isync \
    notmuch plantuml tabbed xdotool

  if ! $WSL; then
    install_packages \
      chrome-gnome-shell gnome-shell-extensions \
      network-manager-openconnect network-manager-openconnect-gnome \
      zathura zathura-djvu zathura-pdf-poppler
  fi

  # Install Ruby gems
  sudo gem install anystyle anystyle-cli
fi

# Install Node.js
if $INSTALL_NODE; then
  echo "Installing Node.js..."
  curl -sL https://deb.nodesource.com/setup_22.x | sudo -E bash -
  install_packages nodejs
  "$REPO_DIR/setup/install_node_packages.sh"
fi

if $INSTALL_PYTHON; then
  echo "Installing uv and plugins..."
  pipx install uv

  "$REPO_DIR/setup/install_python_tools.sh"
fi

if $INSTALL_FONTS && ! $WSL; then
  install_font_packages "$TOOLS_DIR"
fi

# Install tmux from source
if [ ! -d "/usr/local/stow/tmux" ]; then
  echo "Installing tmux..."
  curl -s https://api.github.com/repos/tmux/tmux/releases/latest | jq -r ".assets[] | select(.name | endswith(\".tar.gz\")).browser_download_url" | wget -O "/tmp/tmux.tar.gz" -i -
  mkdir -p /tmp/tmux
  tar -xzf "/tmp/tmux.tar.gz" -C "/tmp/tmux" --strip-components=1
  (cd /tmp/tmux && ./configure && make -j "$(nproc)" && sudo make install prefix=/usr/local/stow/tmux)
  (cd /usr/local/stow && sudo stow -S tmux)
  rm -rf /tmp/tmux /tmp/tmux.tar.gz
fi

# Install neovim
if [ ! -d "/usr/local/stow/nvim" ]; then
  echo "Installing neovim..."
  curl -s https://api.github.com/repos/neovim/neovim/releases/latest | jq -r ".assets[] | select(.name | endswith(\"-linux-x86_64.tar.gz\")).browser_download_url" | wget -O "/tmp/nvim.tar.gz" -i -
  sudo mkdir -p /usr/local/stow/nvim
  sudo tar -xzf "/tmp/nvim.tar.gz" -C "/usr/local/stow/nvim" --strip-components=1
  (cd /usr/local/stow && sudo stow -S nvim)
  rm -f /tmp/nvim.tar.gz
fi

# Install .deb releases from GitHub
install_github_deb() {
  local cmd=$1 repo=$2 filter=$3
  if ! command -v "$cmd" &>/dev/null; then
    echo "Installing $cmd..."
    curl -s "https://api.github.com/repos/$repo/releases/latest" | jq -r ".assets[] | select($filter).browser_download_url" | wget -O "/tmp/$cmd.deb" -i -
    sudo dpkg -i "/tmp/$cmd.deb"
    rm "/tmp/$cmd.deb"
  fi
}
install_github_deb delta dandavison/delta '.name | endswith("amd64.deb") and contains("musl")'
install_github_deb fastfetch fastfetch-cli/fastfetch '.name | endswith("amd64.deb")'

# Install Rust and cargo packages
if $INSTALL_RUST; then
  echo "Installing Rust and cargo packages..."
  if ! command -v rustup &>/dev/null; then
    curl https://sh.rustup.rs -sSf | sh -s -- -y
  fi
  source "$HOME/.cargo/env"
  rustup default stable
  "$REPO_DIR/setup/install_rust_packages.sh"

  # for kanata
  if ! $WSL; then
    sudo groupadd -f uinput
    sudo usermod -aG input,uinput "$USER"
  fi

  if $GUI; then
    cargo install --git https://github.com/neovide/neovide
  fi
fi

# Install go packages
if $INSTALL_GO; then
  add_ppa_and_install longsleep/golang-backports golang-go
  echo "Installing go and go packages..."
  "$REPO_DIR/setup/install_go_packages.sh"
fi

# Install lua packages
if $INSTALL_LUA; then
  echo "Installing luarocks and tiktoken_core..."
  install_packages luarocks
  luarocks install --local tiktoken_core
fi

if $INSTALL_EMACS; then
  install_emacs
fi

if $GUI; then
  install_intellimacs
fi

if $INSTALL_OLLAMA; then
  install_ollama
fi

install_fzf

# Install pathpicker
if ! command -v fpp &>/dev/null; then
  echo "Installing pathpicker..."
  git clone --depth=1 https://github.com/facebook/PathPicker.git /tmp/PathPicker
  (cd /tmp/PathPicker/debian && ./package.sh && sudo dpkg -i ../*.deb)
  rm -rf /tmp/PathPicker
fi

# Install NoiseTorch (GUI and non-WSL only)
if $GUI && ! $WSL && ! command -v noisetorch &>/dev/null; then
  echo "Installing NoiseTorch..."
  git clone --depth=1 https://github.com/noisetorch/NoiseTorch /tmp/NoiseTorch
  (cd /tmp/NoiseTorch && make -j "$(nproc)")
  mkdir -p ~/.local/bin ~/.local/share/applications ~/.local/share/icons/hicolor/256x256/apps
  cp /tmp/NoiseTorch/bin/noisetorch ~/.local/bin/
  cp /tmp/NoiseTorch/assets/noisetorch.desktop ~/.local/share/applications
  cp /tmp/NoiseTorch/assets/icon/noisetorch.png ~/.local/share/icons/hicolor/256x256/apps
  rm -rf /tmp/NoiseTorch
fi

# Install snap packages
if command -v snap &>/dev/null; then
  echo "Installing snap packages..."
  sudo snap refresh
  sudo snap install vale dust
  sudo snap install --classic helix zellij
  if $GUI && ! $WSL; then
    sudo snap install --classic obsidian slack
    sudo snap install discord languagetool logseq spotify
  fi
fi

# Update shell completions
"$REPO_DIR/setup/install_shell_completions.sh"

echo "Setup complete!"
