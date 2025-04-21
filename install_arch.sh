#!/usr/bin/env bash

# Default values
GUI=${GUI:-false}
TOOLS_DIR=${TOOLS_DIR:-"$HOME/local/tools"}
INSTALL_PYTHON=${INSTALL_PYTHON:-true}
INSTALL_RUST=${INSTALL_RUST:-true}
INSTALL_GO=${INSTALL_GO:-true}
INSTALL_LUA=${INSTALL_LUA:-true}
INSTALL_NODE=${INSTALL_NODE:-true}
INSTALL_EMACS=${INSTALL_EMACS:-true}
INSTALL_FONTS=${INSTALL_FONTS:-true}

# Detect WSL
if grep -qi microsoft /proc/version; then
  WSL=true
  echo "This script is running on WSL"
else
  WSL=false
  echo "This script is running on Ubuntu"
fi

# Parse command line arguments
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
    echo "Unknown option: $1"
    exit 1
    ;;
  esac
done

# Source .bashrc
source "$HOME/.bashrc"

# Install Gogh Color theme
bash -c "$(wget -qO- https://git.io/vQgMr)"

sudo pacman -S --needed \
  autojump automake bat bison btop base-devel ca-certificates \
  clang cmake cscope curl fasd fd ffmpeg fish \
  ttf-fira-code ttf-jetbrains-mono powerline-fonts \
  freeglut fzy gcc libgccjit gettext git glances \
  global gnupg guile htop iotop iputils jq keychain \
  kitty alsa-lib bzip2 enchant libevent expat libffi \
  fontconfig freetype2 fuse3 gcc giflib gmime3 \
  gnutls gtk4 jansson jbig2dec \
  mupdf \
  mupdf-tools \
  libmupdf \
  mujs \
  tree-sitter \
  pdfpc \
  libjpeg-turbo leptonica xz imagemagick \
  ncurses openblas libpng poppler poppler-glib \
  readline sdl2 sndio sqlite openssl systemd \
  libtiff tree-sitter libvterm webkit2gtk xapian-core \
  libxcb libxcomposite libxfixes libxcursor libxi libxkbcommon \
  libxmu libxpm llvm lynx make mc meson mosh ncdu net-tools \
  nnn openconnect openssh p7zip pandoc parallel pass \
  pdfgrep peco python-pip python python-pyopenssl \
  ranger ripgrep ruby screen shellcheck \
  the_silver_searcher sqlite stow texinfo tig tk tmux \
  trash-cli unrar wget wmctrl xdg-utils \
  xz zlib zoxide

sudo pacman -S --needed \
  github-cli git git-annex git-crypt git-crypt \
  git-flow git-hub git-lfs \
  git-secrets

sudo yay -S git-hub git-secrets

# Install Rust and cargo packages
if $INSTALL_RUST; then
  echo "Installing Rust and cargo packages..."
  sudo pacman -S --needed rustup
  source "$HOME/.cargo/env"
  cargo install --locked \
    aichat argc atuin bottom broot cargo-edit cargo-outdated eza gitui gping \
    kanata lsd ouch sd tealdeer texlab viu yazi-cli yazi-fm

  if $GUI; then
    cargo install --git https://github.com/neovide/neovide
  fi

  rustup component add rustfmt
  rustup component add clippy
  rustup component add rust-analyzer
fi

# Install go packages
if $INSTALL_GO; then
  echo "Installing go and go packages..."
  sudo pacman -S --needed go
  repos=("charmbracelet/freeze" "charmbracelet/glow" "charmbracelet/mods" "charmbracelet/vhs" "stefanlogue/meteor" "jesseduffield/lazydocker")
  for repo in "${repos[@]}"; do
    go install "github.com/${repo}@latest"
  done
fi

# Install lua packages
if $INSTALL_LUA; then
  echo "Installing luarocks and tiktoken_core..."
  sudo pacman -S --needed luarocks
  luarocks install --local tiktoken_core
fi

# Install Spacemacs
if $INSTALL_EMACS && [ ! -d "$HOME/.emacs.d" ]; then
  echo "Installing Spacemacs..."
  git clone https://github.com/aam-at/spacemacs ~/.emacs.d
fi

# Install Intellimacs
if $GUI && [ ! -d "$HOME/.intellimacs" ]; then
  echo "Installing Intellimacs..."
  git clone https://github.com/MarcoIeni/intellimacs ~/.intellimacs
fi

# Install pathpicker
if ! command -v fpp &>/dev/null; then
  echo "Installing pathpicker..."
  yay -S --needed fpp-git
fi

# Install fzf
if [ ! -d "$HOME/.fzf" ]; then
  echo "Installing fzf..."
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install --all
fi

# Install oh-my-fish (omf)
if [ ! -d "$HOME/.config/omf" ]; then
  echo "Installing oh-my-fish (omf)..."
  curl -L https://get.oh-my.fish | fish
fi
