#!/usr/bin/env bash

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

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
  --gui)
    GUI=true
    shift
    ;;
  --ollama)
    INSTALL_OLLAMA=true
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
  alsa-lib autojump automake base-devel bat bison btop bzip2 ca-certificates \
  clang cmake cscope curl dust emacs-wayland enchant expat fasd fd ffmpeg fish \
  fontconfig freeglut freetype2 fuse3 fzy gcc gcc gettext giflib git glances \
  global gmime3 gnupg gnutls gtk4 guile helix hspell htop imagemagick iotop \
  iputils jansson jbig2dec jq keychain kitty leptonica libevent libffi libgccjit \
  libjpeg-turbo libmupdf libpng libtiff libvoikko libvterm libxcb libxcomposite \
  libxcursor libxfixes libxi libxkbcommon libxmu libxpm llvm lynx make mc meson \
  mosh mujs mupdf mupdf-tools ncdu ncurses neovim net-tools nnn nuspell openblas \
  openconnect openssh openssl p7zip pandoc parallel pass pdfgrep pdfpc peco \
  pinentry poppler poppler-glib powerline-fonts python python-pip python-pipx \
  python-pyopenssl ranger readline ripgrep ruby screen sdl2 shellcheck sndio \
  sqlite sqlite stow systemd texinfo the_silver_searcher tig tk tmux trash-cli \
  tree-sitter tree-sitter ttf-fira-code ttf-jetbrains-mono unrar vale webkit2gtk \
  wget wmctrl xapian-core xdg-utils xz xz zellij zlib zoxide

sudo pacman -S --needed \
  github-cli git git-annex git-crypt git-crypt \
  git-lfs

yay -S --needed git-hub git-secrets fpp-git mu noisetorch hdrop-git gitflow-avh git-secrets teams-for-linux

if $GUI; then
  echo "Installing packages for Wayland..."
  sudo pacman -S --needed obsidian slack-desktop languagetool discord logseq-desktop-bin
fi

if $INSTALL_PYTHON; then
  echo "Installing uv and plugins..."
  pipx install uv

  for tool in aider autoflake autopep8 basedpyright black \
    cmake-language-server docformatter flake9 \
    git+https://github.com/bcbernardo/aw-watcher-ask.git gpustat \
    isort marker-pdf nvitop poetry pre-commit proselint pylint ruff \
    semgrep yapf; do
    uv tool install "$tool"
  done
fi

if $INSTALL_FONTS; then
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
  font_packages=(
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
fi

# Install Rust and cargo packages
if $INSTALL_RUST; then
  echo "Installing Rust and cargo packages..."
  sudo pacman -S --needed rustup
  source "$HOME/.cargo/env"
  cargo install --locked \
    aichat argc atuin bottom broot cargo-edit cargo-outdated eza gitui gping \
    kanata lsd ouch sd tealdeer texlab viu yazi-cli yazi-fm

  # for kanata
  sudo groupadd uinput
  sudo usermod -aG input $USER
  sudo usermod -aG uinput $USER

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

# Install ollama
if $INSTALL_OLLAMA; then
  if ! command -v ollama &>/dev/null; then
    echo "Installing ollama..."
    curl -fsSL https://ollama.com/install.sh | sh
  fi
  echo "Downloading ollama models"
  ollama_models=(
    # coding
    "qwen2.5-coder:3b" "qwen2.5-coder:7b"
    # llm
    "gemma3:4b" "gemma3:12b" "phi4:mini"
    # embedding
    "granite-embedding:278m" "mxbai-embed-large:latest" "nomic-embed-text:latest")
  for ollama_model in "${ollama_models[@]}"; do
    ollama pull "$ollama_model"
  done
fi

# Install fzf
if [ ! -d "$HOME/.fzf" ]; then
  echo "Installing fzf..."
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install --all
fi

echo "Setup complete!"
