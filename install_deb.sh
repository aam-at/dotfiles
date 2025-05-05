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

# Function to install packages
install_packages() {
  echo "Installing packages..."
  sudo apt-fast install -y "$@"
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

# Install basic packages
install_packages \
    apt-file autojump automake bat bison btop build-essential ca-certificates \
    checkinstall clang cmake cmake cscope curl davmail fasd fd-find ffmpeg \
    ffmpegthumbnailer fish fonts-firacode fonts-jetbrains-mono fonts-powerline \
    freeglut3-dev fzy g++-multilib gawk gcc-10 gcc-multilib gettext git glances \
    global gnome-epub-thumbnailer gnupg gnupg2 guile-3.0-dev htop iotop \
    iputils-arping jq keychain kitty libadwaita-1-dev libasound2-dev libbz2-dev \
    libbz2-dev libcld2-dev libenchant-2-dev libevent-dev libexpat1-dev \
    libffi-dev libfontconfig1-dev libfontconfig1-dev libfreetype6-dev \
    libfreetype6-dev libfuse-dev libgccjit-13-dev libgccjit0 libgif-dev \
    libgmime-3.0-dev libgnutls28-dev libgtk-4-dev libgumbo-dev libjansson-dev \
    libjansson4 libjbig2dec0-dev libjpeg-dev libleptonica-dev liblzma-dev \
    libmagick++-dev libmagickcore-dev libmujs-dev libmupdf-dev libncurses-dev \
    libncurses6 libncursesw6 libopenblas-dev libpng-dev libpoppler-glib-dev \
    libpoppler-private-dev libreadline-dev libsdl2-dev libsndio-dev \
    libsqlite3-dev libssl-dev libssl-dev libsystemd-dev libtiff-dev \
    libtree-sitter-dev libvterm-dev libwebkit2gtk-4.1-dev libxapian-dev \
    libxcb-composite0-dev libxcb-xfixes0-dev libxcursor-dev libxi-dev \
    libxkbcommon-dev libxmu-dev libxpm-dev llvm lynx make mc meson mosh ncdu \
    net-tools nnn openconnect openssh-server p7zip-full p7zip-rar pandoc \
    parallel pass pdfgrep pdfpc peco pipx pkg-config pkg-config plocate postfix \
    powertop protobuf-compiler pydf python-dev-is-python3 python3 \
    python3-openssl python3-pip ranger ripgrep ruby ruby-dev screen shellcheck \
    silversearcher-ag sqlite3 stow texinfo tig tk-dev tmux tmuxinator trash-cli \
    ubuntu-restricted-extras unrar wget wmctrl xdg-utils xz-utils zlib1g-dev \
    zoxide

# Add repositories and install upstream packages

add_ppa_and_install git-core/ppa \
  gh git git-annex git-crypt git-flow git-hub git-lfs \
  git-secrets
gh extension install github/gh-copilot # Install github copilot cli
add_ppa_and_install fish-shell/release-3 fish

# Install TLP for battery management (non-WSL only)
if ! $WSL; then
  add_ppa_and_install linrunner/tlp tlp
fi

# Install Node.js
if $INSTALL_NODE; then
  echo "Installing Node.js..."
  curl -sL https://deb.nodesource.com/setup_20.x | sudo -E bash -
  install_packages nodejs
  sudo npm i -g npm

  sudo npm i -g \
    bash-language-server js-beautify prettier tslint typescript \
    typescript-formatter typescript-language-server vim-language-server \
    vscode-json-languageserver
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

  # Install bibtex-tidy
  npm install -g bibtex-tidy

  # Install text linting tools
  sudo npm i -g textlint write-good textlint-plugin-latex textlint-rule-write-good \
    textlint-rule-no-start-duplicated-conjunction textlint-rule-max-comma \
    textlint-rule-terminology textlint-rule-period-in-list-item \
    textlint-rule-unexpanded-acronym textlint-rule-abbr-within-parentheses \
    textlint-rule-alex textlint-rule-common-misspellings \
    textlint-rule-en-max-word-count textlint-rule-diacritics \
    textlint-rule-stop-words
fi

# Create tools directory
mkdir -p "$TOOLS_DIR"

# Install tmux
if ! command -v tmux &>/dev/null || [ ! -d "/usr/local/stow/tmux" ]; then
  echo "Installing tmux..."
  curl -s https://api.github.com/repos/tmux/tmux/releases/latest | jq -r ".assets[] | select(.name | endswith(\".tar.gz\")).browser_download_url" | wget -O "/tmp/tmux.tar.gz" -i -
  mkdir -p /tmp/tmux
  tar -xzvf "/tmp/tmux.tar.gz" -C "/tmp/tmux" --strip-components=1
  cd "/tmp/tmux" || exit
  ./configure
  make -j "$(nproc)"
  sudo make install prefix=/usr/local/stow/tmux
  cd -- || exit
  cd /usr/local/stow || exit
  sudo stow -S tmux
  cd - || exit
fi

# Instal neovim
if ! command -v nvim &>/dev/null || [ ! -d "/usr/local/stow/nvim" ]; then
  echo "Installing neovim..."
  curl -s https://api.github.com/repos/neovim/neovim/releases/latest | jq -r ".assets[] | select(.name | endswith(\"-linux64.tar.gz\")).browser_download_url" | wget -O "/tmp/nvim.tar.gz" -i -
  sudo mkdir -p /usr/local/stow/nvim
  sudo tar -xzvf "/tmp/nvim.tar.gz" -C "/usr/local/stow/nvim" --strip-components=1
  cd -- || exit
  cd /usr/local/stow || exit
  sudo stow -S nvim
  cd - || exit
fi

# Install pyenv and plugins
if $INSTALL_PYTHON && [ ! -d "$HOME/.pyenv" ]; then
  echo "Installing pyenv and plugins..."
  git clone https://github.com/pyenv/pyenv.git ~/.pyenv
  git clone https://github.com/pyenv/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv
  git clone https://github.com/pyenv/pyenv-pip-migrate.git ~/.pyenv/plugins/pyenv-pip-migrate
  git clone https://github.com/pyenv/pyenv-doctor.git ~/.pyenv/plugins/pyenv-doctor
  git clone https://github.com/pyenv/pyenv-update.git ~/.pyenv/plugins/pyenv-update

  # Install Python 3.11.9 with pyenv and set up virtual environments
  CONFIGURE_OPTS=--enable-shared pyenv install 3.11.9

  pyenv virtualenv 3.11.9 neovim3
  pyenv activate neovim3
  pip install pynvim

  pyenv virtualenv 3.11.9 tensor3
  pyenv activate tensor3
  pip install -U "jedi>=0.13.0" "json-rpc>=1.8.1" "service_factory>=0.1.5"
  pip install -U "ptvsd>=4.2" epc importmagic mupy pudb
  pip install -U pylsp-mypy python-lsp-black python-lsp-ruff python-lsp-server[all]

  pyenv deactivate
  pipx install uv

  for tool in aider autoflake autopep8 basedpyright black \
    cmake-language-server docformatter flake9 \
    git+https://github.com/bcbernardo/aw-watcher-ask.git gpustat \
    isort marker-pdf nvitop poetry pre-commit proselint pylint ruff \
    semgrep yapf; do
    uv tool install "$tool"
  done
fi

# Install delta
if ! command -v delta &>/dev/null; then
  echo "Installing delta..."
  curl -s https://api.github.com/repos/dandavison/delta/releases/latest | jq -r ".assets[] | select(.name | endswith(\"amd64.deb\") and contains(\"musl\")).browser_download_url" | wget -O /tmp/delta.deb -i -
  sudo dpkg -i /tmp/delta.deb
  rm /tmp/delta.deb
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
  if ! command -v cargo &>/dev/null; then
    echo "Installing Rust and cargo..."
    curl https://sh.rustup.rs -sSf | sh -s -- -y
  fi
  echo "Installing Rust and cargo packages..."
  source "$HOME/.cargo/env"
  cargo install --locked \
    aichat argc atuin bottom broot cargo-edit cargo-outdated eza gitui gping \
    kanata lsd ouch sd tealdeer texlab viu yazi-cli yazi-fm
  cargo install --git https://github.com/blahgeek/emacs-lsp-booster

  if $GUI; then
    cargo install --git https://github.com/neovide/neovide
    if [ "$XDG_SESSION_TYPE" = "wayland" ]; then
      echo "Installing xremap for Wayland..."
      cargo install xremap --features gnome
    elif [ "$XDG_SESSION_TYPE" = "x11" ]; then
      echo "Installing xremap for X11..."
      cargo install xremap --features x11
    fi
    # configure xremap to use without sudo
    sudo gpasswd -a $USER input
    echo 'KERNEL=="uinput", GROUP="input", TAG+="uaccess"' | sudo tee /etc/udev/rules.d/input.rules
  fi

  rustup component add rustfmt
  rustup component add clippy
  rustup component add rust-analyzer
fi

# Install go packages
if $INSTALL_GO; then
  if ! command -v go &>/dev/null; then
    echo "Installing go..."
    add_ppa_and_install longsleep/golang-backports golang-go
  fi
  echo "Installing go packages..."
  repos=("charmbracelet/freeze" "charmbracelet/glow" "charmbracelet/mods" "charmbracelet/vhs" "stefanlogue/meteor" "jesseduffield/lazydocker")
  for repo in "${repos[@]}"; do
    go install "github.com/${repo}@latest"
  done
fi

# Install lua packages
if $INSTALL_LUA; then
  if ! command -v luarocks &>/dev/null; then
    echo "Installing luarocks..."
    install_packages luarocks
  fi
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
  git clone --depth=1 https://github.com/facebook/PathPicker.git /tmp/PathPicker
  cd /tmp/PathPicker/debian || exit
  ./package.sh
  cd .. || exit
  sudo dpkg -i *.deb
  cd ..
  rm -rf PathPicker
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

# Install icons-in-terminal
if [ ! -d "$HOME/.local/share/icons-in-terminal" ]; then
  echo "Installing icons-in-terminal..."
  git clone --depth=1 https://github.com/sebastiencs/icons-in-terminal /tmp/icons-in-terminal
  cd /tmp/icons-in-terminal || exit
  ./install.sh
  rm -rf /tmp/icons-in-terminal
  cd - || exit
fi

# Install NoiseTorch (GUI and non-WSL only)
if $GUI && ! $WSL && ! command -v noisetorch &>/dev/null; then
  echo "Installing NoiseTorch..."
  git clone --depth=1 https://github.com/noisetorch/NoiseTorch /tmp/NoiseTorch
  cd /tmp/NoiseTorch || exit
  make -j "$(nproc)"
  mkdir -p ~/.local/bin
  cp ./bin/noisetorch ~/.local/bin/
  cp ./assets/noisetorch.desktop ~/.local/share/applications
  cp ./assets/icon/noisetorch.png ~/.local/share/icons/hicolor/256x256/apps
  rm -rf /tmp/NoiseTorch
  cd - || exit
fi

# Install ollama
if ! command -v ollama &>/dev/null; then
  echo "Installing ollama..."
  curl -fsSL https://ollama.com/install.sh | sh
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

# Install tdrop
if ! command -v tdrop &>/dev/null; then
  echo "Installing tdrop..."
  temp_dir=$(mktemp -d)
  git clone --depth 1 https://github.com/noctuid/tdrop "$temp_dir"
  cd "$temp_dir" || exit
  sudo make install PREFIX=/usr/local/stow/tdrop
  cd /usr/local/stow || exit
  sudo stow -S tdrop
  cd - || exit
  rm -rf "$temp_dir"
  echo "tdrop installation complete."
fi

# Install snap packages (non-WSL only)
if command -v snap &>/dev/null; then
  echo "Installing snap packages..."
  sudo snap refresh
  sudo snap install vale
  sudo snap install dust
  sudo snap install --classic helix
  sudo snap install --classic zellij
  if $GUI && ! $WSL; then
    sudo snap install --classic obsidian
    sudo snap install --classic pycharm-professional
    sudo snap install --classic skype
    sudo snap install --classic slack
    sudo snap install languagetool
    sudo snap install discord
    sudo snap install logseq
    sudo snap install opera
    sudo snap install spotify
  fi
fi

echo "Setup complete!"
