#!/usr/bin/env bash

# Debian/Ubuntu specific package lists and installation functions

# Detect WSL
if grep -qi microsoft /proc/version; then
  WSL=true
  echo "This script is running on WSL"
else
  WSL=false
  echo "This script is running on Ubuntu"
fi

# Core packages for Debian/Ubuntu
DEB_CORE_PACKAGES=(
  apt-file autojump automake bat bison btop build-essential ca-certificates
  checkinstall clang cmake cmake cscope curl davmail fasd fd-find ffmpeg
  ffmpegthumbnailer fish fonts-firacode fonts-jetbrains-mono fonts-powerline
  freeglut3-dev fzy g++-multilib gawk gcc-10 gcc-multilib gettext git glances
  global gnome-epub-thumbnailer gnupg gnupg2 guile-3.0-dev htop iotop
  iputils-arping jq keychain kitty libadwaita-1-dev libasound2-dev libbz2-dev
  libbz2-dev libcld2-dev libenchant-2-dev libevent-dev libexpat1-dev
  libffi-dev libfontconfig1-dev libfontconfig1-dev libfreetype6-dev
  libfreetype6-dev libfuse-dev libgccjit-13-dev libgccjit0 libgif-dev
  libgmime-3.0-dev libgnutls28-dev libgtk-4-dev libgumbo-dev libjansson-dev
  libjansson4 libjbig2dec0-dev libjpeg-dev libleptonica-dev liblzma-dev
  libmagick++-dev libmagickcore-dev libmujs-dev libmupdf-dev libncurses-dev
  libncurses6 libncursesw6 libopenblas-dev libpng-dev libpoppler-glib-dev
  libpoppler-private-dev libreadline-dev libsdl2-dev libsndio-dev
  libsqlite3-dev libssl-dev libssl-dev libsystemd-dev libtiff-dev
  libtree-sitter-dev libvterm-dev libwebkit2gtk-4.1-dev libxapian-dev
  libxcb-composite0-dev libxcb-xfixes0-dev libxcursor-dev libxi-dev
  libxkbcommon-dev libxmu-dev libxpm-dev llvm lynx make mc meson mosh ncdu
  net-tools nnn openconnect openssh-server p7zip-full p7zip-rar pandoc
  parallel pass pdfgrep pdfpc peco pipx pkg-config pkg-config plocate postfix
  powertop protobuf-compiler pydf python-dev-is-python3 python3
  python3-openssl python3-pip ranger ripgrep ruby ruby-dev screen shellcheck
  silversearcher-ag sqlite3 stow texinfo tig tk-dev tmux tmuxinator trash-cli
  ubuntu-restricted-extras unrar wget wmctrl xdg-utils xz-utils zlib1g-dev
  zoxide
)

# GUI packages for Debian/Ubuntu (non-WSL)
DEB_GUI_PACKAGES=(
  alacritty anki bibtool ditaa fbreader gnome-tweaks graphviz html2text isync
  notmuch plantuml tabbed xdotool chrome-gnome-shell gnome-shell-extensions
  network-manager-openconnect network-manager-openconnect-gnome
  zathura zathura-djvu zathura-pdf-poppler
)

# GUI packages for Debian/Ubuntu (WSL)
DEB_GUI_WSL_PACKAGES=(
  alacritty anki bibtool ditaa fbreader gnome-tweaks graphviz html2text isync
  notmuch plantuml tabbed xdotool
)

# Snap packages for Debian/Ubuntu
DEB_SNAP_PACKAGES=(
  vale dust helix zellij
)

# GUI snap packages for Debian/Ubuntu (non-WSL)
DEB_GUI_SNAP_PACKAGES=(
  obsidian pycharm-professional skype slack languagetool discord logseq opera
  spotify
)

# Function to install packages using apt-fast
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

# Function to install apt-fast
install_apt_fast() {
  sudo add-apt-repository ppa:apt-fast/stable -y
  sudo apt-get update
  sudo apt-get install -y apt-fast
}

# Function to install core packages
install_core_packages() {
  install_apt_fast
  install_packages "${DEB_CORE_PACKAGES[@]}"
  
  # Ensure fd/bat commands are available under expected names
  local fd_path=$(command -v fdfind || true)
  local bat_path=$(command -v batcat || true)
  
  if [ -n "$fd_path" ]; then
    sudo ln -sf "$fd_path" /usr/local/bin/fd
  fi
  if [ -n "$bat_path" ]; then
    sudo ln -sf "$bat_path" /usr/local/bin/bat
  fi
  
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
}

# Function to install GUI packages
install_gui_packages() {
  if $GUI; then
    echo "Installing packages for X11..."
    if $WSL; then
      install_packages "${DEB_GUI_WSL_PACKAGES[@]}"
    else
      install_packages "${DEB_GUI_PACKAGES[@]}"
    fi
    
    # Install Ruby gems
    sudo gem install anystyle anystyle-cli
    
    # Install text linting tools
    sudo npm i -g textlint write-good textlint-plugin-latex textlint-rule-write-good \
      textlint-rule-no-start-duplicated-conjunction textlint-rule-max-comma \
      textlint-rule-terminology textlint-rule-period-in-list-item \
      textlint-rule-unexpanded-acronym textlint-rule-abbr-within-parentheses \
      textlint-rule-alex textlint-rule-common-misspellings \
      textlint-rule-en-max-word-count textlint-rule-diacritics \
      textlint-rule-stop-words
  fi
}

# Function to install Node.js
install_nodejs() {
  if $INSTALL_NODE; then
    echo "Installing Node.js..."
    curl -sL https://deb.nodesource.com/setup_22.x | sudo -E bash -
    install_packages nodejs
  fi
}

# Function to install Rust
install_rust() {
  if $INSTALL_RUST; then
    if ! command -v cargo &>/dev/null; then
      echo "Installing Rust and cargo..."
      curl https://sh.rustup.rs -sSf | sh -s -- -y
    fi
    echo "Installing Rust and cargo packages..."
    source "$HOME/.cargo/env"
    
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
  fi
}

# Function to install Go
install_go() {
  if $INSTALL_GO; then
    if ! command -v go &>/dev/null; then
      echo "Installing go..."
      add_ppa_and_install longsleep/golang-backports golang-go
    fi
    echo "Installing go packages..."
  fi
}

# Function to install Lua
install_lua() {
  if $INSTALL_LUA; then
    if ! command -v luarocks &>/dev/null; then
      echo "Installing luarocks..."
      install_packages luarocks
    fi
  fi
}

# Function to install snap packages
install_snap_packages() {
  if command -v snap &>/dev/null; then
    echo "Installing snap packages..."
    sudo snap refresh
    sudo snap install "${DEB_SNAP_PACKAGES[@]}"
    if $GUI && ! $WSL; then
      sudo snap install --classic "${DEB_GUI_SNAP_PACKAGES[@]}"
    fi
  fi
}

# Function to install additional tools
install_additional_tools() {
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

  # Install neovim
  if ! command -v nvim &>/dev/null || [ ! -d "/usr/local/stow/nvim" ]; then
    echo "Installing neovim..."
    curl -s https://api.github.com/repos/neovim/neovim/releases/latest | jq -r ".assets[] | select(.name | endswith(\"-linux-x86_64.tar.gz\")).browser_download_url" | wget -O "/tmp/nvim.tar.gz" -i -
    sudo mkdir -p /usr/local/stow/nvim
    sudo tar -xzvf "/tmp/nvim.tar.gz" -C "/usr/local/stow/nvim" --strip-components=1
    cd -- || exit
    cd /usr/local/stow || exit
    sudo stow -S nvim
    cd - || exit
    echo "✅ neovim installed successfully!"
  else
    echo "neovim already installed."
  fi

  # Install delta
  if ! command -v delta &>/dev/null; then
    echo "Installing delta..."
    curl -s https://api.github.com/repos/dandavison/delta/releases/latest | jq -r ".assets[] | select(.name | endswith(\"amd64.deb\") and contains(\"musl\")).browser_download_url" | wget -O /tmp/delta.deb -i -
    sudo dpkg -i /tmp/delta.deb
    rm /tmp/delta.deb
    echo "✅ delta installed successfully!"
  else
    echo "delta already installed."
  fi

  # Install fastfetch
  if ! command -v fastfetch &>/dev/null; then
    echo "Installing fastfetch-cli..."
    curl -s https://api.github.com/repos/fastfetch-cli/fastfetch/releases/latest | jq -r ".assets[] | select(.name | endswith(\"amd64.deb\")).browser_download_url" | wget -O /tmp/fastfetch.deb -i -
    sudo dpkg -i /tmp/fastfetch.deb
    rm /tmp/fastfetch.deb
    echo "✅ fastfetch-cli installed successfully!"
  else
    echo "fastfetch-cli already installed."
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
}

# Function to install pyenv and Python tools
install_pyenv() {
  if $INSTALL_PYTHON && [ ! -d "$HOME/.pyenv" ]; then
    echo "Installing pyenv and plugins..."
    git clone https://github.com/pyenv/pyenv.git ~/.pyenv
    git clone https://github.com/pyenv/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv
    git clone https://github.com/pyenv/pyenv-pip-migrate.git ~/.pyenv/plugins/pyenv-pip-migrate
    git clone https://github.com/pyenv/pyenv-doctor.git ~/.pyenv/plugins/pyenv-doctor
    git clone https://github.com/pyenv/pyenv-update.git ~/.pyenv/plugins/pyenv-update

    # Install Python 3.11.9 with pyenv and set up virtual environments
    CONFIGURE_OPTS=--enable-shared pyenv install 3.11.13

    pyenv virtualenv 3.11.13 neovim3
    pyenv activate neovim3
    pip install pynvim

    pyenv virtualenv 3.11.13 tensor3
    pyenv activate tensor3
    pip install -U "jedi>=0.13.0" "json-rpc>=1.8.1" "service_factory>=0.1.5"
    pip install -U "ptvsd>=4.2" epc importmagic mupy pudb
    pip install -U pylsp-mypy python-lsp-black python-lsp-ruff python-lsp-server[all]

    pyenv deactivate
  fi
}