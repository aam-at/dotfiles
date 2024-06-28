#!/usr/bin/env bash

# Default value for GUI option
GUI=${GUI:-0}
TOOLS_DIR=${TOOLS_DIR:-"$HOME/local/tools"}

# Parse command line arguments
for arg in "$@"; do
    case $arg in
    --gui=*)
        GUI="${arg#*=}"
        shift # Remove argument from processing
        ;;
    *)
        echo "Unknown option: $arg"
        exit 1
        ;;
    esac
done

# Source .bashrc
source ~/.bashrc

# Install apt-fast first
sudo add-apt-repository ppa:apt-fast/stable -y
sudo apt-get update
sudo apt-get install -y apt-fast

# Install other packages
echo "Installing packages..."
sudo apt-fast install -y \
    apt-file autojump automake bison btop build-essential checkinstall clang \
    cmake cscope curl eza fasd fd-find ffmpeg fish fonts-firacode \
    fonts-jetbrains-mono fzy gawk gcc-10 gettext git git-lfs glances global \
    gnupg2 golang-go gpustat guile-3.0-dev htop iotop iputils-arping jq keychain \
    kitty libbz2-dev libevent-dev libffi-dev libfontconfig1-dev libfreetype6-dev \
    libfuse-dev libgccjit-14-dev libgccjit0 libgif-dev libgmime-3.0-dev \
    libgnutls28-dev libjansson-dev libjansson4 libjpeg-dev liblzma-dev \
    libmagick++-dev libmagickcore-dev libncurses-dev libncurses6 libncursesw6 \
    libopenblas-dev libpng-dev libpoppler-glib-dev libpoppler-private-dev \
    libreadline-dev libsqlite3-dev libssl-dev libsystemd-dev libtiff-dev \
    libtree-sitter-dev libwebkit2gtk-4.1-dev libxapian-dev libxcb-xfixes0-dev \
    libxkbcommon-dev libxpm-dev llvm make mc meson mosh ncdu net-tools nnn \
    nvitop openconnect openssh-server p7zip-full p7zip-rar pandoc pass pdfpc \
    peco pkg-config postfix protobuf-compiler pydf python-dev-is-python3 python3 \
    python3-openssl python3-pip ranger ripgrep ruby ruby-dev screen shellcheck \
    silversearcher-ag sqlite3 stow texinfo tig tk-dev tmux tmuxinator trash-cli \
    ubuntu-restricted-extras unrar wget wmctrl xdg-utils xz-utils zlib1g-dev \
    zoxide

# Add repositories
sudo add-apt-repository ppa:git-core/ppa -y
sudo add-apt-repository ppa:neovim-ppa/stable -y
sudo add-apt-repository ppa:fish-shell/release-3 -y
sudo add-apt-repository ppa:linrunner/tlp -y

# Install packages from added repositories
sudo apt-fast update
sudo apt-fast install -y git neovim fish tlp

# Install Node.js
curl -sL https://deb.nodesource.com/setup_20.x | sudo -E bash -
sudo apt-get install -y nodejs
# force npm upgrade
sudo npm i -g npm
# bash lsp
sudo npm i -g bash-language-server
# json lsp
sudo npm i -g vscode-json-languageserver prettier
# json beautify
sudo npm i -g js-beautify
# vim lsp
sudo npm i -g vim-language-server
# typescript
sudo npm i -g typescript tslint
sudo npm i -g typescript-language-server
sudo npm i -g typescript-formatter

# packages that require GUI/X interface
if [ $GUI -eq 1 ]; then
    echo "Installing packages for X11..."
    sudo apt-fast install -y \
        alacritty anki bibtool chrome-gnome-shell ditaa fbreader gnome-tweaks \
        graphviz html2text isync network-manager-openconnect \
        network-manager-openconnect-gnome notmuch plantuml tabbed xbindkeys xdotool \
        zathura zathura-djvu zathura-pdf-poppler

    # Install Ruby gems
    sudo gem install anystyle anystyle-cli

    # text linting
    sudo npm i -g textlint
    sudo npm i -g write-good textlint-plugin-latex textlint-rule-write-good \
        textlint-rule-no-start-duplicated-conjunction textlint-rule-max-comma \
        textlint-rule-terminology textlint-rule-period-in-list-item \
        textlint-rule-unexpanded-acronym textlint-rule-abbr-within-parentheses \
        textlint-rule-alex textlint-rule-common-misspellings \
        textlint-rule-en-max-word-count textlint-rule-diacritics \
        textlint-rule-stop-words
fi

# create tools directory
mkdir -p $TOOLS_DIR

# Install tmux
if ! command -v tmux &>/dev/null; then
    echo "Installing tmux..."
    curl -s https://api.github.com/repos/tmux/tmux/releases/latest | jq -r ".assets[] | select(.name | endswith(\".tar.gz\")).browser_download_url" | wget -O $TOOLS_DIR/tmux.tar.gz -i -
    mkdir $TOOLS_DIR/tmux
    tar -xzvf $TOOLS_DIR/tmux.tar.gz -C $TOOLS_DIR/tmux --strip-components=1
    cd $TOOLS_DIR/tmux
    ./configure
    make -j $(nproc)
    sudo checkinstall
    cd -
fi

# Install pyenv and plugins
if [ ! -d "$HOME/.pyenv" ]; then
    echo "Installing pyenv and plugins..."
    git clone https://github.com/pyenv/pyenv.git ~/.pyenv
    git clone https://github.com/pyenv/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv
    git clone https://github.com/pyenv/pyenv-pip-migrate.git ~/.pyenv/plugins/pyenv-pip-migrate
    git clone https://github.com/pyenv/pyenv-doctor.git ~/.pyenv/plugins/pyenv-doctor
    git clone https://github.com/pyenv/pyenv-update.git ~/.pyenv/plugins/pyenv-update
fi

# Install delta
if ! command -v delta &>/dev/null; then
    echo "Installing delta..."
    curl -s https://api.github.com/repos/dandavison/delta/releases/latest | jq -r ".assets[] | select(.name | endswith(\"amd64.deb\") and contains(\"musl\")).browser_download_url" | wget -O /tmp/delta.deb -i -
    sudo dpkg -i /tmp/delta.deb
    rm /tmp/delta.deb
fi

# Install nerd-fonts
if [ ! -d "$TOOLS_DIR/nerd-fonts" ]; then
    echo "Installing nerd-fonts..."
    git clone --depth=1 https://github.com/ryanoasis/nerd-fonts $TOOLS_DIR/nerd-fonts
    cd $TOOLS_DIR/nerd-fonts
    ./install.sh
    cd -
fi

if ! fc-list | grep -qi "JetBrains Mono"; then
    echo "Installing JetBrains Mono..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/JetBrains/JetBrainsMono/master/install_manual.sh)"
fi

# Install Python 3.11.9 with pyenv and set up virtual environments
if [ ! -d "$HOME/.pyenv/versions/3.11.9" ]; then
    echo "Installing Python 3.11.9 with pyenv..."
    CONFIGURE_OPTS=--enable-shared pyenv install 3.11.9

    pyenv virtualenv 3.11.9 neovim3
    pyenv activate neovim3
    pip install pynvim

    pyenv virtualenv 3.10.11 tensor3
    pyenv activate tensor3
    pip install -U "jedi>=0.13.0" "json-rpc>=1.8.1" "service_factory>=0.1.5"
    pip install -U python-language-server[all] pyls-isort pyls-mypy pyls-black pyls-memestra
    pip install -U pudb flake8 pylint yapf autoflake isort autopep8 "ptvsd>=4.2" importmagic epc proselint cmake-language-server

    pyenv deactivate
    python3 -m pip install --user pipx
    python3 -m pipx ensurepath
fi

# Install Rust and cargo packages
if ! command -v cargo &>/dev/null; then
    echo "Installing Rust and cargo..."
    curl https://sh.rustup.rs -sSf | sh -s -- -y
    source $HOME/.cargo/env
fi
cargo install atuin bottom gitui lsd ouch tealdeer texlab yazi-cli yazi-fm

# Install Spacemacs
if [ ! -d "$HOME/.emacs.d" ]; then
    echo "Installing Spacemacs..."
    git clone https://github.com/aam-at/spacemacs ~/.emacs.d
fi

# Install SpaceVim
if [ ! -d "$HOME/.SpaceVim" ]; then
    echo "Installing SpaceVim..."
    curl -sLf https://spacevim.org/install.sh | bash
fi

# Install Intellimacs
if [ $GUI -eq 1 ] && [ ! -d "$HOME/.intellimacs" ]; then
    echo "Installing Intellimacs..."
    git clone https://github.com/MarcoIeni/intellimacs ~/.intellimacs
fi

# Install pathpicker
if ! command -v fpp &>/dev/null; then
    echo "Installing pathpicker..."
    git clone https://github.com/facebook/PathPicker.git /tmp/PathPicker
    cd /tmp/PathPicker/debian
    ./package.sh
    cd ..
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
    git clone https://github.com/sebastiencs/icons-in-terminal /tmp/icons-in-terminal
    cd /tmp/icons-in-terminal
    ./install.sh
    rm -rf /tmp/icons-in-terminal
    cd -
fi

# Install NoiseTorch
if [ $GUI -eq 1 ] && ! command -v noisetorch &>/dev/null; then
    echo "Installing NoiseTorch..."
    git clone https://github.com/noisetorch/NoiseTorch /tmp/NoiseTorch
    cd /tmp/NoiseTorch
    make -j
    mkdir -p ~/.local/bin
    cp ./bin/noisetorch ~/.local/bin/
    cp ./assets/noisetorch.desktop ~/.local/share/applications
    cp ./assets/icon/noisetorch.png ~/.local/share/icons/hicolor/256x256/apps
    rm -rf /tmp/NoiseTorch
    cd -
fi

# Install ollama
if ! command -v ollama &>/dev/null; then
    echo "Installing ollama..."
    curl -fsSL https://ollama.com/install.sh | sh
fi

# Install snap packages
echo "Installing snap packages..."
sudo snap refresh
sudo snap install --classic helix
sudo snap install --classic zellij
if [ $GUI -eq 1 ]; then
    # sudo snap install --classic code
    sudo snap install --classic obsidian
    sudo snap install --classic pycharm-professional
    sudo snap install --classic skype
    sudo snap install --classic slack
    sudo snap install discord
    sudo snap install logseq
    sudo snap install opera
    sudo snap install spotify
fi

# Restore to initial directory
cd ~/dotfiles
