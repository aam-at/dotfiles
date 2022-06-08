#!/usr/bin/env bash

. ~/.bashrc

# install apt-fast first
sudo add-apt-repository ppa:apt-fast/stable -y
sudo apt-get install apt-fast

# install other packages
sudo apt-fast install -y \
    anki autojump automake bibtool build-essential checkinstall chrome-gnome-shell \
    clang cmake cscope curl curl ditaa fasd fbreader fd-find fish fzy gcc-10 git \
    git-lfs glances global gnome-tweaks graphviz guake guile-3.0-dev html2text htop \
    iotop iputils-arping isync jq keychain libbz2-dev libevent-dev libffi-dev \
    libgccjit-10-dev libgccjit0 libgif-dev libgmime-3.0-dev libgnutls28-dev \
    libjansson-dev libjansson4 libjpeg-dev liblzma-dev libmagick++-dev \
    libmagickcore-dev libncurses5-dev libncursesw5 libncursesw5-dev libpng-dev \
    libpoppler-glib-dev libpoppler-private-dev libreadline-dev libsqlite3-dev \
    libssl-dev libsystemd-dev libtiff-dev libwebkit2gtk-4.0-dev libxapian-dev \
    libxpm-dev llvm make mc mosh mu ncdu net-tools nnn notmuch openssh-server \
    p7zip-full p7zip-rar pandoc pass peco plantuml postfix pydf python3-openssl \
    ranger ripgrep rtv rtv screen shellcheck sqlite3 stow texinfo tig tk-dev tmux \
    trash-cli ubuntu-restricted-extras unrar wget wmctrl xdg-utils xz-utils zathura \
    zathura-djvu zathura-pdf-poppler zlib1g-dev

sudo add-apt-repository ppa:git-core/ppa -y
sudo add-apt-repository ppa:neovim-ppa/stable -y
sudo add-apt-repository ppa:fish-shell/release-3 -y
sudo add-apt-repository ppa:linrunner/tlp -y

sudo apt-fast install -y git neovim fish tlp

sudo gem install tmuxinator
sudo gem install anystyle-cli

# install nodejs
curl -sL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt-get install -y nodejs
# force npm upgrade
sudo npm i -g npm
# bash lsp
sudo npm i -g bash-language-server
# json lsp
sudo npm i -g vscode-json-languageserver prettier
# vim lsp
sudo npm i -g vim-language-server
# text linting
sudo npm i -g textlint
sudo npm i -g write-good textlint-plugin-latex textlint-rule-write-good \
     textlint-rule-no-start-duplicated-conjunction textlint-rule-max-comma \
     textlint-rule-terminology textlint-rule-period-in-list-item \
     textlint-rule-unexpanded-acronym textlint-rule-abbr-within-parentheses \
     textlint-rule-alex textlint-rule-common-misspellings \
     textlint-rule-en-max-word-count textlint-rule-diacritics \
     textlint-rule-stop-words

TOOLS_DIR=$HOME/local/tools
mkdir -p $TOOLS_DIR

# install tmux
if ! [ -f "/usr/local/bin/tmux" ]; then
    curl -s https://api.github.com/repos/tmux/tmux/releases/latest | jq -r ".assets[] | select(.name) | .browser_download_url" | grep -E ".tar.gz" | wget -O $TOOLS_DIR/tmux.tar.gz -i -
    mkdir $TOOLS_DIR/tmux
    tar -xzvf $TOOLS_DIR/tmux.tar.gz -C $TOOLS_DIR/tmux --strip-components=1
    cd $TOOLS_DIR/tmux
    ./configure
    sudo checkinstall
    cd -
fi

# install pyenv
if [ ! -d $HOME/.pyenv ]; then
    git clone https://github.com/pyenv/pyenv.git ~/.pyenv
    # install pyenv plugins
    git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
    git clone git://github.com/pyenv/pyenv-pip-migrate.git $(pyenv root)/plugins/pyenv-pip-migrate
    git clone git://github.com/pyenv/pyenv-doctor.git $(pyenv root)/plugins/pyenv-doctor
    git clone git://github.com/pyenv/pyenv-update.git $(pyenv root)/plugins/pyenv-update
fi

# install delta
if ! [ -x "$(command -v delta)" ]; then
    curl -s https://api.github.com/repos/dandavison/delta/releases/latest | jq -r ".assets[] | select(.name | contains(\"amd64.deb\")) | .browser_download_url" | grep -E "musl" | wget -O /tmp/delta.deb -i -
    sudo dpkg -i /tmp/delta.deb
    rm /tmp/delta.deb
fi


if [ ! -d $HOME/.pyenv/versions/3.8.1 ]; then
    CONFIGURE_OPTS=--enable-shared pyenv install 3.8.1

    pyenv virtualenv 3.8.1 neovim3
    pyenv activate neovim3
    pip3 install pynvim

    # configure emacs
    pyenv virtualenv 3.8.1 tensor3
    pyenv activate tensor3
    pip3 install -U "jedi>=0.13.0" "json-rpc>=1.8.1" "service_factory>=0.1.5"
    pip3 install -U python-language-server[all] pyls-isort pyls-mypy pyls-black pyls-memestra
    pip3 install -U pudb
    pip3 install -U flake8 pylint yapf autoflake isort autopep8
    pip3 install -U "ptvsd>=4.2"
    pip3 install -U importmagic epc
    pip3 install -U proselint
    pip3 install -U cmake-language-server
fi

# install spacevim
if [ ! -d $HOME/.SpaceVim ]; then
    curl -sLf https://spacevim.org/install.sh | bash
fi

# install enhancd for bash
if [ ! -d $HOME/local/tools/enhancd-bash ]; then
    git clone https://github.com/b4b4r07/enhancd $TOOLS_DIR/enhancd-bash
fi

# install pathpicker
if ! [ -x "$(command -v fpp)" ]; then
    git clone https://github.com/facebook/PathPicker.git /tmp/PathPicker
    cd /tmp/PathPicker/debian
    ./package.sh
    cd ..
    sudo dpkg -i "$(ls *.deb)"
    cd ..
    rm -rf PathPicker
fi

# install fzf
if [ ! -d $HOME/.fzf ]; then
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    ~/.fzf/install
fi

# install omf
if [ ! -d $HOME/.config/omf ]; then
    curl -L https://get.oh-my.fish | fish
    omf install https://github.com/b4b4r07/enhancd
fi

# snap packages
sudo snap refresh
sudo snap install --classic skype
sudo snap install --classic slack
sudo snap install --classic pycharm-professional
sudo snap install --classic code
sudo snap install opera
sudo snap install spotify
sudo snap install tusk

# restore dir
cd ~/dotfiles
