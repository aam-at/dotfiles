#!/usr/bin/env bash

. ~/.bashrc

# install apt-fast first
sudo add-apt-repository ppa:apt-fast/stable -y
sudo apt-get install apt-fast

# install other packages
sudo apt-fast install -y \
    anki autojump automake bibtool build-essential checkinstall clang cmake cscope \
    curl ditaa fasd fbreader fd-find fish fzy git glances gnome-tweak-tool graphviz \
    guile-2.0-dev html2text iotop isync libbz2-dev libevent-dev libffi-dev \
    libgif-dev libgmime-3.0-dev libgnutls28-dev libjpeg-dev liblzma-dev \
    libmagick++-dev libmagickcore-dev libncurses5-dev libncursesw5-dev libpng-dev \
    libpoppler-glib-dev libpoppler-private-dev libreadline-dev libsqlite3-dev \
    libssl-dev libsystemd-dev libtiff-dev libwebkit2gtk-4.0-dev \
    libxapian-dev libxpm-dev llvm make mc mosh ncdu net-tools nnn openssh-server \
    pass plantuml pydf python-openssl ripgrep rtv rtv shellcheck texinfo tig tk-dev \
    trash-cli wget wmctrl xdg-utils xz-utils zathura zathura-djvu \
    zathura-pdf-poppler zlib1g-dev zlib1g-dev

sudo add-apt-repository ppa:git-core/ppa -y
sudo add-apt-repository ppa:neovim-ppa/stable -y
sudo add-apt-repository ppa:fish-shell/release-3 -y
sudo add-apt-repository ppa:linrunner/tlp -y

sudo apt-fast install -y git neovim fish tlp

curl -sL https://deb.nodesource.com/setup_14.x | sudo -E bash -
sudo apt-get install -y nodejs
# bash lsp
sudo npm i -g bash-language-server
# json lsp
sudo npm i -g vscode-json-languageserver prettier
# vim lsp
sudo npm i -g vim-language-server

# install pyenv
if [ ! -d $HOME/.pyenv ]; then
    git clone https://github.com/pyenv/pyenv.git ~/.pyenv
    git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
    git clone git://github.com/pyenv/pyenv-doctor.git $(pyenv root)/plugins/pyenv-doctor
    git clone git://github.com/pyenv/pyenv-update.git $(pyenv root)/plugins/pyenv-update
fi

if [ ! -d $HOME/.pyenv/versions/2.7.17 ]; then
    CONFIGURE_OPTS=--enable-shared pyenv install 2.7.17
    # configure neovim
    pyenv virtualenv 2.7.17 neovim2
    pyenv activate neovim2
    pip2 install pynvim
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
    pip3 install -U python-language-server[all] pyls-isort pyls-black
    pip3 install -U pudb
    pip3 install -U flake8 pylint yapf autoflake isort autopep8
    pip3 install -U "ptvsd>=4.2"
    pip3 install -U importmagic epc
fi

# install spacevim
if [ ! -d $HOME/.SpaceVim ]; then
    curl -sLf https://spacevim.org/install.sh | bash
fi

mkdir -p ~/local/tools

# download tmux
if [ ! -f $HOME/local/tools/3.1b.tar.gz ]; then
    wget https://github.com/tmux/tmux/archive/3.1b.tar.gz -P ~/local/tools
fi

# download fish
if [ ! -f $HOME/local/tools/fish-3.1.2.tar.gz ]; then
    wget https://github.com/fish-shell/fish-shell/releases/download/3.1.2/fish-3.1.2.tar.gz -P ~/local/tools
fi

# install enhancd for bash
if [ ! -d $HOME/local/tools/enhancd-bash ]; then
    git clone https://github.com/b4b4r07/enhancd $HOME/local/tools/enhancd-bash
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
sudo snap install --classic skype
sudo snap install opera
sudo snap install spotify
sudo snap install tusk
