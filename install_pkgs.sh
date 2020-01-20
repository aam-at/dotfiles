#!/bin/sh

sudo apt-get install -y autojump automake build-essential checkinstall cmake \
     cscope curl fasd fish git libbz2-dev libevent1-dev libffi-dev \
     libgmime-3.0-dev liblzma-dev libncurses5-dev libncursesw5-dev libpng-dev \
     libpoppler-glib-dev libpoppler-private-dev libreadline-dev \
     libsqlite3-dev libssl-dev libxapian-dev llvm make python-openssl texinfo \
     tig tk-dev tmux wget xz-utils zlib1g-dev zlib1g-dev

sudo add-apt-repository ppa:git-core/ppa -y
sudo add-apt-repository ppa:apt-fast/stable
sudo add-apt-repository ppa:neovim-ppa/stable -y
sudo add-apt-repository ppa:fish-shell/release-3 -y

sudo apt-get isntall git apt-fast neovim fish

# compile and install tmux
mkdir ~/local/tools
wget https://github.com/tmux/tmux/archive/3.0a.tar.gz -P ~/local/tools

# install pyenv
git clone https://github.com/pyenv/pyenv.git ~/.pyenv
git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
git clone git://github.com/pyenv/pyenv-doctor.git $(pyenv root)/plugins/pyenv-doctor
git clone git://github.com/pyenv/pyenv-update.git $(pyenv root)/plugins/pyenv-update

CONFIGURE_OPTS=--enable-shared pyenv install 2.7.9
CONFIGURE_OPTS=--enable-shared pyenv install 3.8.1

# configure neovim
pyenv virtualenv 2.7.9 neovim2
pyenv activate neovim2
pip install pynvim

pyenv virtualenv 3.8.1 neovim3
pyenv activate neovim3
pip install pynvim

# install spacevim
curl -sLf https://spacevim.org/install.sh | bash

# configure fish
curl -L https://get.oh-my.fish | fish
