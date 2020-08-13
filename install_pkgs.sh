#!/bin/sh

# install apt-fast first
sudo add-apt-repository ppa:apt-fast/stable -y
sudo apt-get install apt-fast

# install other packages
sudo apt-fast install -y \
    autojump automake bibtool build-essential checkinstall cmake cscope curl fasd fish \
    git gnome-tweak-tool guile-2.0-dev html2text isync libbz2-dev libevent1-dev \
    libffi-dev libgif-dev libgmime-3.0-dev libgnutls28-dev libjpeg-dev \
    liblzma-dev libncurses5-dev libncursesw5-dev libpng-dev libpoppler-glib-dev \
    libpoppler-private-dev libreadline-dev libsqlite3-dev libssl-dev libtiff-dev \
    libwebkit2gtk-4.0-dev libwebkitgtk-3.0-dev libxapian-dev libxpm-dev llvm \
    make mc ncdu nnn pass pydf python-openssl rtv rtv texinfo tig tk-dev wget \
    xdg-utils xz-utils zlib1g-dev zlib1g-dev wmctrl \
    libsystemd-dev libmagick++-dev libmagickcore-dev

sudo add-apt-repository ppa:git-core/ppa -y
sudo add-apt-repository ppa:neovim-ppa/stable -y
sudo add-apt-repository ppa:fish-shell/release-3 -y
sudo add-apt-repository ppa:linrunner/tlp -y

sudo apt-fast install -y git neovim fish tlp

# compile and install tmux
mkdir ~/local/tools
wget https://github.com/tmux/tmux/archive/3.0a.tar.gz -P ~/local/tools

# install pyenv
git clone https://github.com/pyenv/pyenv.git ~/.pyenv
git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
git clone git://github.com/pyenv/pyenv-doctor.git $(pyenv root)/plugins/pyenv-doctor
git clone git://github.com/pyenv/pyenv-update.git $(pyenv root)/plugins/pyenv-update

CONFIGURE_OPTS=--enable-shared pyenv install 2.7.17
CONFIGURE_OPTS=--enable-shared pyenv install 3.8.1

# configure neovim
pyenv virtualenv 2.7.17 neovim2
pyenv activate neovim2
pip install pynvim

pyenv virtualenv 3.8.1 neovim3
pyenv activate neovim3
pip install pynvim

# install spacevim
curl -sLf https://spacevim.org/install.sh | bash

# configure fish
curl -L https://get.oh-my.fish | fish

# snap packages
sudo snap install --classic skype
sudo snap install opera
sudo snap install spotify
sudo snap install tusk
