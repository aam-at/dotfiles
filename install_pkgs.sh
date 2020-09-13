#!/bin/sh

# install apt-fast first
sudo add-apt-repository ppa:apt-fast/stable -y
sudo apt-get install apt-fast

# install other packages
sudo apt-fast install -y \
     autojump automake bibtool build-essential checkinstall cmake cscope curl ditaa fasd \
     fbreader fish git gnome-tweak-tool guile-2.0-dev graphviz html2text isync libbz2-dev \
     libevent-dev libffi-dev libgif-dev libgmime-3.0-dev libgnutls28-dev libjpeg-dev \
     liblzma-dev libncurses5-dev libncursesw5-dev libpng-dev libpoppler-glib-dev \
     libpoppler-private-dev libreadline-dev libsqlite3-dev libssl-dev libtiff-dev \
     libwebkit2gtk-4.0-dev libwebkitgtk-3.0-dev libxapian-dev libxpm-dev llvm \
     make mc ncdu net-tools nnn openssh-server pass plantuml pydf python-openssl \
     rtv rtv shellcheck texinfo tig tk-dev wget xdg-utils xz-utils zlib1g-dev zlib1g-dev \
     wmctrl libsystemd-dev libmagick++-dev libmagickcore-dev

sudo add-apt-repository ppa:git-core/ppa -y
sudo add-apt-repository ppa:neovim-ppa/stable -y
sudo add-apt-repository ppa:fish-shell/release-3 -y
sudo add-apt-repository ppa:linrunner/tlp -y

sudo apt-fast install -y git neovim fish tlp

curl -sL https://deb.nodesource.com/setup_14.x | sudo -E bash -
sudo apt-get install -y nodejs
# bash lsp
sudo npm i -g bash-language-server prettier
#json lsp
sudo npm i -g vscode-json-languageserver

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
pip2 install pynvim

pyenv virtualenv 3.8.1 neovim3
pyenv activate neovim3

# configure emacs
pyenv virtualenv 3.8.1 tensor3
pyenv activate tensor3
pip3 install --upgrade "jedi>=0.13.0" "json-rpc>=1.8.1" "service_factory>=0.1.5"
pip3 install python-language-server[all] pyls-isort
pip3 install flake8 yapf autoflake isort autopep8
pip3 install "ptvsd>=4.2"
pip3 install importmagic epc

# install spacevim
curl -sLf https://spacevim.org/install.sh | bash

# configure fish
curl -L https://get.oh-my.fish | fish

# snap packages
sudo snap install --classic skype
sudo snap install opera
sudo snap install spotify
sudo snap install tusk
