#!/bin/sh

# fresh ubuntu installation {
    # sudo apt-get update
    # sudo apt-get upgrade -y
    # sudo apt-get install vim-gtk cmake build-essential silversearcher-i fish -y
    # install vundle
    # git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
    # vim +PluginInstall
    # Compile and configure vim to use anaconda python2.7
    # sudo apt-get install liblua5.2-dev ruby-dev ctags
    # pipi install ropevim
    # ./configure --with-features=huge --enable-rubyinterp \
    #             --enable-pythoninterp --with-python-config-dir=$HOME/anaconda/lib/python2.7/config \
    #             --enable-gui=gtk --enable-cscope \
    #             --enable-luainterp --with-luajit --with-lua-prefix=$HOME/local/lua_packages/torch/install/bin/th \
    #             --enable-fail-if-missing --prefix=$HOME/opt/vim
    # make install
    # sudo update-alternatives --install /usr/bin/editor editor /usr/local/bin/vim 0
    # http://stackoverflow.com/questions/26956933/how-to-make-vim74-compile-with-python
# }
ln -fi ./tmux/tmux.conf ~/.tmux.conf
ln -fi ./fish/config.fish ~/.config/fish/config.fish
ln -fi vimrc ~/.vimrc
ln -fi ./idea/ideavimrc ~/.ideavimrc
