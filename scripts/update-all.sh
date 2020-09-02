#!/usr/bin/env sh
sudo apt-get update
sudo apt-fast upgrade -y
sudo apt-fast dist-upgrade -y

cd ~/dotfiles
git stash; git pull --rebase; git stash pop
cd ~/.emacs.d
git stash; git pull --rebase; git stash pop

emacs --batch -l ~/.emacs.d/init.el --eval="(configuration-layer/update-packages t)"
