# Path to Oh My Fish install.
set -gx OMF_PATH /home/aam/.local/share/omf

# Customize Oh My Fish configuration path.
#set -gx OMF_CONFIG /home/aam/.config/omf

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish

# Oh My Fish theme
set fish_theme agnoster

# Oh My Fish plugins
set fish_plugins vi-mode python pyenv
# Load fishmarks (http://github.com/techwizrd/fishmarks)
# install: curl -L https://github.com/techwizrd/fishmarks/raw/master/install.fish | fish
. $HOME/.fishmarks/marks.fish

# autojump utility for easy navigation
if test -f /home/aam/.autojump/share/autojump/autojump.fish;
    . /home/aam/.autojump/share/autojump/autojump.fish;
end

# use vi-mode by default
fish_vi_mode

# colorscheme for fish
source ~/dotfiles/fish/solarized.fish

# Add sbin to PATH variable
set -x PATH $PATH /sbin/
set -x GOPATH $HOME/go

####################
# Custom functions #
####################

function ll
    ls -lh $argv
end
alias fuck='TF_ALIAS=fuck eval $(thefuck $(fc -ln -1))'
