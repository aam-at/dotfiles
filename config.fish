set -x PATH $PATH /sbin/

function ll
    ls -lh $argv
end

# Path to Oh My Fish install.
set -gx OMF_PATH /home/aam/.local/share/omf

# Customize Oh My Fish configuration path.
#set -gx OMF_CONFIG /home/aam/.config/omf

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish

# Oh My Fish theme
set fish_theme agnoster

# Oh My Fish plugins
set fish_plugins vi-mode vundle python pyenv

# Load fishmarks (http://github.com/techwizrd/fishmarks)
# install: curl -L https://github.com/techwizrd/fishmarks/raw/master/install.fish | fish
. $HOME/.fishmarks/marks.fish
