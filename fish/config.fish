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

function fuck -d 'Correct your previous console command'
    set -l exit_code $status
    set -l eval_script (mktemp 2>/dev/null ; or mktemp -t 'thefuck')
    set -l fucked_up_commandd $history[1]
    thefuck $fucked_up_commandd > $eval_script
    . $eval_script
    rm $eval_script
    if test $exit_code -ne 0
        history --delete $fucked_up_commandd
    end
end
