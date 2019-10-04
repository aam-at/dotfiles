################
# Basic config #
################

# use emacs keybindgins by default
set -U fish_key_bindings fish_default_key_bindings

# colorscheme for fish
source ~/dotfiles/fish/solarized.fish

if test -n "$EMACS"
    set -x TERM eterm-color

    function fish_title
        true
    end
end

##############################
# Load environment variables #
##############################

# Source environement variables shared between different shells.
# http://unix.stackexchange.com/questions/176322/share-environment-variables-between-bash-and-fish/176331#176331
function export --description 'Set global variable. Alias for set -gx, made for bash compatibility'
    if test -z "$argv"
        set
        return 0
    end
    for arg in $argv
        set -l v (echo $arg|sed s/=/\\n/)
        switch (count $v)
            case 1
                set -gx $v $$v
            case 2
                if [ $v[1] = PATH ]
                    set -gx PATH (echo $v[2]|tr ': ' \n)
                else
                    set -gx $v
                end
        end
    end
end

if test -e ~/.env
    if test -z "$PATH_BK"
        set -x PATH_BK $PATH
    end
    source ~/.env
end

#####################
# Configure plugins #
#####################

# Path to Oh My Fish install.
set -gx OMF_PATH $HOME/.local/share/omf

# Load oh-my-fish configuration.
if test -d $OMF_PATH # Customize Oh My Fish configuration path.
    set -gx OMF_CONFIG $HOME/.config/omf

    source $OMF_PATH/init.fish

    # Oh My Fish plugins
    set fish_plugins python pyenv gi git-flow emacs weather fasd
    # Oh My Fish themes
    set fish_themes agnoster batman krisleech zish toaster ocean syl20bnr
    # select theme
    set fish_theme agnoster
end

# configure autojump
if test -d /usr/share/autojump
    source /usr/share/autojump/autojump.fish
end

# configure pyenv
if test -d ~/.pyenv
    set -gx PYENV_ROOT $HOME/.pyenv
    set -x PATH $HOME/.pyenv/bin $PATH
    pyenv init - | source
    pyenv virtualenv-init - | source
end

# configure icons-in-terminal
if test -d ~/.local/share/icons-in-terminal
    source ~/.local/share/icons-in-terminal/icons.fish
end

################################
# Custom aliases and functions #
################################
if test -e ~/.aliases
    source ~/.aliases
end
abbr -a -- - 'cd -'

function reload
    source ~/.config/fish/config.fish
end
