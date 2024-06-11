################
# Basic config #
################

# reset user paths
set -e fish_user_paths

# use emacs keybindgins by default
set -U fish_key_bindings fish_default_key_bindings

if test -n "$EMACS"
    set -x TERM eterm-color

    function fish_title
        true
    end
end

##############################
# Load environment variables #
##############################

if test -f ~/.keychain/$hostname-fish
    source ~/.keychain/$hostname-fish
end
if test -f ~/.keychain/$hostname-fish-gpg
    source ~/.keychain/$hostname-fish-gpg
end


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
    set -x PATH_BK $PATH
    source ~/.env
    set -U fish_user_paths $PATH $fish_user_paths
    set -gx PATH $PATH_BK
    set -e PATH_BK
end

#####################
# Configure plugins #
#####################

# Path to Oh My Fish install.
set -gx OMF_PATH $HOME/.local/share/omf

# Load oh-my-fish configuration.
if test -d $OMF_PATH # Customize Oh My Fish configuration path.
    set -gx OMF_CONFIG $HOME/.config/omf

    # Oh My Fish plugins
    set fish_plugins emacs fasd gi git-flow pyenv python weather
    # Oh My Fish themes
    set fish_themes agnoster batman krisleech ocean syl20bnr toaster zish
    # select theme
    set fish_theme batman
end

# configure zoxide
if type -q zoxide
  zoxide init fish | source
end

# configure autojump
if test -d /usr/share/autojump
    source /usr/share/autojump/autojump.fish
end

# configure pyenv
if test -d ~/.pyenv
    set -gx PYENV_ROOT $HOME/.pyenv
    set -U fish_user_paths $PYENV_ROOT/bin $fish_user_paths
    pyenv init - | source
    pyenv virtualenv-init - | source
end

# configure direnv
if type -q direnv
    # do stuff
    direnv hook fish | source
    set -g direnv_fish_mode eval_on_arrow    # trigger direnv at prompt, and on every arrow-based directory change (default)
    set -g direnv_fish_mode eval_after_arrow # trigger direnv at prompt, and only after arrow-based directory changes before executing command
    set -g direnv_fish_mode disable_arrow    # trigger direnv at prompt only, this is similar functionality to the original behavior
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
if test -e ~/.config/fish/aliases.fish
    source ~/.config/fish/aliases.fish
end

# configure fzf
if test -d ~/.fzf/bin
    set -U fish_user_paths $HOME/.fzf/bin $fish_user_paths
end

function reload
    source ~/.config/fish/config.fish
end

function emacs
    # https://github.com/syl20bnr/spacemacs/wiki/Terminal
    set -lx TERM xterm-24bit
    eval (which emacs) $argv
end

bind --erase --all \cf
bind -M insert \cf forward-char
bind --erase --all \ef
bind -M insert \ef forward-word
