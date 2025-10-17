################
# Basic config #
################

# Rebuild user-specific PATH entries deterministically each session.
set -e fish_user_paths

# Keep default (Emacs-style) key bindings unless explicitly overridden.
set -q fish_key_bindings; or set -U fish_key_bindings fish_default_key_bindings

set -g __fish_config_file ~/.config/fish/config.fish

if set -q EMACS
    set -x TERM eterm-color

    function fish_title
        true
    end
end

function __fish_config_hostname --description 'Return current hostname in a cross-shell friendly way'
    if set -q hostname
        echo $hostname
    else
        hostname
    end
end

function __fish_config_load_keychain --description 'Load cached ssh/gpg agent environment exported by keychain'
    set -l host (__fish_config_hostname)
    set -l base ~/.keychain/$host-fish

    for file in $base $base-gpg
        if test -r $file
            source $file
        end
    end
end

function __fish_config_source_env --description 'Merge ~/.env exports into fish and keep PATH ordering intact'
    set -l env_file ~/.env
    if not test -r $env_file
        return
    end

    set -l original_path $PATH
    set -l original_user_paths $fish_user_paths

    source $env_file

    set -l env_path $PATH

    set -gx PATH $original_path
    set -U fish_user_paths $original_user_paths

    if set -q env_path[1]
        if functions -q fish_add_path
            set -l env_count (count $env_path)
            for idx in (seq $env_count -1 1)
                set -l dir $env_path[$idx]
                if test -n "$dir"
                    fish_add_path --move $dir
                end
            end
        else
            set -U fish_user_paths $env_path $fish_user_paths
        end
    end
end

__fish_config_source_env

if status --is-interactive; and test -t 1
    __fish_config_load_keychain
end

#####################
# Configure plugins #
#####################

# Path and configuration for Oh My Fish live in conf.d/omf.fish

# Configure fifc only once the command is available.
if type -q fifc
    set -q fifc_editor; or set -Ux fifc_editor vim
    set -q fifc_keybinding; or set -U fifc_keybinding \cx
    set -q fifc_bat_opts; or set -U fifc_bat_opts --style=numbers
    set -q fifc_fd_opts; or set -U fifc_fd_opts --hidden
end

if status --is-interactive
    if type -q zoxide
        zoxide init fish | source
    end

    if type -q atuin
        set -gx ATUIN_NOBIND true
        atuin init fish | source
    end

    if test -d /usr/share/autojump
        source /usr/share/autojump/autojump.fish
    end

    if test -d ~/.pyenv
        set -gx PYENV_ROOT $HOME/.pyenv
        if functions -q fish_add_path
            fish_add_path --move $PYENV_ROOT/bin
        else
            set -U fish_user_paths $PYENV_ROOT/bin $fish_user_paths
        end
        if type -q pyenv
            pyenv init - | source
            pyenv virtualenv-init - | source
        end
    end

    if type -q direnv
        direnv hook fish | source
        set -g direnv_fish_mode disable_arrow
    end

    if test -d ~/.local/share/icons-in-terminal
        source ~/.local/share/icons-in-terminal/icons.fish
    end
end

################################
# Custom aliases and functions #
################################

if test -r ~/.aliases
    source ~/.aliases
end

if test -r ~/.config/fish/aliases.fish
    source ~/.config/fish/aliases.fish
end

if functions -q fish_add_path
    fish_add_path --move $HOME/.fzf/bin
else
    set -U fish_user_paths $HOME/.fzf/bin $fish_user_paths
end

function reload
    if test -r $__fish_config_file
        source $__fish_config_file
    end
end

function emacs
    # https://github.com/syl20bnr/spacemacs/wiki/Terminal
    set -lx TERM xterm-24bit
    eval (which emacs) $argv
end

functions -e __fish_config_hostname
functions -e __fish_config_load_keychain
functions -e __fish_config_source_env
