###################
# Basic functions #
###################
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
    set -l env_fish_file ~/.env.fish

    # Check if .env.fish exists, if not, create it
    if not test -r $env_fish_file
        # If .env.fish doesn't exist, create it from .env
        if test -r $env_file
            # Create .env.fish by extracting exports from .env
            set -l env_content (cat $env_file)
            for line in $env_content
                if echo $line | grep -q '^export ' # Only process export lines
                    # Remove 'export ' and split the remaining string by the first '='
                    set -l export_line (string trim (string replace 'export ' '' $line))
                    set -l var_name (string split '=' $export_line)[1]
                    set -l var_value (string split '=' $export_line)[2..]
                    set var_value (string join '=' $var_value) # In case the value has multiple '=' signs
                    echo "set -gx $var_name $var_value" >>$env_fish_file
                end
            end
            echo "Generated $env_fish_file from $env_file."
        end
    end

    # Now proceed to source the .env.fish (or .env)
    if test -r $env_fish_file
        source $env_fish_file
    else
        if test -r $env_file
            source $env_file
        end
    end

    set -U fish_user_paths $env_path $fish_user_paths
end

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

__fish_config_source_env

if status --is-interactive; and test -t 1
    __fish_config_load_keychain
end

# Rebuild user-specific PATH entries deterministically each session.
set -e fish_user_paths

#####################
# Configure plugins #
#####################

if status is-interactive

    # Starship custom prompt
    starship init fish | source

    # atuin
    if command -v atuin &>/dev/null
        set -gx ATUIN_NOBIND true
        atuin init fish | source
    end

    # autojump
    if test -d /usr/share/autojump
        source /usr/share/autojump/autojump.fish
    end

    # direnv
    if command -v direnv &>/dev/null
        direnv hook fish | source
        set -g direnv_fish_mode disable_arrow
    end

    # fifc
    if type -q fifc
        set -q fifc_editor; or set -Ux fifc_editor vim
        # set -q fifc_keybinding; or set -U fifc_keybinding \cx
        set -q fifc_bat_opts; or set -U fifc_bat_opts --style=numbers
        set -q fifc_fd_opts; or set -U fifc_fd_opts --hidden
    end

    # television
    if command -v television &>/dev/null
        television init fish | source
    end

    # zoxide
    if command -v zoxide &>/dev/null
        zoxide init fish | source
    end

    # icons-in-terminal
    if test -d ~/.local/share/icons-in-terminal
        source ~/.local/share/icons-in-terminal/icons.fish
    end

    # pyenv if installed
    if test -d ~/.pyenv
        set -gx PYENV_ROOT $HOME/.pyenv
        if functions -q fish_add_path
            fish_add_path --move $PYENV_ROOT/bin
        else
            set -U fish_user_paths $PYENV_ROOT/bin $fish_user_paths
        end
        if command -v pyenv &>/dev/null
            pyenv init - | source
            pyenv virtualenv-init - | source
        end
    end

    # # Custom colours
    # cat ~/.local/state/caelestia/sequences.txt 2>/dev/null

    # For jumping between prompts in foot terminal
    function mark_prompt_start --on-event fish_prompt
        echo -en "\e]133;A\e\\"
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
