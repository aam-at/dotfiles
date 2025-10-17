function __fish_config_fzf_cmd --description 'Return the preferred fzf frontend.'
    if type -q fzf-tmux
        echo fzf-tmux
        return 0
    end

    if type -q fzf
        echo fzf
        return 0
    end

    echo 'fzf is required but not installed.' >&2
    return 127
end
