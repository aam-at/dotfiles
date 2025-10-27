function ff --description 'cd to a directory selected via fzf'
    set -l search_root '.'
    if set -q argv[1]
        set search_root $argv[1]
    end

    if not test -d $search_root
        printf 'ff: "%s" is not a directory.\n' $search_root >&2
        return 1
    end

    set -l fzf (__fish_config_fzf_cmd)
    or return $status

    set -l selection (command find -L $search_root -path '*/.*' -prune -o -type d -print 2>/dev/null | command $fzf +m)
    if not set -q selection[1]
        return 1
    end

    builtin cd -- $selection
end
