function cfe --description 'Fuzzy open file from locate database'
    set -l query (string join ' ' -- $argv)

    if not type -q locate
        echo 'cfe: locate is required but not installed.' >&2
        return 127
    end

    set -l fzf (__fish_config_fzf_cmd)
    or return $status

    set -l selection (command locate -Ai -0 -- $query | command grep -z -vE '~$' | command $fzf --read0 -0 --exit-0)
    if not set -q selection[1]
        return 1
    end

    set -l target $selection[1]
    set -l escaped (string escape -- $target)

    if test -d $target
        eval $EDITOR $escaped
    else
        eval $EDITOR -- $escaped
    end
end
