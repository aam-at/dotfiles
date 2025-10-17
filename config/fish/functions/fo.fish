function fo --description 'Open the selected file (ctrl-o via opener, enter via $EDITOR)'
    set -l query (string join ' ' -- $argv)

    set -l fzf (__fish_config_fzf_cmd)
    or return $status

    set -l result (command $fzf --exit-0 --expect=ctrl-o,ctrl-e --query="$query")
    if not set -q result[2]
        return 1
    end

    set -l key $result[1]
    set -l target $result[2]
    set -l escaped (string escape -- $target)

    if test "$key" = ctrl-o
        if type -q xdg-open
            command xdg-open -- $target >/dev/null 2>&1 &
            disown
        else if type -q open
            command open -- $target >/dev/null 2>&1 &
            disown
        else
            echo 'fo: neither xdg-open nor open is available.' >&2
            return 127
        end
    else
        eval $EDITOR $escaped
    end
end
