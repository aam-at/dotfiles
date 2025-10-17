function fe --description 'Open selected files with $EDITOR'
    set -l query (string join ' ' -- $argv)

    set -l fzf (__fish_config_fzf_cmd)
    or return $status

    set -l selections (command $fzf --multi --select-1 --exit-0 --query="$query")
    if not set -q selections[1]
        return 1
    end

    set -l escaped (string escape -- $selections)
    eval $EDITOR $escaped
end
