function cdf --description 'cd into the directory of the selected file'
    set -l query (string join ' ' -- $argv)

    set -l fzf (__fish_config_fzf_cmd)
    or return $status

    set -l result (command $fzf +m --exit-0 --query="$query")
    if not set -q result[1]
        return 1
    end

    set -l dir (command dirname -- $result[1])
    builtin cd -- $dir
end
