function export --description 'Bash-compatible helper that proxies to set -gx'
    if test (count $argv) -eq 0
        set
        return
    end

    for arg in $argv
        set -l pair (string split -m 1 '=' -- $arg)
        if test (count $pair) -eq 1
            set -l name $pair[1]
            if set -q $name
                set -gx $name $$name
            else
                set -gx $name
            end
            continue
        end

        set -l name $pair[1]
        set -l value $pair[2]

        if test $name = PATH
            set -l paths (string split ':' -- $value)
            set -gx PATH $paths
        else
            set -gx $name $value
        end
    end
end
