function fkill --description 'Interactively kill selected processes'
    set -l signal 9
    if set -q argv[1]
        set signal $argv[1]
    end

    set -l fzf (__fish_config_fzf_cmd)
    or return $status

    set -l rows (ps -eo pid=,ppid=,stat=,command= | command $fzf --multi)
    if not set -q rows[1]
        return 1
    end

    set -l pids
    for row in $rows
        set -l pid (string match -r '^[0-9]+' -- $row)
        if set -q pid[1]
            set -a pids $pid[1]
        end
    end

    if not set -q pids[1]
        return 1
    end

    command kill -$signal $pids
end
