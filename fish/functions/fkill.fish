function fkill -d "kill process"
    set -lx pid (ps -ef | sed 1d | fzf-tmux -m | awk '{print $2}')
    if [ "x$pid" != "x" ]
        set -lx signal $argv[1]
        if test -z $signal
            set signal 9
        end
        echo $pid | xargs kill -$signal $pid
    end
end
