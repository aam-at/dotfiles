function fd -d "cd to selected directory"
    set -lx path $argv[1]
    if test -z $path
        set path "."
    end
    set -lx dir (find -L $path -path '*/\.*' -prune -o -type d -print 2> /dev/null | fzf-tmux +m)
    cd $dir
end
