function cfe -d "fuzzy open file from anywhere"
    set -lx file (locate -Ai -0 $argv | grep -z -vE '~$' | fzf-tmux --read0 -0 -1)
    if test -n $file
        if test -d $file
            eval $EDITOR $file
        else
            eval $EDITOR -- $file
        end
    end
end
