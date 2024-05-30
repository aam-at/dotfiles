function fe -d "Open the selected file with the default editor
    - Bypass fuzzy finder if there's only one match (--select-1)
    - Exit if there's no match (--exit-0)"
    set -lx files (fzf-tmux --query=$argv --multi --select-1 --exit-0)
    if test -n $files
        eval $EDITOR $files
    end
end
