function fo -d "Open the selected file
    - CTRL-O to open with `open` command,
    - CTRL-E or Enter key to open with the $EDITOR"
    set -lx out (fzf-tmux --query=$argv --exit-0 --expect=ctrl-o,ctrl-e)
    set -lx key (echo $out | cut -d " " -f1)
    set -lx file (echo $out | cut -d " " -f2-)
    if test -n $file
        if [ $key = "ctrl-o" ]
            xdg-open "$file"
        else
            eval $EDITOR "$file"
        end
    end
end
