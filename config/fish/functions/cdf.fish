function cdf -d "cd into the directory of the selected file"
    set -lx file (fzf-tmux +m -q "$argv")
    set -lx dir (dirname $file)
    cd $dir
end
