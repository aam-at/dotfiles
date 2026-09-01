# Open HerdR in its dedicated Kitty OS window instead of nesting it in tmux.
function herd --description 'Open HerdR in a dedicated Kitty OS window'
    if not type -q herdr
        echo "herdr is not installed or is not on PATH" >&2
        return 127
    end

    if set -q KITTY_WINDOW_ID
        kitty @ launch --type=os-window --cwd=current --title=HerdR herdr $argv
    else
        kitty --title HerdR herdr $argv
    end
end
