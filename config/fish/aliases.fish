abbr -a -- - 'cd -'

# If inside a kitty terminal, add aliases for kitty's icat, ssh, diff, and hg
if test -n "$KITTY_PID"; or begin; tmux info > /dev/null 2>&1; and test -n (tmux showenv KITTY_PID 2>/dev/null | string split -f2 '='); end
    alias icat="kitten icat"
    alias ssh="kitty +kitten ssh"
    alias realssh="/usr/bin/ssh"
    alias diff="kitten diff"
    alias hg="kitten hyperlinked-grep"
end

function y
    set tmp (mktemp -t "yazi-cwd.XXXXXX")
    yazi $argv --cwd-file="$tmp"
    if set cwd (command cat -- "$tmp"); and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
        builtin cd -- "$cwd"
    end
    rm -f -- "$tmp"
end
