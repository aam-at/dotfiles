abbr -a -- - 'cd -'

# If inside a kitty terminal, add aliases for kitty's helpers
set -l __fish_in_kitty 0

if set -q KITTY_PID
    set __fish_in_kitty 1
else if set -q TMUX; and type -q tmux
    set -l kitty_env (tmux showenv KITTY_PID 2>/dev/null | string split -f2 '=')
    if test -n "$kitty_env"
        set __fish_in_kitty 1
    end
end

if test $__fish_in_kitty -eq 1
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

if type -q hyprctl
    set -l __fish_aliases_dir (command dirname -- (status filename))
    if test -r "$__fish_aliases_dir/hypr_aliases.fish"
        source "$__fish_aliases_dir/hypr_aliases.fish"
    end
end

if type -q niri
    set -l __fish_aliases_dir (command dirname -- (status filename))
    if test -r "$__fish_aliases_dir/niri_aliases.fish"
        source "$__fish_aliases_dir/niri_aliases.fish"
    end
end
