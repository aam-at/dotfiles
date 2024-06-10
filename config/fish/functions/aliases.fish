# some more ls aliases
alias l='ls -CF'
alias -- -="cd -"

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

if test -n "$KITTY_PID"
  alias icat="kitten icat"
  alias ssh="kitty +kitten ssh"
  alias diff="kitten diff"
  alias hg="kitten hyperlinked-grep"
fi
