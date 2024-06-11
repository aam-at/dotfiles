abbr -a -- - 'cd -'

# If inside a kitty terminal, add aliases for kitty's icat, ssh, diff, and hg
if test -n "$KITTY_PID"
  alias icat="kitten icat"
  alias ssh="kitty +kitten ssh"
  alias diff="kitten diff"
  alias hg="kitten hyperlinked-grep"
end
