#!/bin/bash
# Drop-down scratch terminal, niri's take on the Windows Terminal quake window
# in ~/windots (shells/Niri-Common.ahk): a full-width kitty layer-shell panel
# hanging from the monitor's top edge, over the bar, down to the middle of the
# work area. Clicking away hides it, and the shell keeps running either way.
#
#   scratch-terminal.sh                    show or hide it (Mod+Grave)
#   scratch-terminal.sh <niri action...>   the window binds in common/binds.kdl
#
# A layer-shell panel is not a window niri can act on, so those binds come
# through here: while the drop-down is up they resize or close the panel, and
# otherwise they go to niri unchanged.

BAR=36 # [bar.main] thickness in config/noctalia/config.toml
SOCKET="unix:$XDG_RUNTIME_DIR/scratch-terminal.sock"
SIZE="$XDG_RUNTIME_DIR/scratch-terminal.size" # "width height" last applied

[ $# -eq 0 ] || kitten @ --to "$SOCKET" ls 2>/dev/null | jq -e '.[0].is_focused' >/dev/null ||
  exec niri msg action "$@"

read -r OUT_W OUT_H < <(niri msg -j focused-output | jq -r '.logical | "\(.width) \(.height)"')
: "${OUT_W:=1920}" "${OUT_H:=1080}"
HALF=$(((OUT_H + BAR) / 2))

[ -S "${SOCKET#unix:}" ] || rm -f "$SIZE" # a new panel starts at the default size
read -r w h 2>/dev/null <"$SIZE" || w=$OUT_W h=$HALF
case "$1" in
close-window) exec kitten @ --to "$SOCKET" close-window --match all ;;
maximize-column | fullscreen-window)
  # Both toggle between the drop-down and the whole screen.
  if [ "$w" -ge "$OUT_W" ] && [ "$h" -ge "$OUT_H" ]; then h=$HALF; else h=$OUT_H; fi
  w=$OUT_W
  ;;
set-column-width) w=$((w + OUT_W * ${2%\%} / 100)) ;;
set-window-height) h=$((h + OUT_H * ${2%\%} / 100)) ;;
esac

clamp() { echo $(($1 < $2 ? $2 : $1 > $3 ? $3 : $1)); }
w=$(clamp "$w" $((OUT_W * 3 / 10)) "$OUT_W")
h=$(clamp "$h" $((OUT_H * 2 / 10)) "$OUT_H")
margin=$(((OUT_W - w) / 2)) # narrower than the screen stays centered

if [ $# -eq 0 ]; then
  # Exclusive zone -1 ignores the bar's reserved strip; kitty's own
  # quick-access-terminal kitten pins it to 0, which stops under the bar.
  # kitty re-applies these settings on every show, hence the saved size.
  exec kitten panel --single-instance --instance-group scratch-terminal \
    --toggle-visibility --app-id scratch-terminal --listen-on "$SOCKET" \
    --edge top --layer overlay --exclusive-zone=-1 --override-exclusive-zone \
    --lines "${h}px" --margin-left "$margin" --margin-right "$margin" \
    --focus-policy on-demand --hide-on-focus-loss
fi

kitten @ --to "$SOCKET" resize-os-window --action os-panel --incremental \
  "lines=${h}px" "margin-left=$margin" "margin-right=$margin" &&
  echo "$w $h" >"$SIZE"
