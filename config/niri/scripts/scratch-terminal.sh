#!/bin/bash
# Drop-down scratch terminal, niri's take on the Windows Terminal quake window
# in ~/windots (shells/Niri-Common.ahk): a full-width kitty layer-shell panel
# hanging from the monitor's top edge, over the bar, down to the middle of the
# work area. Mod+Grave toggles it, and the shell keeps running either way.
#
#   scratch-terminal.sh                    show or hide it (Mod+Grave)
#   scratch-terminal.sh <niri action...>   the window binds in common/binds.kdl
#
# A layer-shell panel is not a window niri can act on, so those binds come
# through here: while the drop-down is up they resize or close the panel, and
# otherwise they go to niri unchanged.

BAR=36 # [bar.main] thickness in config/noctalia/config.toml

# A shell can outlive the compositor whose socket it inherited (for example,
# after restarting Niri). Keep the supplied socket when it is live; otherwise
# find the socket belonging to this Wayland display.
resolve_niri_socket() {
  if [[ -n ${NIRI_SOCKET:-} ]] && niri msg version >/dev/null 2>&1; then
    return 0
  fi
  for socket in "$XDG_RUNTIME_DIR"/niri."${WAYLAND_DISPLAY:-}".*.sock; do
    [[ -S $socket ]] || continue
    if NIRI_SOCKET="$socket" niri msg version >/dev/null 2>&1; then
      export NIRI_SOCKET="$socket"
      return 0
    fi
  done
  return 1
}

if [ $# -gt 0 ]; then
  resolve_niri_socket || exec niri msg action "$@"
else
  resolve_niri_socket || true
fi

read -r OUT_NAME OUT_W OUT_H < <(
  niri msg -j focused-output 2>/dev/null |
    jq -r 'if .name and .logical then [.name, .logical.width, .logical.height] | @tsv else empty end'
)
if [[ -z ${OUT_NAME:-} || $OUT_NAME == null ]]; then
  OUT_NAME=default
  OUT_W=1920
  OUT_H=1080
fi
OUTPUT_ID=${OUT_NAME//[^[:alnum:]_.-]/_}

SOCKET_PATH="$XDG_RUNTIME_DIR/scratch-terminal-$OUTPUT_ID.sock"
SOCKET="unix:$SOCKET_PATH"
SIZE="$XDG_RUNTIME_DIR/scratch-terminal-$OUTPUT_ID.size"
INSTANCE_GROUP="scratch-terminal-$OUTPUT_ID"
PANEL_OUTPUT_ARGS=()
[[ $OUT_NAME == default ]] || PANEL_OUTPUT_ARGS=(--output-name "$OUT_NAME")

if [ $# -gt 0 ]; then
  kitten @ --to "$SOCKET" ls 2>/dev/null | jq -e '.[0].is_focused' >/dev/null ||
    exec niri msg action "$@"
fi

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

show_panel() {
  kitten @ --to "$SOCKET" resize-os-window --action os-panel --incremental \
    edge=top layer=overlay focus-policy=exclusive \
    "lines=${h}px" "margin-left=$margin" "margin-right=$margin" &&
    sleep 0.1 &&
    kitten @ --to "$SOCKET" resize-os-window --action show &&
    echo "$w $h" >"$SIZE"
}

if [ $# -eq 0 ]; then
  for other_socket in "$XDG_RUNTIME_DIR"/scratch-terminal*.sock; do
    [[ -S $other_socket && $other_socket != "${SOCKET#unix:}" ]] || continue
    kitten @ --to "unix:$other_socket" resize-os-window --action hide 2>/dev/null || true
  done
  sleep 0.1

  if [ ! -S "${SOCKET#unix:}" ]; then
    # Exclusive zone -1 ignores the bar's reserved strip; kitty's own
    # quick-access-terminal kitten pins it to 0, which stops under the bar.
    kitten panel --single-instance --instance-group "$INSTANCE_GROUP" --detach \
      --app-id scratch-terminal --listen-on "$SOCKET" \
      --edge top --layer overlay --exclusive-zone=-1 --override-exclusive-zone \
      --lines "${h}px" --margin-left "$margin" --margin-right "$margin" \
      --focus-policy exclusive "${PANEL_OUTPUT_ARGS[@]}" || exit
    for _ in {1..50}; do
      kitten @ --to "$SOCKET" ls >/dev/null 2>&1 && break
      sleep 0.1
    done
  elif kitten @ --to "$SOCKET" ls 2>/dev/null | jq -e '.[0].is_focused' >/dev/null; then
    exec kitten @ --to "$SOCKET" resize-os-window --action hide
  fi

  show_panel
  exit
fi

kitten @ --to "$SOCKET" resize-os-window --action os-panel --incremental \
  "lines=${h}px" "margin-left=$margin" "margin-right=$margin" &&
  echo "$w $h" >"$SIZE"
