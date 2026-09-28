#!/usr/bin/env bash
# ActivityWatch on Linux, set up for Wayland (Hyprland, niri, COSMIC):
#   - aw-server, the stock Python server: activitywatch-bin from the AUR with
#     yay, else the official release in ~/.local/opt/activitywatch;
#   - aw-awatcher (github.com/2e3s/awatcher) instead of the stock
#     aw-watcher-window and aw-watcher-afk, which only see X11: the focused
#     window and idle time under Wayland, in the same buckets. The latest
#     release goes to ~/.local/opt/aw-awatcher.
# systemd user units run them (config/activitywatch), not aw-qt, whose
# autostart entry is hidden here so it can't start a second server.
# Safe to re-run: only what's missing or outdated is fetched.
set -euo pipefail

opt="$HOME/.local/opt"
bin="$HOME/.local/bin"
mkdir -p "$opt" "$bin"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# The latest release's tag, or nothing if GitHub can't be reached.
latest_tag() {
  { curl -fsSL "https://api.github.com/repos/$1/releases/latest" || true; } | sed -n 's/.*"tag_name": *"\([^"]*\)".*/\1/p' | head -1
}

# unzip, or Python's zipfile where unzip isn't installed.
extract() {
  if command -v unzip >/dev/null 2>&1; then
    unzip -oq "$1" -d "$2"
  else
    python3 -m zipfile -e "$1" "$2"
  fi
}

# --- aw-server ---
if command -v aw-server >/dev/null 2>&1 || [[ -x "$bin/aw-server" ]]; then
  echo "aw-server: $(command -v aw-server || echo "$bin/aw-server")"
elif command -v yay >/dev/null 2>&1; then
  yay -S --needed --noconfirm activitywatch-bin
else
  tag=$(latest_tag ActivityWatch/activitywatch)
  echo "aw-server: installing ActivityWatch $tag in $opt/activitywatch"
  curl -fsSL -o "$tmp/aw.zip" "https://github.com/ActivityWatch/activitywatch/releases/download/$tag/activitywatch-$tag-linux-x86_64.zip"
  rm -rf "$opt/activitywatch"
  extract "$tmp/aw.zip" "$opt"
  chmod +x "$opt"/activitywatch/aw-*/aw-* 2>/dev/null || true
  ln -sf "$opt/activitywatch/aw-server/aw-server" "$bin/aw-server"
fi

# --- aw-awatcher ---
tag=$(latest_tag 2e3s/awatcher)
if [[ -z "$tag" ]]; then
  echo "aw-awatcher: couldn't read the latest release from GitHub; skipped" >&2
elif [[ "$(cat "$opt/aw-awatcher/VERSION" 2>/dev/null)" == "$tag" ]]; then
  echo "aw-awatcher: $tag, up to date"
else
  echo "aw-awatcher: installing $tag"
  curl -fsSL -o "$tmp/aw-awatcher.zip" "https://github.com/2e3s/awatcher/releases/download/$tag/aw-awatcher.zip"
  mkdir -p "$opt/aw-awatcher"
  extract "$tmp/aw-awatcher.zip" "$opt/aw-awatcher"
  chmod +x "$opt/aw-awatcher/aw-awatcher"
  echo "$tag" >"$opt/aw-awatcher/VERSION"
  ln -sf "$opt/aw-awatcher/aw-awatcher" "$bin/aw-awatcher"
  # A running watcher picks up the new binary.
  systemctl --user try-restart aw-awatcher.service 2>/dev/null || true
fi

# --- aw-qt's autostart ---
# activitywatch-bin installs /etc/xdg/autostart/aw-qt.desktop; a user entry
# of the same name with Hidden=true switches it off (XDG autostart spec).
mkdir -p "$HOME/.config/autostart"
printf '[Desktop Entry]\nType=Application\nName=ActivityWatch\nHidden=true\n' >"$HOME/.config/autostart/aw-qt.desktop"
