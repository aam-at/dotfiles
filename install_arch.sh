#!/usr/bin/env bash
# Arch setup: system services, packages, desktop, language toolchains and
# editors. Run ./install afterwards to link the configuration.
# Options: see --help. INSTALL_DESKTOP_SHELLS=false skips the desktop shells.
set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$REPO_DIR/setup/lib.sh"

# ── Options ──────────────────────────────────────────────────

GUI=${GUI:-false}
TOOLS_DIR=${TOOLS_DIR:-"$HOME/local/tools"}
INSTALL_PYTHON=${INSTALL_PYTHON:-true}
INSTALL_RUST=${INSTALL_RUST:-true}
INSTALL_GO=${INSTALL_GO:-true}
INSTALL_LUA=${INSTALL_LUA:-true}
INSTALL_NODE=${INSTALL_NODE:-true}
INSTALL_OLLAMA=${INSTALL_OLLAMA:-false}
INSTALL_EMACS=${INSTALL_EMACS:-true}
INSTALL_FONTS=${INSTALL_FONTS:-true}
INSTALL_DESKTOP_SHELLS=${INSTALL_DESKTOP_SHELLS:-true}

if [ -f /etc/os-release ]; then
  . /etc/os-release
  echo "This script is running on $NAME"
else
  echo "Unable to detect distribution."
  exit 1
fi

parse_common_args "$@"

# ── Helpers ──────────────────────────────────────────────────

install_packages() {
  echo "Installing packages..."
  sudo pacman -S --needed --noconfirm "$@"
}

install_aur_packages() {
  echo "Installing AUR packages..."
  yay -S --needed --noconfirm "$@"
}

# ── System ───────────────────────────────────────────────────

install_packages power-profiles-daemon
sudo systemctl enable --now power-profiles-daemon.service

# Firmware updates and NVMe health.
install_packages fwupd nvme-cli

if [[ "$(findmnt -no FSTYPE / 2>/dev/null || true)" == btrfs ]]; then
  install_packages btrfs-progs
fi

# ── Base packages ────────────────────────────────────────────

# Everything below relies on these (yay, ufw and pipx among them).
install_packages \
  adw-gtk-theme age alsa-lib appmenu-gtk-module aspell aspell-en aspell-ru \
  automake base-devel bash-completion bat bc bibtool bison btop bun bzip2 \
  ca-certificates cava chafa clang cmake cscope curl dgop direnv dust \
  emacs-wayland enchant exo expat fasd fd ffmpeg file-roller fish fontconfig \
  freeglut freetype2 fuse2 fuse3 fzy gcc gettext ghostty giflib git \
  github-desktop glances global gmime3 gnome-keyring gnupg gnuplot gnutls gtk4 \
  guile gvfs helix hexyl hspell htop httpie hunspell hyprpolkitagent \
  imagemagick iotop iputils isync jansson jbig2dec jq kitty lazygit leptonica \
  libappindicator libdbusmenu-glib libevent libffi libgccjit libjpeg-turbo \
  libmupdf libpng libsecret libtiff libvoikko libvterm libxcb libxcomposite \
  libxcursor libxfixes libxi libxkbcommon libxmu libxpm lldb llvm lynx make \
  markdown-oxide mc meson mosh mujs mupdf mupdf-tools ncdu ncurses neovim \
  net-tools nnn notmuch nuspell openblas openconnect openssh openssl p7zip \
  pandoc parallel pass pdfgrep pdfpc pdftk peco pinentry poppler poppler-glib \
  procs python python-gobject python-pip python-pipx python-pyopenssl qps ranger \
  readline ripgrep ruby screen sdl2 shellcheck shfmt sndio sqlite stow systemd \
  taplo texinfo the_silver_searcher thunar thunar-archive-plugin tig tk tmux \
  trash-cli tree-sitter ufw unrar vale webkit2gtk-4.1 wget wmctrl xapian-core \
  xdg-desktop-portal-hyprland xdg-utils xfce4-settings xh xz yay ydotool \
  zathura zathura-djvu zathura-pdf-poppler zed zellij zenity zlib-ng zoxide

# Preserve remote access before enabling the firewall.
sudo ufw allow ssh
sudo ufw enable

# ── Command-line tools ───────────────────────────────────────

install_packages git-annex git-crypt git-delta git-lfs github-cli

install_packages \
  atuin bottom broot eza gitu gitui gping lsd navi ouch sd starship tealdeer \
  television texlab viu watchexec yazi

install_aur_packages fpp-git fswatch git-hub git-secrets gitflow-avh mu

# Install or update herdr.
if command -v herdr >/dev/null 2>&1; then
  herdr update
else
  curl -fsSL https://herdr.dev/install.sh | sh
fi

install_fzf

# ── TeX Live ─────────────────────────────────────────────────

install_packages \
  biber perl-file-homedir perl-ipc-system-simple perl-tk perl-yaml-tiny \
  texlive-basic texlive-bibtexextra texlive-binextra texlive-fontsextra \
  texlive-fontsrecommended texlive-langcyrillic texlive-latex texlive-latexextra \
  texlive-latexrecommended texlive-luatex texlive-mathscience \
  texlive-plaingeneric texlive-xetex wdiff

# ── Fonts ────────────────────────────────────────────────────

install_packages \
  powerline-fonts ttf-fira-code ttf-jetbrains-mono ttf-nerd-fonts-symbols-mono

if $INSTALL_FONTS; then
  install_font_packages "$TOOLS_DIR"
fi

# ── Desktop ──────────────────────────────────────────────────

# Both shells are installed so `desktop-shell` can switch between Dank
# Material Shell and Noctalia without another package transaction.
if $INSTALL_DESKTOP_SHELLS; then
  install_packages \
    brightnessctl cliphist dms-shell hyprland matugen niri noctalia quickshell
fi

install_packages chatgpt-desktop-bin claude-desktop

install_aur_packages \
  antigravity dsearch-bin insync microsoft-edge-stable-bin noisetorch \
  teams-for-linux visual-studio-code-bin

if $GUI; then
  echo "Installing packages for Wayland..."

  install_packages discord flatpak languagetool neovide obsidian

  flatpak install flathub com.discordapp.Discord
  flatpak install flathub com.logseq.Logseq
  flatpak install flathub it.mijorus.gearlever

  # aw-server and awatcher (Wayland window and idle watcher).
  "$REPO_DIR/setup/install_activitywatch.sh"
fi

# ── Language toolchains ──────────────────────────────────────

if $INSTALL_NODE; then
  echo "Installing Node.js..."
  install_packages nodejs npm
  "$REPO_DIR/setup/install_node_packages.sh"
fi

if $INSTALL_PYTHON; then
  echo "Installing uv and plugins..."
  pipx install --force uv
  "$REPO_DIR/setup/install_python_tools.sh"
fi

if $INSTALL_RUST; then
  echo "Installing Rust and cargo packages..."
  install_packages cargo-update rustup
  rustup default stable
  "$REPO_DIR/setup/install_rust_packages.sh"

  # for kanata
  sudo groupadd -f uinput
  sudo usermod -aG input "$USER"
  sudo usermod -aG uinput "$USER"

  if $GUI; then
    cargo install --git https://github.com/neovide/neovide
  fi
fi

if $INSTALL_GO; then
  echo "Installing go and go packages..."
  install_packages go
  "$REPO_DIR/setup/install_go_packages.sh"
fi

if $INSTALL_LUA; then
  install_packages lua
fi

# ── Editors and AI ───────────────────────────────────────────

if $INSTALL_EMACS; then
  install_emacs
fi

if $GUI; then
  install_intellimacs
fi

if $INSTALL_OLLAMA; then
  install_ollama
fi

# ── Shell completions ────────────────────────────────────────

# Last, so every tool installed above gets its completions.
"$REPO_DIR/setup/install_shell_completions.sh"

echo "Setup complete!"
