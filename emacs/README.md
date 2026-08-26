# Shared Emacs configuration

All configurations require Emacs 31.1 or later.

`config/` contains reusable, framework-independent feature setup. `funcs/`
contains reusable commands and helpers. Neither directory depends on a
Spacemacs layer manager, Doom's leader map, or either distribution's runtime
API.

`doom/` contains Doom-only module selection, package manifests, and adapter
configuration.  Its `packages/` directory is split by feature domain.

`spacemacs/` contains Spacemacs-only profiles, layer declarations, snippets,
and package-manager integration.

Leader-key declarations live in the host-specific configuration.  They may be
duplicated when the two hosts require different semantics.

## Language-server client

`config/config-lsp.el` selects the client shared by Doom and Spacemacs.
Eglot is the default; set `aam/language-server-client` to `lsp` to opt into
lsp-mode and its additional integrations. Restart Emacs after changing it;
run `doom-profile sync -! --no-env` when changing Doom's selection.

## Installed profiles

The tracked files above are configuration source only.  Do not install any
distribution into `~/.emacs.d`; that path cannot safely be shared by Doom,
Spacemacs, Prelude, and other distributions.

Use these XDG locations instead:

```text
~/.config/emacs/                 private profile configuration (Dotbot links)
~/.local/share/emacs/            framework checkouts
~/.local/state/emacs/            generated Doom state
```

Dotbot links the current profiles to `~/.config/emacs/doom`,
`~/.config/emacs/spacemacs-full`, `~/.config/emacs/spacemacs-basic`, and
`~/.config/emacs/spacemacs-writing`.  The `emacs-profile` launcher starts a
profile, while `doom-profile` runs the Doom CLI with the matching isolated
paths:

```sh
emacs-profile doom
emacs-profile spacemacs-full
doom-profile sync
```

Clone Doom to `~/.local/share/emacs/doom` and Spacemacs to
`~/.local/share/emacs/spacemacs`.  Future standalone profiles, such as
Prelude or Emalla, can place an `init.el` in `~/.config/emacs/<profile>/` and
run `emacs-profile <profile>`.

## Systemd daemons

The user unit is templated by profile, so each daemon has an isolated named
server socket.  First deploy the dotfiles so the unit and launcher are linked,
then use `emacs-daemon switch` to select the daemon started automatically and
used by graphical file-open requests:

```sh
./install
systemctl --user daemon-reload
systemctl --user disable --now emacs.service  # one-time migration from the old unit
emacs-daemon switch doom
emacs-daemon switch spacemacs-full
emacs-daemon open spacemacs-full path/to/file
```

`switch` intentionally keeps only one managed profile enabled and records the
selection in `~/.local/state/emacs/default-profile`; `emacsclient-visual.sh`,
desktop-file associations, and the Hyprland editor binding follow it.  To run
two profiles concurrently, start each instance explicitly and connect to its
named socket with `emacsclient --socket-name=<profile>`.  For a one-off frame,
use `EMACS_PROFILE=spacemacs-full emacsclient-visual.sh`.

## Optional native Wayland build

On Arch, [build-emacs-wayland](../scripts/build-emacs-wayland) builds Emacs
31.1 with PGTK, native compilation ahead-of-time, Tree-sitter, dynamic
modules, link-time optimization, and the available image, database, desktop,
input, sound, and xwidget extensions.

```sh
scripts/build-emacs-wayland
```

It installs the build under `/usr/local/stow/emacs-31.1-pgtk` and uses GNU
Stow to link it into `/usr/local`, without modifying pacman-owned files.
