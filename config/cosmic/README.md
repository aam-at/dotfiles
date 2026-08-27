# COSMIC DE

COSMIC settings that mirror the shared niri/Hyprland workflow where COSMIC has an equivalent. COSMIC is a separate desktop environment, not part of the `scripts/desktop-shell` selector, so its config lives here instead of under `niri/` or `hypr/`.

The repository owns the COSMIC application namespaces below `~/.config/cosmic`, while `~/.config/cosmic` itself remains a real directory. Dotbot links each tracked `com.system76.*` directory into `config/cosmic`. This keeps the root path available for COSMIC's config service and inotify watchers while still making changes from COSMIC Settings land directly in the dotfiles working tree.

## Tiling and workspaces

`com.system76.CosmicComp/v1/` makes COSMIC behave primarily as a tiling compositor:

- `autotile = true` enables automatic tiling.
- `autotile_behavior = PerWorkspace` keeps new workspaces tiled while preserving COSMIC's per-workspace toggle when a floating-only workspace is useful.
- `workspaces` uses output-bound, vertical workspaces with wraparound, matching the vertical workspace/navigation model used by niri and Hyprland.
- `focus_follows_cursor = true` with a 250 ms delay gives pointer focus without making incidental pointer crossings too aggressive.
- `cursor_follows_focus = false` keeps keyboard navigation from warping the pointer.
- `active_hint = true` keeps the focused tile visually obvious.

## Dock overlay / full-height windows

`com.system76.CosmicPanel.Dock/v1/` configures the dock as a non-exclusive top-layer surface:

- `exclusive_zone = false`
- `layer = Top`
- `autohide = OnOverlap`
- `keep_style_on_maximize = true`

Current COSMIC panel code treats a non-autohiding panel as exclusive even when `exclusive_zone` is false. `OnOverlap` is therefore required for tiled/maximized windows to use the full output height. The dock remains easy to reveal but stops shrinking the tiling area, and its floating styling is retained around maximized windows.

Dock position, size, opacity, applets, and theme styling are part of the tracked COSMIC snapshot as well. COSMIC Settings remains the preferred way to edit them; the resulting changes simply appear in git.

## Input

The compositor config carries the shared desktop input behavior:

- XKB layouts: `us,ru`, switched with Alt+Shift; Right Alt is Compose and Caps Lock swaps with Escape.
- Keyboard repeat: 250 ms delay, 35 repeats/sec.
- NumLock on at login.
- Touchpad: adaptive acceleration, clickfinger, disable-while-typing, tap-to-click/drag, two-finger natural scrolling, and 0.3 scroll factor.

Device-specific overrides remain unmanaged.

## Keybindings

`com.system76.CosmicSettings.Shortcuts/v1/custom` is merged on top of COSMIC's shipped defaults. Only listed bindings are added or overridden; all other defaults stay intact.

The bindings follow `config/niri/common/binds.kdl` and `config/hypr/common/binds.conf` where COSMIC exposes matching actions:

- Launchers: Super+Return / Super+T `kitty`, Super+E `thunar`, Super+B Edge, Super+C `emacsclient-visual.sh`, Super+Space launcher, Super+D workspace overview.
- Clipboard: Super+V toggles Clippy Land, matching the DMS and Noctalia clipboard overlay.
- Session: Super+Shift+E log out, Super+Alt+L lock screen.
- Window: Super+F maximize, Super+Shift+F fullscreen, Super+Shift+T floating.
- Workspaces: Super+U/I/Page_Down/Page_Up next/previous; Super+Ctrl and Super+Shift variants move the active window/workspace as documented in the shortcut file.
- Numbered workspaces: Super+1-0 focuses 1-10 and Super+Shift+1-0 sends the active window, matching both Niri and Hyprland.
- Monitors: Super+Ctrl+HJKL/arrows focus, Super+Shift+Ctrl+HJKL/arrows send.
- Resize: Super+= / Super+- grow/shrink.

COSMIC now supports arbitrary `Spawn(...)` custom shortcuts, so browser/editor launchers no longer need to be omitted.

## Optional productivity applets

COSMIC panel applets are installed separately from this dotfile repository.
Their panel positions and ordering are tracked as part of the complete COSMIC
configuration snapshot.

### Clipboard history: Clippy Land

Clippy Land provides searchable clipboard history and pinned text/image items.
It is the COSMIC counterpart to the DMS/Noctalia clipboard overlay. The managed
Super+V shortcut runs its Flatpak ID, `io.github.k33wee.clippy-land`.

On Arch-based systems, install Flatpak once (requires administrator access):

```sh
sudo pacman -S flatpak
flatpak remote-add --if-not-exists --user flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak remote-add --if-not-exists --user cosmic https://apt.pop-os.org/cosmic/cosmic.flatpakrepo
curl -LO https://github.com/k33wee/clippy-land/releases/download/v1.0.0/clippy-land_1.0.0.flatpak
flatpak install --user ./clippy-land_1.0.0.flatpak
```

Log out and back in if COSMIC Settings does not list it immediately, then add
**Clippy Land** under **Desktop → Panel (or Dock) → Configure applets**. Its
normal panel popup opens from the applet; Super+V uses the applet's documented
full-width shortcut surface because COSMIC has no pointer-less panel-popup API.

### Night Light

COSMIC's compositor does not currently expose the Wayland gamma-control
protocol, so Redshift cannot provide a night-light effect in a COSMIC session.
COSMIC Night Light supplies a scheduled, COSMIC-native panel applet instead:

```sh
curl -LO https://github.com/cosmic-nightlight/cosmic-nightlight/releases/download/v0.5.0/cosmic-nightlight-0.5.0.flatpak
flatpak install --user ./cosmic-nightlight-0.5.0.flatpak
```

Add **Night Light** through the same panel-apps screen. At first enable, it
installs a host-side DRM helper through `pkexec`; approve that prompt only after
reviewing it, and ensure the account is in the system's `wheel` or `sudo`
group. The applet can schedule sunset-to-sunrise or fixed hours and offers a
background mode when it is not on a panel.

### Remaining compositor-specific gaps

These still do not have a clean COSMIC equivalent in the current setup:

- Super+Grave reusable scratch terminal (the existing scripts depend on niri/Hyprland window IPC).
- Super+Shift+P direct compositor DPMS action.
- Direct niri-style column width presets and consume/expel column operations.
- Hyprland named special workspaces (Super+F1-F5).
- Mouse-wheel workspace/column compositor bindings.

## Autostart

`autostart/trash-empty.desktop` runs `trash-empty 30` at login, matching the niri and Hyprland startup hooks.

## Applying the configuration

COSMIC has its own `install.conf.yaml` in this directory. The repository-level
`./install` script discovers component manifests automatically and runs each one
relative to the directory that contains it, so the COSMIC install rules stay
local to this configuration instead of growing the main manifest.

Do not symlink `~/.config/cosmic` itself. COSMIC owns and watches that root, and
replacing the whole directory with a symlink is not a reliable configuration
boundary.

Instead, the install manifest keeps `~/.config/cosmic` as a normal directory and
force-links each tracked `com.system76.*` application directory below it. This
also avoids linking individual setting files: COSMIC may recreate a setting file
while saving it, whereas recreating a file inside a symlinked application
directory still writes into the repository.

The installer also removes the obsolete whole-root symlink from earlier versions
of this profile, if present, and recreates `~/.config/cosmic` as a real directory
before linking the application namespaces.

Before first adopting an existing COSMIC installation, copy its current
`~/.config/cosmic/com.system76.*` directories into this repository and review
the diff. Running Dotbot after that replaces those application directories with
links to the copies in this repository. Any new top-level COSMIC namespace added
by a later COSMIC release should likewise be copied here and the installer rerun.

Apply the directory-link migration while logged out of COSMIC when practical,
or log out and back in immediately afterward. Long-running COSMIC components
watch config paths with inotify, so an already-running session can retain watches
on directories that the installer has just replaced.
