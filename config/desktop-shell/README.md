# Desktop-shell profiles

This directory selects the desktop shell independently of the compositor, in
the same spirit as separate Spacemacs and Doom configurations. Both Niri and
Hyprland use the same active profile.

`dms` is the default and retains the existing Dank Material Shell setup.
`noctalia` starts the current native Noctalia release and uses the same
high-value DMS shortcuts where Noctalia has an equivalent.

Switch profiles, then reload or restart the compositor:

```sh
desktop-shell dms
desktop-shell noctalia
desktop-shell status
# Reload Niri and replace the running shell without logging out.
desktop-shell noctalia --restart-niri
desktop-shell dms --restart-niri
```

The selector changes only the local, Git-ignored `config/niri/shells/active.kdl`
and `config/hypr/shells/active.conf` files. Dotbot creates both with Noctalia
selected after linking; the selector never edits the DMS-generated files in
`dms/`.

The selector reloads Niri's configuration by default so keybindings and
compositor rules apply immediately. Add `--restart-niri` to also replace the
active shell: it stops DMS before starting Noctalia, or stops Noctalia and
restarts DMS. The DMS restart replaces its QuickShell child as well. This does
not restart the Niri compositor or end the graphical session.

Both profiles load the same compositor keymap (terminal and application
launchers, directional focus/move, workspace movement, layout presets,
overview, and monitor controls). Shell overlays only provide features the
shell owns: launcher, clipboard, notifications, settings, media, brightness,
wallpaper, session menu, lock screen, and screenshots.

For Noctalia, use `~/.config/noctalia/` for curated TOML settings and its
settings UI for live adjustments. Match the existing DMS look with a blue
accent, 12px rounded surfaces, low-transparency panels, and the stationary
wallpaper option. GUI overrides live outside this repository, so profile files
remain stable across Noctalia releases.
