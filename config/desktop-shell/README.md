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
```

The selector changes only `config/niri/shells/active.kdl` and
`config/hypr/shells/active.conf`. It never edits the DMS-generated files in
`dms/`.

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
