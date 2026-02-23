# Niri Config Refactor: Hyprland Parity

**Date:** 2026-02-24
**Status:** Approved

## Goal

Restructure niri configuration to match Hyprland's modular architecture and feature parity. The main `config.kdl` becomes a minimal include-only file, with all settings in `dms/` submodules.

## Current State

- `config.kdl` contains inline sections (input, animations, environment, window-rules, etc.)
- Several `dms/` files exist but some are empty (`windowrules.kdl`, `outputs.kdl`)
- Missing features: named workspaces, comprehensive window rules, input refinements

## Target File Structure

```
config/niri/
├── config.kdl              # Pure includes only
└── dms/
    ├── binds.kdl           # Existing + F1-F5 workspace binds
    ├── colors.kdl          # Existing (auto-generated)
    ├── cursor.kdl          # Existing (auto-generated)
    ├── layout.kdl          # Update: gaps 2→3, rounding 8→10
    ├── alttab.kdl          # Existing (auto-generated)
    ├── wpblur.kdl          # Existing
    ├── input.kdl           # NEW: keyboard, touchpad, mouse config
    ├── animations.kdl      # NEW: spring-based animations
    ├── windowrules.kdl     # Populate: port from Hyprland rules.conf
    ├── outputs.kdl         # Populate: fallback monitor
    ├── environment.kdl     # NEW: env vars (TERMINAL, BROWSER, etc.)
    ├── execs.kdl           # NEW: spawn-at-startup processes
    ├── workspaces.kdl      # NEW: named workspaces
    └── misc.kdl            # NEW: hotkey-overlay, screenshot-path, debug, gestures, overview
```

## Key Changes

### 1. config.kdl → Pure Includes
Strip all inline content. Only `include "dms/*.kdl"` directives remain.

### 2. Window Rules (empty → populated)
Port Hyprland rules.conf to niri equivalents:
- Base visuals: geometry-corner-radius, clip-to-geometry
- Float/tile defaults per app
- Dialog windows: open-floating
- PiP windows: open-floating
- Steam notifications: open-floating, open-focused false, positioned bottom-right
- **Skip:** opacity rules (not supported), blur rules (not supported), tearing (not supported)

### 3. Named Workspaces
Create named workspaces: sysmon, music, communication, ai, todo.
Add F1-F5 bindings to focus them, Shift+F1-F5 to move windows there.

### 4. Input Refinements
- Add repeat-delay 250, repeat-rate 35 (match Hyprland)
- Add touchpad scroll-factor 0.3
- Add disable-while-typing

### 5. Layout Alignment
- Gaps: 2 → 3
- Rounding: 8 → 10

### 6. Startup Execs
- Keep: xremap, kanata
- Add: trash-empty 30

## Known Limitations

- No per-window blur in niri
- No per-window opacity rules in niri
- No allow_tearing equivalent in niri
- Special workspaces (toggle show/hide) don't exist; using named workspaces instead
- No dwindle/master layout toggle; niri is column-based only
