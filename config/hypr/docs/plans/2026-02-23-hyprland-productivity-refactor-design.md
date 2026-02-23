# Hyprland Productivity Refactor Design

**Date:** 2026-02-23
**Scope:** `/home/amatyasko/dotfiles/config/hypr`

## Goal

Refactor the Hyprland configuration for better keyboard throughput and lower-friction multitasking while preserving core muscle memory and existing DMS integrations.

## User Priorities (Approved)

- Primary: keyboard throughput
- Primary: low-friction multitasking
- Refactor style: balanced (preserve core shortcuts, rework inconsistent flows)
- Constraint: keep a single `dms/binds.conf` file (no split bind modules)

## Current State Summary

- Modular config with `hyprland.conf` sourcing `dms/*.conf`.
- Strong existing foundation for DMS launchers, HJKL navigation, workspace routing, and media controls.
- Main maintenance risk is `dms/binds.conf` size and inconsistency.
- `dms/rules.conf` has useful routing and UI behavior but includes repeated or overlapping rules.
- `dms/outputs.conf` contains duplicate generic monitor fallback lines.

## Chosen Approach

Balanced workflow refactor:

- Keep core launcher and workspace muscle memory intact.
- Standardize keybinding patterns for window/focus/move/monitor/workspace actions.
- Add explicit special-workspace controls for multitasking.
- Clean up duplicate/conflicting rules and reload-sensitive startup commands.
- Apply light performance tuning only where non-disruptive.

## Design

### 1. Keymap and Multitasking Model

Preserve the existing mental model:

- `SUPER` for focus, actions, and toggles
- `SUPER+SHIFT` for move/send operations
- `SUPER+CTRL` for routing across monitors/workspace contexts
- Number row workspace switching and send-to-workspace
- HJKL and arrow-key navigation/movement

Refactor behavior:

- Keep core app launchers (`terminal`, `browser`, `editor`, file explorer) unchanged.
- Keep DMS toggles unchanged and complement them with workspace-level multitasking binds.
- Remove cryptic scancode-only resize bindings and use named keys only.
- Normalize resize step sizes (fine vs coarse, horizontal vs vertical) for predictability.
- Retain ergonomic duplicates only when intentional (e.g. `SUPER+T` and `SUPER+Return`).

### 2. Single-File Bind Hygiene (`dms/binds.conf`)

Do not split the file. Instead, restructure internally with a strict section order:

1. app variables and modifier aliases
2. launchers and DMS toggles
3. security and session controls
4. media and brightness
5. window management
6. focus/move (HJKL + arrows)
7. monitor navigation and send-to-monitor
8. workspace navigation and send-to-workspace
9. numbered workspaces
10. special workspaces (toggle + send)
11. layout and resize
12. mouse drag bindings
13. screenshots and system controls

Additional hygiene:

- Introduce aliases such as `$mod`, `$modShift`, and `$modCtrl`.
- Add minimal comments only where behavior is not obvious.
- Remove or annotate duplicate bindings.

### 3. Special Workspaces (Multitasking)

Keep existing automatic routing in `dms/rules.conf` for:

- `special:sysmon`
- `special:music`
- `special:communication`
- `special:ai`
- `special:todo`

Add manual controls in `dms/binds.conf`:

- Direct toggle binds for each special workspace
- Direct send-active-window-to-special-workspace binds using a consistent modifier pattern

Rationale:

- Auto-routing handles routine app placement.
- Manual binds cover ad-hoc workflows and temporary context switching.

### 4. Rules Cleanup (`dms/rules.conf`)

Reorganize rules into clear blocks without intentionally changing core behavior:

- base visuals and general behavior (opacity, centering, layer rules)
- app tiling/floating defaults
- dialog/pop-up rules
- special workspace routing
- PiP rules
- Steam/gaming rules
- XWayland and application-specific quirks

Cleanup targets:

- Deduplicate repeated classes (e.g. `blueman-manager`)
- Consolidate overlapping `pavucontrol` rules to avoid drift
- Clarify `org.quickshell` handling comments and rule intent

### 5. Startup and Output Cleanup

`dms/execs.conf`:

- Keep existing startup behavior (`dbus-update-activation-environment`, `hyprland-session.target`, trash cleanup, `kanata`)
- Make `kanata` startup reload-safe to avoid duplicate processes on reload/session restarts

`dms/outputs.conf`:

- Remove duplicate generic fallback monitor line and keep one fallback entry

### 6. Light Performance Tuning

Keep current visual style but tune for snappier response:

- Slightly faster animation timings (windows/workspaces/fade)
- Modest blur cost reduction only if visual quality remains acceptable

No aggressive visual downgrades are included in this refactor.

## Non-Goals

- Replacing DMS commands/endpoints
- Full submap-driven keybinding redesign
- Per-monitor explicit output profiles
- Aggressive performance-only simplification

## Validation Plan

After implementation:

- Reload Hyprland config with `hyprctl reload` (or equivalent safe reload path in-session)
- Manual smoke test of:
  - app launchers and DMS toggles
  - HJKL/arrow focus and movement
  - workspace navigation and send-to-workspace
  - special workspace toggle/send binds
  - media/brightness/screenshot keys
- Confirm no duplicate `kanata` process after reload
- Verify common float/dialog/PiP rules still behave as expected

## Risks and Mitigations

- Risk: muscle-memory regressions on secondary shortcuts
  - Mitigation: preserve core binds and change only inconsistent/low-visibility binds
- Risk: rule cleanup accidentally changes app behavior
  - Mitigation: reorder carefully and validate key apps after reload
- Risk: reload-safe startup wrapper changes command semantics
  - Mitigation: keep command behavior identical, only add duplicate-process guard
