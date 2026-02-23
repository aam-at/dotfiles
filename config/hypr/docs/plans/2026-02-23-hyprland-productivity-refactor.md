# Hyprland Productivity Refactor Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Refactor the Hyprland config for faster keyboard-driven workflows and smoother multitasking while preserving core muscle memory and DMS integrations.

**Architecture:** Keep the existing modular `dms/*.conf` layout and preserve a single `dms/binds.conf`, but restructure that file internally with aliases and strict sections. Pair keybinding cleanup with small rule/startup/output cleanups and light visual-performance tuning, then validate with `hyprctl` reload plus manual smoke tests.

**Tech Stack:** Hyprland config (`.conf`), shell commands (`hyprctl`, `pgrep`, `git`), DMS IPC bindings

---

## Implementation Notes

- This is a config refactor, so there is no unit test harness. Use `hyprctl reload` + targeted `hyprctl` inspections + manual keybind smoke testing as the verification substitute.
- Prefer small commits after each task.
- Preserve the single-file bind constraint: all keybind changes stay in `dms/binds.conf`.
- Relevant process skills for execution:
  - `@superpowers:test-driven-development` (adapt verification-first for config work)
  - `@superpowers:verification-before-completion`
  - `@superpowers:subagent-driven-development` (if executing in this session)

### Task 1: Capture Baseline and Safe Working State

**Files:**
- Modify: none

**Step 1: Record current file snapshots for review**

Run: `git status --short -- dms/binds.conf dms/rules.conf dms/execs.conf dms/outputs.conf dms/animations.conf dms/decoration.conf`
Expected: shows current tracked status for the target files (likely `A` for new files in this repo state)

**Step 2: Capture current bind-related baseline (if in a Hyprland session)**

Run: `hyprctl binds | rg 'SUPER|special|workspace' | head -n 40`
Expected: exits 0 and prints current bind entries; if `hyprctl` is unavailable outside a Hyprland session, note and continue

**Step 3: Capture current `kanata` process count (if running)**

Run: `pgrep -af 'kanata .*config.kbd' | wc -l`
Expected: integer count (commonly `0` or `1`)

**Step 4: Create a task branch/worktree or confirm scoped edits**

Run: `git rev-parse --show-toplevel`
Expected: prints repo root; if staying in the current workspace, explicitly commit with `--only` for safety

**Step 5: Commit (optional checkpoint if you added notes/scripts)**

```bash
# Usually no commit for this task unless files were created.
```

### Task 2: Refactor `dms/binds.conf` Structure and Modifier Aliases

**Files:**
- Modify: `dms/binds.conf`

**Step 1: Establish new section order and modifier aliases at the top**

Add a header block near the top of `dms/binds.conf`:

```conf
# Modifiers
$mod = SUPER
$modShift = SUPER SHIFT
$modCtrl = SUPER CTRL
$modCtrlShift = SUPER SHIFT CTRL
```

Expected: aliases appear before bindings and app variables remain intact.

**Step 2: Reorder existing bind blocks into the approved section sequence**

Reorder in-file sections (without changing command behavior yet):

- launchers/DMS toggles
- security/session
- media/brightness
- window management
- focus/move
- monitor nav/send
- workspace nav/send
- numbered workspaces
- special workspaces (placeholder section if not added yet)
- layout/resize
- mouse drag
- screenshots/system

Expected: file is easier to scan; no functional changes introduced in this step.

**Step 3: Replace repeated `SUPER` prefixes with aliases incrementally**

Example conversions:

```conf
bind = $mod, Return, exec, $terminal
bind = $modShift, Q, killactive
bind = $modCtrl, H, focusmonitor, l
```

Expected: no behavior change; syntax remains valid.

**Step 4: Reload Hyprland to verify syntax**

Run: `hyprctl reload`
Expected: command exits successfully with no config parse errors

**Step 5: Commit**

```bash
git add dms/binds.conf
git commit -m "refactor(hypr): reorganize binds file and add modifier aliases"
```

### Task 3: Add Special Workspace Toggle and Send Binds (Multitasking)

**Files:**
- Modify: `dms/binds.conf`

**Step 1: Add a dedicated "Special Workspaces" section**

Insert a section with named toggles (use mnemonic keys that do not conflict with preserved core binds):

```conf
# === Special Workspaces ===
# Toggle named special workspaces
bind = $mod, F1, togglespecialworkspace, sysmon
bind = $mod, F2, togglespecialworkspace, music
bind = $mod, F3, togglespecialworkspace, communication
bind = $mod, F4, togglespecialworkspace, ai
bind = $mod, F5, togglespecialworkspace, todo
```

Expected: direct toggles exist for all routed special workspaces.

**Step 2: Add send-active-window-to-special-workspace binds with a consistent pattern**

Add matching send binds:

```conf
bind = $modShift, F1, movetoworkspace, special:sysmon
bind = $modShift, F2, movetoworkspace, special:music
bind = $modShift, F3, movetoworkspace, special:communication
bind = $modShift, F4, movetoworkspace, special:ai
bind = $modShift, F5, movetoworkspace, special:todo
```

Expected: manual routing complements existing automatic routing.

**Step 3: Confirm no key collisions with preserved core binds**

Run: `rg -n 'F[1-5]' dms/binds.conf`
Expected: only the new special-workspace entries (or documented intentional F-key uses)

**Step 4: Reload and verify bind presence**

Run: `hyprctl reload && hyprctl binds | rg 'specialworkspace|special:'`
Expected: exits 0; lists the new special workspace toggle/send binds (if in-session)

**Step 5: Commit**

```bash
git add dms/binds.conf
git commit -m "feat(hypr): add direct special workspace toggle and send binds"
```

### Task 4: Normalize Layout/Resize Binds and Remove Hidden Scancode Resizing

**Files:**
- Modify: `dms/binds.conf`

**Step 1: Remove scancode resize bindings**

Delete scancode-only entries:

```conf
bindd = SUPER, code:20, Expand window left, resizeactive, -100 0
bindd = SUPER, code:21, Shrink window left, resizeactive, 100 0
```

Expected: hidden/keyboard-layout-sensitive behavior is eliminated.

**Step 2: Normalize named-key resize step sizes**

Make horizontal/vertical fine/coarse increments consistent. Example target pattern:

```conf
# Coarse resize
binde = $mod, minus, resizeactive, -10% 0
binde = $mod, equal, resizeactive, 10% 0
binde = $modShift, minus, resizeactive, 0 -10%
binde = $modShift, equal, resizeactive, 0 10%

# Fine resize
binde = $modCtrl, minus, resizeactive, -5% 0
binde = $modCtrl, equal, resizeactive, 5% 0
binde = $modCtrlShift, minus, resizeactive, 0 -5%
binde = $modCtrlShift, equal, resizeactive, 0 5%
```

Expected: `CTRL+SHIFT` vertical steps are meaningfully finer than non-CTRL vertical steps.

**Step 3: Keep existing layout controls unless conflicting**

Retain:

```conf
bind = $mod, R, layoutmsg, togglesplit
bind = $modCtrl, F, resizeactive, exact 100%
```

Expected: core layout muscle memory remains unchanged.

**Step 4: Reload and manually test resize behavior**

Run: `hyprctl reload`
Expected: successful reload; manual test confirms coarse/fine horizontal and vertical resize all work

**Step 5: Commit**

```bash
git add dms/binds.conf
git commit -m "refactor(hypr): normalize resize binds and remove scancode resizing"
```

### Task 5: Reorganize and Deduplicate `dms/rules.conf`

**Files:**
- Modify: `dms/rules.conf`

**Step 1: Reorder rules into the approved logical blocks**

Reorder existing rules into:

1. base visuals / layer rules / generic centering
2. app tile/float defaults
3. float+size+center groups
4. special workspace routing
5. dialogs
6. PiP
7. Steam/games
8. app-specific/XWayland quirks

Expected: readability improves without intentional behavior changes.

**Step 2: Deduplicate repeated app rules**

Consolidate duplicated/overlapping entries (examples to review):

- `blueman-manager` (appears multiple times)
- `pavucontrol` / `org\\.pulseaudio\\.pavucontrol` (tile vs float/size rules)
- `org.quickshell` comments/rules for float/opaque behavior

Expected: each app class has a single clear default rule path unless multiple rules are intentionally layered.

**Step 3: Add comments linking special-workspace routing to manual binds**

Add a short comment such as:

```conf
# Routed automatically; manual toggle/send binds are defined in dms/binds.conf
```

Expected: routing and keybinding behavior are documented as one system.

**Step 4: Reload and spot-check rule-sensitive apps**

Run: `hyprctl reload`
Expected: successful reload; manually verify `pavucontrol`, GNOME Settings, and a PiP window still float/size correctly

**Step 5: Commit**

```bash
git add dms/rules.conf
git commit -m "refactor(hypr): reorganize and deduplicate window rules"
```

### Task 6: Make Startup Reload-Safe and Clean Outputs Fallback

**Files:**
- Modify: `dms/execs.conf`
- Modify: `dms/outputs.conf`

**Step 1: Guard `kanata` startup against duplicate launches**

Replace the raw command with a duplicate-safe wrapper (example):

```conf
exec-once = sh -lc 'pgrep -af "kanata .*config.kbd" >/dev/null || kanata -c "$HOME/dotfiles/config/kanata/config.kbd"'
```

Expected: `kanata` still starts on first session start but does not duplicate on reload/restart edge cases.

**Step 2: Keep startup tasks but clarify sections**

Group `exec-once` entries into:

- session environment init
- non-critical maintenance (`trash-empty 30`)
- input tooling (`kanata`)

Expected: future maintenance is easier without behavior change.

**Step 3: Remove duplicate monitor fallback in `dms/outputs.conf`**

Keep a single generic fallback, e.g.:

```conf
monitor = , preferred, auto, 1
```

Expected: no duplicate default monitor declaration.

**Step 4: Reload and verify no duplicate `kanata`**

Run: `hyprctl reload && pgrep -af 'kanata .*config.kbd'`
Expected: one `kanata` process line (or none if intentionally not running in current environment)

**Step 5: Commit**

```bash
git add dms/execs.conf dms/outputs.conf
git commit -m "fix(hypr): guard kanata startup and remove duplicate monitor fallback"
```

### Task 7: Tighten Animations and Blur for Faster Perceived Response

**Files:**
- Modify: `dms/animations.conf`
- Modify: `dms/decoration.conf`

**Step 1: Reduce animation durations slightly**

Adjust animation speeds while preserving visual feedback. Example target:

```conf
animation = windowsIn, 1, 2.5, default
animation = windowsOut, 1, 2.5, default
animation = workspaces, 1, 4, default
animation = windowsMove, 1, 3, default
animation = fade, 1, 2.5, default
animation = border, 1, 2.5, default
```

Expected: transitions feel snappier without turning animations off.

**Step 2: Trim blur cost modestly**

Adjust blur parameters conservatively. Example target:

```conf
blur {
  size = 6
  passes = 1
}
```

Expected: slightly lower GPU cost; visual style remains acceptable.

**Step 3: Preserve all other decoration behavior unless needed**

Do not change rounding/shadows unless validation shows a problem.

Expected: visual identity remains stable.

**Step 4: Reload and visually inspect**

Run: `hyprctl reload`
Expected: successful reload; animations/blur still look acceptable in regular use

**Step 5: Commit**

```bash
git add dms/animations.conf dms/decoration.conf
git commit -m "perf(hypr): tighten animations and reduce blur cost"
```

### Task 8: Final Validation, Regression Check, and Documentation Comments

**Files:**
- Modify: `dms/binds.conf` (optional final comments only)
- Modify: `dms/rules.conf` (optional final comments only)

**Step 1: Run final syntax reload**

Run: `hyprctl reload`
Expected: exits 0 with no parse errors

**Step 2: Run targeted inspections**

Run: `hyprctl binds | rg 'F[1-5]|workspace|movefocus|movewindow|resizeactive' | head -n 80`
Expected: new special-workspace binds are present and core focus/move/resize binds remain

**Step 3: Manual smoke test checklist**

Validate in-session:

- `SUPER+Return` / `SUPER+T` terminal launch
- HJKL and arrow focus movement
- `SUPER+SHIFT+HJKL` and arrows window movement
- numbered workspaces + send-to-workspace
- special workspace toggle/send (F1-F5 pattern)
- screenshots, media, brightness
- DMS toggles (`spotlight`, `clipboard`, `notifications`, `overview`, `powermenu`)

Expected: no core workflow regression.

**Step 4: Final process safety checks**

Run: `pgrep -af 'kanata .*config.kbd' | wc -l`
Expected: `0` or `1` (not multiple duplicates)

**Step 5: Commit**

```bash
git add dms/binds.conf dms/rules.conf dms/execs.conf dms/outputs.conf dms/animations.conf dms/decoration.conf
git commit -m "refactor(hypr): improve keyboard workflow and multitasking ergonomics"
```
