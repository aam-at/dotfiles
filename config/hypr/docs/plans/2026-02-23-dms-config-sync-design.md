# DMS Config Sync Design

## Goal

Update all `dms/*.conf` files so they reflect the split Hyprland configuration in `hyprland/`, plus shared values in `variables.conf` and `scheme/current.conf`, while preserving DMS-specific behavior where direct parity is not possible.

## User Decisions

- Sync scope: all `dms/*.conf` files (`colors`, `layout`, `outputs`, `cursor`, `windowrules`, `binds`)
- Bind policy: prioritize Hyprland key combinations first, then use the closest DMS action when exact behavior is unavailable

## Source of Truth

- `hyprland/*.conf` for feature-specific Hyprland settings
- `variables.conf` for shared Hyprland values (gaps, borders, rounding, cursor, keybind variables)
- `scheme/current.conf` for current color values
- `hyprland.conf` only as a fallback for monitor lines because there is no `hyprland/outputs.conf`

## Sync Policy

- Treat `hyprland/` + `variables.conf` + `scheme/current.conf` as the canonical source.
- Update every `dms/*.conf` file.
- Prefer nearest DMS-compatible equivalents over literal Hyprland-only commands.
- If no clean mapping exists, keep the existing DMS line or add a short comment.
- Preserve DMS IPC commands in `dms/binds.conf` wherever possible.

## Per-file Mapping Rules

### `dms/layout.conf`

Map from `variables.conf`:

- `general.gaps_in` <- `$windowGapsIn`
- `general.gaps_out` <- `$windowGapsOut`
- `general.border_size` <- `$windowBorderSize`
- `decoration.rounding` <- `$windowRounding`

### `dms/colors.conf`

Map from `scheme/current.conf`:

- `$primary` -> `rgb(...)`
- `$outline` -> `rgb(...)`
- `$error` -> `rgb(...)`

Keep the existing DMS file structure (`general` and `group` sections). Do not attempt to derive alpha-suffixed color variables (for example `$primaryd4`, `$primarye6`) because they are not defined in the checked-in sources used for this sync.

### `dms/outputs.conf`

- Copy active `monitor = ...` lines from `hyprland.conf` (fallback source).
- If no active lines are found, keep the existing DMS default line.

### `dms/cursor.conf`

Populate using a DMS-compatible subset from `hyprland/env.conf`, `hyprland/input.conf`, and `variables.conf`:

- `env = XCURSOR_THEME, ...`
- `env = XCURSOR_SIZE, ...`
- `cursor { hotspot_padding = ... }`

Do not copy runtime startup commands (`exec-once = hyprctl setcursor ...`) into this file.

### `dms/windowrules.conf`

- Populate with a curated subset of `windowrule = ...` lines from `hyprland/rules.conf`.
- Exclude `workspace = ...` and `layerrule = ...` directives.
- Preserve section comments where helpful.
- Resolve `$windowOpacity` to a literal value from `variables.conf` when copied.

### `dms/binds.conf`

- Preserve DMS IPC actions where possible.
- Change key combinations to match `hyprland/keybinds.conf` when there is a DMS-equivalent action.
- Keep DMS-only functionality, but move it off conflicting keys when needed.
- Avoid replacing DMS actions with `caelestia` / `app2unit` commands unless no DMS equivalent exists and leaving the old binding is worse than keeping DMS behavior.

## Edge Cases

- Missing source values: keep the existing DMS value and annotate with a short comment.
- Multiple monitor entries: copy all active `monitor = ...` lines in order.
- Unsupported Hyprland-only behaviors in binds: preserve existing DMS bindings or use an approximate DMS action.
- `dms/windowrules.conf` remains `windowrule`-only; layer/workspace rules stay out of scope.

## Verification Plan

- Review diffs for all `dms/*.conf` files and confirm mappings match the chosen sources.
- Run a sanity scan for unresolved variables accidentally copied into DMS files where literal values were intended.
- Do not claim runtime validity without a Hyprland reload and runtime check.
