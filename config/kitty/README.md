# kitty keybindings

Custom overrides in `kitty.conf`; `kitty_mod` remains kitty's default, `ctrl+shift`. Built-in bindings are intentionally omitted; use kitty's command palette (`ctrl+shift+f3`) when needed.

## Panes / windows

| Key | Action |
|---|---|
| `ctrl+-` | split horizontal (new pane below, inherits cwd) |
| `ctrl+\` | split vertical (new pane beside, inherits cwd) |
| `ctrl+]` / `ctrl+[` | next / previous window |
| `shift+left` / `shift+right` | move window left / right |
| `alt+k` / `alt+j` | move window up / down |
| `alt+h` / `alt+l` | move window forward / backward (cycle order) |
| `` alt+` `` | move window to top |
| `F8` | rotate split direction (splits layout only) |
| `ctrl+alt+z` | toggle zoom — jump to `stack` (current pane fullscreen), press again to go back |
| `f11` | toggle fullscreen (OS window) |

### Layouts (`enabled_layouts`)

`splits` (default/startup) → `tall` → `fat` → `grid` → `horizontal` →
`vertical` → `stack`, cycled by `ctrl+shift+l`. All 7 of kitty's layouts are
enabled — nothing held back:

- **splits** — freeform, manual `ctrl+-`/`ctrl+\` splits (main daily driver)
- **tall** / **fat** — one large pane + a stack of small ones, vertically /
  horizontally
- **grid** — even grid, auto-sized to window count
- **horizontal** / **vertical** — every window in one row / one column
- **stack** — one fullscreen window at a time (this is what `ctrl+alt+z` zooms into)

## Tabs

The bar hides within 0.5 seconds when the focused pane runs local tmux or
HerdR, and returns for other foreground programs (with two or more tabs).
`ctrl+shift+d` opens the tab picker even while the bar is hidden.

| Key | Action |
|---|---|
| `ctrl+shift+t` | new tab (inherits cwd) |
| `ctrl+shift+d` | select tab (interactive picker) |
| `F2` | rename tab |
| `ctrl+shift+n` | new OS window (inherits cwd) |

## Scrollback / search

| Key | Action |
|---|---|
| `F3` | open Kitty's native scrollback search (same action as `kitty_mod+/`) |
| `shift+up/down`, `shift+page_up/page_down`, `ctrl+shift+home/end` | smart-scroll: scrolls kitty's buffer normally, but forwards the raw keypress to the foreground app instead when it's a full-screen program (vim, less, fzf, htop, …) so those apps handle it themselves |

## Hints kitten (grab things off-screen by keyboard)

Clicking a `file://` link (or a path matched by `open-actions.conf`) opens it
in `vim` at the right line if the link carries a `#<num>` fragment, otherwise
in `$EDITOR` (`nvim`).

## Clipboard / editing

| Key | Action |
|---|---|
| `middle-click` | paste from selection (mouse) |
| `ctrl+c` | copy selection, or send interrupt if nothing selected |
| `ctrl+backspace` | send `^W` (delete word, works even if terminal binding is eaten) |
| `ctrl+shift+c` | clear terminal (scroll history to cursor) |

## Font / background / misc

| Key | Action |
|---|---|
| `f5` | reload config |
| `f6` | show resolved config (debug) |
| `f7` | launch HerdR in a new OS window |

## Disabled

`alt+0`…`alt+9` and `ctrl+enter` are bound to `no-op` so they do not leak into terminal applications.

## Files

| File | Purpose |
|---|---|
| `kitty.conf` | main config, all keybindings |
| `~/.config/theme/kitty.conf` | active generated color scheme |
| `diff.conf` | config for `kitty +kitten diff` |
| `open-actions.conf` | rules for what happens when you click a link/path |
| `auto_hide_tab_bar.py` | hide the tab bar while tmux or HerdR owns the focused pane |
| `smart_scroll.py` | context-aware scroll kitten (see Scrollback section) |
