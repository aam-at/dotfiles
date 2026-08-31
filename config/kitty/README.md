# kitty keybindings

Reference for `kitty.conf`. `kitty_mod` is **not** overridden here, so it's
kitty's default: `ctrl+shift`. This doc lists every binding actually active —
both this config's own `map` lines and kitty's own defaults (marked
*default*) that are still live because nothing here overrides them.

## Panes / windows

| Key | Action |
|---|---|
| `ctrl+-` | split horizontal (new pane below, inherits cwd) |
| `ctrl+\` | split vertical (new pane beside, inherits cwd) |
| `ctrl+shift+enter` *(default)* | new window (split, in current layout) |
| `ctrl+shift+w` *(default)* | close window |
| `ctrl+]` / `ctrl+[` | next / previous window |
| `ctrl+shift+]` / `ctrl+shift+[` *(default)* | next / previous window (same, different key) |
| `ctrl+h` / `ctrl+j` / `ctrl+k` / `ctrl+l` | focus neighboring window left/down/up/right — **vim-aware**: if the pane's foreground process is vim/nvim, the raw key is forwarded to it instead, so `vim-tmux-navigator` handles it (see `smart_pane_nav.py`) |
| `ctrl+shift+left/right/up/down` | focus neighboring window (arrow-key variant; shadows the default tab-switch on `ctrl+shift+left/right`, intentional) |
| `ctrl+shift+f7` *(default)* | focus window via visual picker (labels overlay) |
| `shift+left` / `shift+right` | move window left / right |
| `alt+k` / `alt+j` | move window up / down |
| `alt+h` / `alt+l` | move window forward / backward (cycle order) |
| `ctrl+shift+f8` *(default)* | swap window with another, visually |
| `` alt+` `` | move window to top |
| `ctrl+shift+r` *(default)* | start resizing window (arrow keys resize, enter/esc exit) |
| `F8` | rotate split direction (splits layout only) |
| `ctrl+shift+l` *(default)* | next layout (cycles the 7 below) |
| `ctrl+alt+z` | toggle zoom — jump to `stack` (current pane fullscreen), press again to go back |
| `ctrl+shift+f10` *(default)* | toggle OS window maximized |
| `f11` | toggle fullscreen (OS window) |
| `ctrl+shift+f11` *(default)* | toggle fullscreen (same, default key) |
| `ctrl+shift+escape` *(default)* | open kitty's internal command shell |

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

| Key | Action |
|---|---|
| `ctrl+shift+t` | new tab (inherits cwd) |
| `ctrl+shift+q` *(default)* | close tab |
| `ctrl+tab` / `ctrl+shift+tab` *(default)* | next / previous tab |
| `ctrl+shift+d` | select tab (interactive picker) |
| `F2` | rename tab |
| `ctrl+shift+.` / `ctrl+shift+,` *(default)* | move tab forward / backward |
| `ctrl+shift+1`…`9` | goto tab N (shadows the default "jump to Nth **window**" on these same keys — deliberate, tabs are used far more) |
| `ctrl+shift+0` *(default)* | jump to 10th window in current tab (still reachable, only 1-9 are shadowed) |
| `ctrl+shift+n` | new OS window (inherits cwd) |

## Scrollback / search

| Key | Action |
|---|---|
| `ctrl+shift+h` *(default)* | open scrollback in pager (`less -R -F`) |
| `ctrl+shift+page_up` / `page_down` *(default)* | scroll one page up/down |
| `ctrl+shift+j` / `ctrl+shift+k` *(default)* | scroll one line down/up (note: different from plain `ctrl+j`/`ctrl+k`, which are pane-nav) |
| `shift+up/down`, `shift+page_up/page_down`, `ctrl+shift+home/end` | smart-scroll: scrolls kitty's buffer normally, but forwards the raw keypress to the foreground app instead when it's a full-screen program (vim, less, fzf, htop, …) so those apps handle it themselves |
| `ctrl+shift+z` *(default)* | scroll to previous shell prompt (needs shell integration, on by default) |
| `ctrl+shift+x` *(default)* | scroll to next shell prompt |
| `ctrl+shift+g` *(default)* | show **just the last command's output** in a pager |
| `ctrl+shift+/` *(default)* | kitty's own scrollback search |
| `F3` | open interactive scrollback search (`search.py` kitten) in a split — a different, custom search UI, complements the one above |

## Hints kitten (grab things off-screen by keyboard)

| Key | Action |
|---|---|
| `ctrl+shift+e` *(default)* | open a URL via hints picker |
| `ctrl+shift+p` then `f` *(default)* | insert selected path |
| `ctrl+shift+p` then `shift+f` *(default)* | open selected path (routed through `open-actions.conf` → `vim`/`$EDITOR`, see below) |
| `ctrl+shift+p` then `c` / `d` *(default)* | choose a file / directory (fuzzy picker), insert path |
| `ctrl+shift+p` then `l` *(default)* | insert selected line |
| `ctrl+shift+p` then `w` *(default)* | insert selected word |
| `ctrl+shift+p` then `h` *(default)* | insert selected hash (e.g. a git commit) |
| `ctrl+shift+p` then `n` *(default)* | open the selected file at the selected line number |
| `ctrl+shift+p` then `y` *(default)* | open the selected hyperlink |

Clicking a `file://` link (or a path matched by `open-actions.conf`) opens it
in `vim` at the right line if the link carries a `#<num>` fragment, otherwise
in `$EDITOR` (`nvim`).

## Clipboard / editing

| Key | Action |
|---|---|
| `ctrl+shift+v` *(default)* | paste from clipboard |
| `ctrl+shift+s` *(default)* | paste from primary selection |
| `middle-click` | paste from selection (mouse) |
| `ctrl+c` | copy selection, or send interrupt if nothing selected |
| `ctrl+backspace` | send `^W` (delete word, works even if terminal binding is eaten) |
| `ctrl+shift+c` | clear terminal (scroll history to cursor) |
| `ctrl+shift+delete` *(default)* | full terminal reset |
| `ctrl+shift+o` *(default)* | pass current selection to a program |

## Font / background / misc

| Key | Action |
|---|---|
| `ctrl+shift+equal` / `+` / `kp_add` *(default)*, `ctrl+KP_+` | increase font size |
| `ctrl+shift+minus` / `kp_subtract` *(default)*, `ctrl+KP_-` | decrease font size |
| `ctrl+shift+backspace` *(default)*, `ctrl+KP_0` | restore font size |
| `ctrl+shift+a` then `m` / `l` / `1` / `d` *(default)* | increase / decrease / max / reset background opacity live (works because `dynamic_background_opacity yes` is set) |
| `ctrl+shift+u` *(default)* | unicode input picker |
| `ctrl+shift+f1` *(default)* | open kitty documentation |
| `ctrl+shift+f3` *(default)* | command palette — fuzzy-searchable list of every kitty action, run any of them by name without memorizing a key |
| `f5` | reload config |
| `ctrl+shift+f5` *(default)* | reload config (same, default key) |
| `f6` | show resolved config (debug) |
| `ctrl+shift+f6` *(default)* | show resolved config (same, default key) |
| `ctrl+shift+f2` *(default)* | edit `kitty.conf` in `$EDITOR` |

## Disabled

`alt+0`…`alt+9` and `ctrl+enter` are bound to `no-op` — swallowed by kitty,
never reach the shell or terminal apps. (Historically these used to be
kitty's default first-through-ninth-window keys; that default has since moved
to `ctrl+shift+1`…`9`, which this config repurposes for `goto_tab` anyway —
so this block mostly just blocks stray `alt+number` keypresses from leaking
into whatever's running. Revisit if it turns out to fight your window
manager's own `alt+number` bindings.)

## Files

| File | Purpose |
|---|---|
| `kitty.conf` | main config, all keybindings |
| `~/.config/theme/kitty.conf` | active generated color scheme |
| `diff.conf` | config for `kitty +kitten diff` |
| `open-actions.conf` | rules for what happens when you click a link/path |
| `search.py` | scrollback search kitten (`F3`) |
| `smart_scroll.py` | context-aware scroll kitten (see Scrollback section) |
| `smart_pane_nav.py` | vim-aware pane-nav kitten (see Panes section) — the counterpart living on the vim side is `christoomey/vim-tmux-navigator`, configured in `config/lazyvim/lua/plugins/extras.lua` |
