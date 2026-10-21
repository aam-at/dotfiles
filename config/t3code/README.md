# T3 Code

Settings for T3 Code, linked into `~/.t3/userdata` by `windots` (`setup\Install-Links.ps1`).

- `settings.json`, `client-settings.json`: server and client settings.
- `keybindings.json`: the full rule list, upstream defaults plus `Alt` chords.

## Why chords only

T3 has no modal (vim) mode, and bare printable keys cannot be bound safely:

- Type-to-focus sends any printable key to the composer when nothing editable is
  focused, before shortcuts run.
- The command palette and sidebar handlers ignore `editableFocus`, so a bare key
  fires while typing (`p` opened the file picker).
- Keys typed inside the preview webview are invisible to `editableFocus`.

Only the usage page keeps bare keys (`c`, `t`, `l`, upstream defaults).

## Defaults are kept

T3 drops every upstream default for a command that the file binds. So each
command with an extra chord also lists its default (`Ctrl-k`, `Ctrl-p`, `Ctrl-d`,
...), and the file is the complete list. Re-check it against upstream
`DEFAULT_KEYBINDINGS` after a T3 upgrade.

## Alt chords

Left-hand, Windows (`mod` = `Ctrl`).

| Action | Chord |
|---|---|
| command palette | `Alt-m`, `Ctrl-SPC` |
| file picker / project search | `Alt-p` / `Alt-/` |
| find in chat | `Alt-f` |
| diff (also from the terminal) | `Alt-g` |
| terminal (also from the terminal) | `Alt-'` |
| sidebar / right panel | `Alt-0` / `Alt-w` |
| new chat / new local chat | `Alt-n` / `Alt-Shift-n` |
| next / previous thread | `Alt-j` / `Alt-k` |
| thread 1-9 | `Alt-1`..`Alt-9` |
| back / forward | `Alt-Left` / `Alt-Right` |

The previous `Alt-b` / `Alt-v` terminal splits were dropped: they shadowed the
shell's word-back (`Alt-b`). Use the default `Ctrl-d` / `Ctrl-Shift-d`.

Preview zoom and reload use the default `Ctrl-r`, `Ctrl-=`, `Ctrl--` while the
preview has focus. T3 has no key sequences, so herdr's `Ctrl-a` prefix is dropped.
