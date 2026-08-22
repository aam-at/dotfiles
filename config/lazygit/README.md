# lazygit config

`config.yml` rides lazygit's stock keybindings (v0.64.1) with a small set of
non-default settings and one custom command.

## Non-default settings

| Setting | Value | Why |
|---|---|---|
| `gui.showRandomTip` | `false` | No startup noise. |
| `gui.nerdFontsVersion` | `"3"` | Terminal font is JetBrainsMono Nerd Font — enables file/branch icons. |
| `git.parseEmoji` | `true` | Render `:emoji:` shortcodes in commit messages. |
| `disableStartupPopups` | `true` | Skip the "did you know" popups on launch. |
| `promptToReturnFromSubprocess` | `false` | Return to lazygit immediately after an editor/subprocess closes, no extra keypress. |
| `os.copyToClipboardCmd` / `readFromClipboardCmd` | `~/dotfiles/scripts/pbcopy.sh` / `pbpaste.sh` | Repo's clipboard wrappers (auto-pick `wl-copy`/`xclip`/`xsel`/`clip.exe`) — works under Wayland (niri) or X11, not just one session type. |

## Custom commands

| Key | Context | Action |
|---|---|---|
| `<ctrl+a>` | Files | `git commit --amend --no-edit` — amend the last commit with staged changes, keep its message. |

Add more in `config.yml` under `customCommands:` — press `e` on the Status
panel inside lazygit to jump straight to the config file. Docs:
`/usr/share/doc/lazygit/Custom_Command_Keybindings.md` (or
[upstream](https://github.com/jesseduffield/lazygit/blob/master/docs/Custom_Command_Keybindings.md)).

## Keybindings (stock defaults)

Regenerate this table after a lazygit upgrade from the file lazygit ships
itself: `/usr/share/doc/lazygit/keybindings/Keybindings_en.md`, or from
inside lazygit press `?` / `<disabled-key>` → "Open keybindings menu".

Notation: `<c-x>` = Ctrl+x, `<a-x>` = Alt+x, `<s-x>` = Shift+x.

### Global (works everywhere)

| Key | Action |
|---|---|
| `q`, `<ctrl+c>` | Quit |
| `<ctrl+z>` | Suspend the application |
| `<esc>` | Cancel |
| `?` | Open keybindings menu |
| `<ctrl+r>` | Switch to a recent repo |
| `<pgup>`, `K`, `<ctrl+u>` | Scroll up main window |
| `<pgdown>`, `J`, `<ctrl+d>` | Scroll down main window |
| `@` | View command log options |
| `P` | Push |
| `p` | Pull |
| `R` | Refresh |
| `m` | View merge/rebase options |
| `:` | Execute shell command |
| `<ctrl+p>` | View custom patch options |
| `<ctrl+s>` | View filter options |
| `W`, `<ctrl+e>` | View diffing options |
| `<ctrl+w>` | Toggle whitespace in diff |
| `}` / `{` | Increase / decrease diff context size |
| `)` / `(` | Increase / decrease rename similarity threshold |
| `\|` / `\` | Cycle diff renderers forward / reverse |
| `+` / `_` | Next / prev screen mode (normal/half/full) |
| `z` / `Z` | Undo / redo (reflog-based, commits only) |
| `<alt+shift+c>` | Edit config file |

### List panel navigation (any list)

| Key | Action |
|---|---|
| `<up>`/`k`, `<down>`/`j` | Prev / next item |
| `<left>`/`h`, `<right>`/`l`, `<tab>`/`<backtab>` | Prev / next block (panel) |
| `,` / `.` | Prev / next page |
| `<`/`<home>`, `>`/`<end>` | Scroll to top / bottom |
| `H` / `L` | Scroll left / right |
| `[` / `]` | Prev / next tab |
| `v` | Toggle range select |
| `<shift+up>` / `<shift+down>` | Range select up / down |
| `1`-`5` | Jump to block N |
| `/` | Search / filter current view |
| `n` / `N` | Next / prev search match |
| `<space>` | Select |
| `<enter>` | Go into / confirm |

### Status panel

| Key | Action |
|---|---|
| `e` | Edit config file |
| `u` | Check for update |
| `<enter>` | Switch to a recent repo |
| `a` / `A` | Show/cycle all-branches log (/ reverse) |
| `0` | Focus main view |

### Files

| Key | Action |
|---|---|
| `<space>` | Toggle staged |
| `a` | Toggle staged/unstaged for all files |
| `<enter>` | Stage individual hunks/lines, or collapse directory |
| `c` | Commit staged changes |
| `w` | Commit without pre-commit hook |
| `C` | Commit using git editor |
| `A` | Amend last commit |
| `<ctrl+a>` | **Custom:** amend last commit, keep message (`--no-edit`) |
| `<ctrl+f>` | Find base commit for fixup |
| `d` | View discard options |
| `D` | View reset options (working tree) |
| `g` | View upstream reset options |
| `s` | Stash all changes |
| `S` | View stash options |
| `i` | Ignore/exclude file |
| `r` | Refresh files |
| `f` | Fetch |
| `M` | View merge conflict options |
| `` ` `` | Toggle flat/tree file view |
| `-` / `=` | Collapse / expand all directories |
| `o` | Open file | 
| `e` | Edit file in external editor |
| `<ctrl+t>` | Open external diff tool |
| `<ctrl+o>` | Copy path to clipboard |
| `y` | Copy to clipboard (menu) |
| `<ctrl+b>` | Filter files by status |

### Local branches

| Key | Action |
|---|---|
| `<space>` | Checkout selected |
| `c` | Checkout by name (`-` for previous branch) |
| `-` | Checkout previous branch |
| `n` | New branch |
| `N` | Move unpushed commits to a new branch |
| `w` | New worktree |
| `F` | Force checkout (discards local changes) |
| `r` | Rebase current branch onto selected |
| `M` | Merge selected into current |
| `R` | Rename branch |
| `d` | View delete options |
| `g` | View reset options |
| `f` | Fast-forward from upstream |
| `u` | View upstream options |
| `T` | New tag |
| `s` | Sort order |
| `i` | Show git-flow options |
| `o` | Create pull request |
| `O` | View create-pull-request options |
| `G` | Open pull request in browser |
| `<ctrl+y>` | Copy PR URL to clipboard |
| `<ctrl+o>` | Copy branch name to clipboard |
| `<ctrl+t>` | Open external diff tool |
| `<enter>` | View commits |

### Commits

| Key | Action |
|---|---|
| `<space>` | Checkout as detached HEAD |
| `<enter>` | View files |
| `e` | Edit (start interactive rebase from here) |
| `i` | Start interactive rebase (from branch base) |
| `p` | Pick (mid-rebase) |
| `d` | Drop |
| `r` / `R` | Reword / reword with editor |
| `s` | Squash into commit below |
| `f` | Fixup into commit below |
| `F` | Create `fixup!` commit |
| `S` | Apply/autosquash fixup commits |
| `c` | Set fixup message |
| `A` | Amend with staged changes |
| `a` | Amend commit attribute (author/co-author) |
| `t` | Revert |
| `T` | New tag at commit |
| `B` | Mark as base commit for rebase |
| `g` | View reset options |
| `<ctrl+j>`/`<alt+down>`, `<ctrl+k>`/`<alt+up>` | Move commit down / up |
| `C` | Copy (cherry-pick source) |
| `V` | Paste (cherry-pick) |
| `<ctrl+r>` | Reset cherry-pick selection |
| `n` | New branch off commit |
| `N` | Move unpushed commits to new branch |
| `w` | New worktree |
| `b` | View bisect options |
| `*` | Select commits of current branch |
| `<ctrl+l>` | View log options (sort order, graph) |
| `o` / `G` | Open commit / open PR in browser |
| `y` | Copy commit attribute to clipboard |
| `<ctrl+o>` | Copy abbreviated hash to clipboard |
| `<ctrl+t>` | Open external diff tool |

### Amend commit attribute

| Key | Action |
|---|---|
| `a` / `A` | Reset / set author |
| `c` | Add co-author |

### Commit files

| Key | Action |
|---|---|
| `<enter>` | Enter file (patch mode) / toggle directory |
| `<space>` | Toggle file in custom patch |
| `a` | Toggle all files in custom patch |
| `c` | Checkout file from this commit |
| `d` | Discard this commit's changes to file |
| `` ` `` | Toggle flat/tree view |
| `-` / `=` | Collapse / expand all |
| `o` / `e` | Open / edit file |
| `<ctrl+t>` | Open external diff tool |
| `<ctrl+o>` / `y` | Copy path / copy to clipboard |

### Stash

| Key | Action |
|---|---|
| `<space>` | Apply |
| `g` | Pop |
| `d` | Drop |
| `n` | New branch from stash |
| `r` | Rename stash |
| `w` | New worktree |
| `<enter>` | View files |

### Remotes / Remote branches

| Key | Action |
|---|---|
| `<enter>` | View branches / view commits |
| `n` | New remote / new branch |
| `d` | Remove remote / delete remote branch |
| `e` | Edit remote |
| `f` | Fetch |
| `F` | Add fork remote (remotes panel) |
| `<space>` | Checkout (remote branches panel) |
| `r` | Rebase onto selected (remote branches) |
| `M` | Merge selected into current (remote branches) |
| `u` | Set as upstream (remote branches) |
| `s` | Sort order (remote branches) |
| `w` | New worktree (remote branches) |

### Tags

| Key | Action |
|---|---|
| `<space>` | Checkout as detached HEAD |
| `n` | New tag |
| `d` | View delete options |
| `P` | Push tag |
| `g` | View reset options |
| `w` | New worktree |
| `<enter>` | View commits |

### Worktrees

| Key | Action |
|---|---|
| `<space>` | Switch to worktree |
| `n` | New worktree |
| `o` | Open in editor |
| `d` | Remove worktree |

### Submodules

| Key | Action |
|---|---|
| `<enter>` | Enter submodule (`<esc>` to leave) |
| `i` | Initialize |
| `u` | Update |
| `n` | New submodule |
| `e` | Update submodule URL |
| `d` | Remove |
| `b` | View bulk submodule options |
| `<ctrl+o>` | Copy name to clipboard |

### Reflog / Sub-commits

Same as **Commits** panel, minus rebase/edit actions (these are read-only
history views): checkout, cherry-pick, reset, new branch, new worktree, open
in browser, copy attributes, select commits of current branch.

### Main panel — normal / staging / patch building / merge conflicts

| Key | Action | Where |
|---|---|---|
| `<tab>` | Switch staged/unstaged view | normal, staging |
| `<esc>` | Exit back to side panel | normal, staging, patch, merge |
| `<left>`/`h`, `<right>`/`l` | Prev / next hunk (or conflict, in merge view) | staging, patch, merge |
| `<up>`/`k`, `<down>`/`j` | Prev / next hunk | merge conflicts |
| `v` | Toggle range select | staging, patch |
| `a` | Toggle hunk vs line-by-line selection | staging, patch |
| `<space>` | Stage / toggle lines in patch | staging, patch |
| `<space>` | Pick hunk | merge conflicts |
| `b` | Pick both hunks | merge conflicts |
| `d` | Discard / unstage / remove lines from commit | staging, patch |
| `E` | Edit hunk in external editor | staging |
| `c` / `w` / `C` | Commit / commit no-hook / commit via editor | staging |
| `<ctrl+f>` | Find base commit for fixup | staging |
| `z` | Undo last conflict resolution | merge conflicts |
| `M` | View merge conflict options | merge conflicts |
| `o` / `e` | Open / edit file | staging, patch, merge |
| `<ctrl+o>` | Copy selected text to clipboard | staging, patch |
| `/` | Search current view | most |

### Menu / prompt / confirmation panels

| Key | Action |
|---|---|
| `<enter>` | Confirm / execute |
| `<esc>` | Close / cancel |
| `/` | Filter menu items |
| `<ctrl+o>` | Copy to clipboard (confirmation panel) |
