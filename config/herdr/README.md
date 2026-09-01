# HerdR

HerdR is the dedicated Kitty-hosted workspace for coding agents. It complements
the tmux workflow; it does not replace it.

After running the normal dotfiles install, start it with `F7` in Kitty or with
the Bash command `herd`. Both open a Kitty OS window and attach to HerdR's
default persistent session.

The configuration uses Fish for new panes, Gruvbox styling, a tmux-familiar
`Ctrl-A` prefix, in-app agent notifications, native agent-session restoration,
and worktrees under `~/.herdr/worktrees`. `Ctrl-A Alt-G` opens Lazygit in a
temporary HerdR popup.

## Agent integrations

Running `./install` runs the official integration installer for Claude, Pi,
OMP, Codex, OpenCode, and GitHub Copilot CLI. It invokes the official installer
for every agent inline from `install.conf.yaml`, with no preflight checks. The
integration installer modifies the relevant agent configuration directories.
Run `herdr integration status` afterward to inspect installed integration
versions.
