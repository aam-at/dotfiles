# Agent configuration

This directory contains the small, portable portion of the configuration for
the coding agents used on this machine. `install.conf.yaml` links the supported
files into each agent's normal home directory.

| Agent | Tracked settings | Deployment |
| --- | --- | --- |
| Codex | `codex/config.toml` | `~/.codex/config.toml` |
| Claude Code | `claude/settings.json` | `~/.claude/settings.json` |
| Pi | `pi/settings.json`, `pi/keybindings.json` | `~/.pi/agent/` |
| OMP | `omp/config.yml` | `~/.omp/agent/config.yml` |

Authentication, chat history, databases, caches, project trust, local hooks,
and absolute machine paths remain local. Sign in separately after installing on
a new machine. Add further settings as explicit file links in
`install.conf.yaml`; never link an entire agent home directory, because it
usually contains auth and runtime state.

Run the repository's normal Dotbot installer to apply these links. Existing
local settings should be reviewed or backed up before the first run, since a
file-level link replaces that particular settings file.
