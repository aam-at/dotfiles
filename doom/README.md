# Doom Emacs Port

This directory is a Doom Emacs port of `spacemacs_full`.

It keeps Doom-native module selection in `init.el`, extra packages and local
recipes in `packages.el`, and compatibility/config glue in `config.el`.
Existing Spacemacs config files are reused from the parent directory, so changes
to `config/config-org.el`, `config/config-ai.el`, and related files still apply.

## Usage

```sh
~/.config/emacs/bin/doom --doomdir ~/dotfiles/doom sync
~/.config/emacs/bin/doom --doomdir ~/dotfiles/doom doctor
```

To make this the default Doom config, either set `DOOMDIR` to this directory or
link/copy it to the private config location used by your Doom setup.

## Notes

`doom doctor` currently initializes this config, but reports environment
warnings for external tools/fonts that are not installed or not on `PATH`
(`Symbola`, `ledger`, `gnuplot`, `plantuml.jar`, `pyflakes`, `pipenv`,
`nosetests`, `shfmt`, and a mail fetcher such as `mbsync`/`offlineimap`).

It also reports that `~/.config/doom` exists. If you want this directory to be
the active private config without passing `--doomdir`, remove or rename that
older private config, or use Doom profiles/Chemacs.
