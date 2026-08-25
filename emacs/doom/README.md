# Doom Emacs Port

This directory is the Doom-specific entrypoint for the shared Emacs setup.

It keeps Doom-native module selection in `init.el`, extra packages and local
recipes in `packages.el`, and host wiring in `config.el`. Shared configuration
lives in `../config`, while Spacemacs-only layers remain in `../spacemacs`.

## Usage

```sh
~/.config/emacs/bin/doom --doomdir ~/dotfiles/emacs/doom sync
~/.config/emacs/bin/doom --doomdir ~/dotfiles/emacs/doom doctor
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
