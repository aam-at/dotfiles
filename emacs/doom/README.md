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

## Custom keybindings

The Doom profile uses Doom's native leader layout directly. There is no
Spacemacs compatibility prefix and no separate comma localleader; mode-local
commands therefore use Doom's standard `SPC m` prefix.

### Global leader keys

| Key | Command / group |
| --- | --- |
| `SPC p l` | Find project Git file with Helm (`helm-ls-git`) |
| `SPC p h` | Harpoon: `.` quick menu, `m` menu, `a` add, `d` delete, `1`-`9` jump to slot |
| `SPC h f/v/k` | Helpful callable / variable / key help |
| `SPC b j` | Buffer marks: `t` toggle, `n` next, `p` previous, `l` list |
| `SPC o w` | Window manager (`ewmctrl`) |
| `SPC t W` | Writing modes: `d` Write or Die, `g` writegood, `r` writeroom |
| `SPC s w` | Word tools; see below |
| `SPC d a` | DAP fallback: `d` debug, `h` hydra |
| `SPC n B` | Ebib bibliography manager |
| `SPC n D` | Org Doing |
| `SPC n M` | Recent Org clocks: `i` clock in, `g` go to clock, `s` select recent task |
| `SPC n g` | Org Google Calendar: `s` sync, `f` fetch, `p` post, `r` refresh token (when enabled) |
| `SPC n r T` | Toggle Org-roam properties |
| `SPC n r v/V/b` | Vulpea find / insert / backlinks |
| `SPC c F` | Clang-format the active region or buffer |
| `SPC o l c/C` | Open / display Copilot Chat |
| `SPC o l d` | Start speech dictation |
| `SPC o l E` | Open the Ellama command menu |
| `SPC o l k` | Open Khoj |
| `SPC o l w/W` | Whisper fast / accurate transcription |
| `SPC c A` | Copilot code actions; see below |
| `SPC b A` | Copilot context buffers: `a` add, `d` remove, `l` list |
| `SPC g c m` | Generate a commit message with Copilot Chat |

`SPC s w` contains the writing lookup tools: `s` synonyms, `a` antonyms, `r`
related words, `d` definitions, `e` example sentences, `l/L` Libre Thesaurus
synonyms/antonyms, `m` Merriam-Webster, `y` Synosaurus, `w` the Words menu,
`p/P` academic phrases/by section, and `g` LanguageTool.

`SPC c A` contains Copilot Chat actions: `e/E/s` explain selection/defun/symbol,
`d` document, `f` fix, `o` optimize, `t` test, `r/R` review selection/buffer,
`p/P` custom prompt/prompt function, and `i` ask and insert.

The configuration deliberately relies on Doom's existing bindings where Doom
already provides the same workflow. In particular, `SPC n e` opens Org Noter,
`SPC n r f/i` finds/inserts Org-roam nodes, and `SPC o l ...` retains Doom's
native gptel commands alongside the additions above.

### Mode-local keys (`SPC m`)

| Mode | Key | Command / group |
| --- | --- | --- |
| Org Agenda | `A` | Casual agenda menu |
| BibTeX | `b` / `s` | Biblio / Google Scholar lookup |
| Org | `R` | Research tools: `o` remove overlays, `y/Y` sort/filter year, `c/C` sort/filter citations, `u` update citations |
| Org | `l f` | Convert an Org ID link to a file link |
| Org | `l r/R` | Insert org-ref reference / label link |
| Org | `l x` | Org Transclusion transient menu |
| Org | `N` | Org-roam-bibtex note actions |
| Org | `D` | Delve: `d` open, `c` collect, `e` edit, `i` inspect |
| Org | `G` | Org Google Calendar: `s/f/p/r` (when enabled) |
| PDF View | `e` / `N` | Extract PDF text / Org Noter |
| LaTeX | `C` / `x` | Toggle continuous latexmk / Texpresso preview |
| Copilot Chat | `M` / `R` | Set model / reset chat |
| Copilot Chat | `l`, `n/p`, `r`, `d`, `f`, `o`, `t`, `q` | Prompt list, history, review, document, fix, optimize, test, quit |

Delve buffers also keep their navigation and action commands on `SPC m`, while
Copilot completion continues to use `TAB` to accept and `C-TAB` to accept by
word. Popper keeps its direct C-`, M-`, and C-M-` bindings.

## Notes

`doom doctor` currently initializes this config, but reports environment
warnings for external tools/fonts that are not installed or not on `PATH`
(`Symbola`, `ledger`, `gnuplot`, `plantuml.jar`, `pyflakes`, `pipenv`,
`nosetests`, `shfmt`, and a mail fetcher such as `mbsync`/`offlineimap`).

It also reports that `~/.config/doom` exists. If you want this directory to be
the active private config without passing `--doomdir`, remove or rename that
older private config, or use Doom profiles/Chemacs.
