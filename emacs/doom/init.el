;;; init.el -*- lexical-binding: t; -*-

;; Doom module selection for the personal Emacs profile.
;; Run: ~/.config/emacs/bin/doom sync --doomdir ~/dotfiles/emacs/doom

(add-to-list 'load-path (expand-file-name "../config" doom-user-dir))
(require 'config-lsp)

(doom! :completion
       (corfu +icons +orderless +dabbrev)
       (vertico +icons)

       :ui
       doom
       dashboard
       deft
       hl-todo
       ligatures
       modeline
       (popup +defaults)
       tabs
       (:if (aam/lsp-client-p) (treemacs +lsp) treemacs)
       unicode
       (vc-gutter +pretty)
       vi-tilde-fringe
       window-select
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       multiple-cursors
       parinfer
       snippets
       (whitespace +guess +trim)
       word-wrap

       :emacs
       (dired +dirvish)
       electric
       ibuffer
       tramp
       undo
       vc

       :term
       eshell
       shell
       term
       vterm

       :checkers
       grammar
       (spell +flyspell)
       syntax

       :tools
       biblio
       debugger
       direnv
       (eval +overlay)
       lookup
       (:if (aam/eglot-client-p) (lsp +eglot))
       (:if (aam/lsp-client-p) lsp)
       llm
       magit
       pass
       pdf
       tmux
       tree-sitter

       :os
       (:if (featurep :system 'macos) macos)
       (:if (featurep :system 'linux) tty)

       :lang
       (cc +lsp +tree-sitter)
       data
       emacs-lisp
       ess
       json
       (latex +latexmk +cdlatex)
       ledger
       markdown
       (org +crypt +gnuplot +journal +noter +pandoc +present +pretty +roam)
       plantuml
       (python +lsp +tree-sitter +uv)
       rest
       (rust +lsp +tree-sitter)
       sh
       (yaml +lsp)

       :email
       (:if (featurep :system 'linux) (mu4e +org))

       :app
       calendar
       (rss +org)

       :config
       (default +bindings +smartparens))
