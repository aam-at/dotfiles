;;; init.el -*- lexical-binding: t; -*-

;; Doom module selection translated from spacemacs_full.
;; Run: ~/.config/emacs/bin/doom sync --doomdir ~/dotfiles/doom

(doom! :completion
       (company +childframe +icons)
       (vertico +childframe +icons)

       :ui
       doom
       dashboard
       deft
       hl-todo
       ligatures
       modeline
       (popup +defaults)
       tabs
       treemacs
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
       dired
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
       lsp
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
       (latex +latexmk)
       ledger
       markdown
       (org +crypt +gnuplot +journal +noter +pandoc +present +pretty +roam)
       plantuml
       (python +lsp +tree-sitter)
       rest
       (rust +lsp +tree-sitter)
       sh
       yaml

       :email
       (:if (featurep :system 'linux) (mu4e +org))

       :app
       calendar
       (rss +org)

       :config
       (default +bindings +smartparens))
