;;; config.el -*- lexical-binding: t; -*-

;; Doom port of the Spacemacs configuration in this repository.

(setq user-full-name "Alexander Matyasko"
      user-mail-address "alexander.matyasko@gmail.com"
      display-line-numbers-type 'relative
      doom-localleader-key ","
      shell-file-name (or (executable-find "bash") shell-file-name))

(setq-default vterm-shell "/bin/fish"
              explicit-shell-file-name "/bin/fish")

(defconst aam/spacemacs-root
  (file-truename (expand-file-name "../spacemacs" doom-user-dir)))

(defconst aam/spacemacs-config-dir
  (file-truename (expand-file-name "../config" doom-user-dir)))

(add-to-list 'load-path aam/spacemacs-config-dir)
(add-to-list 'load-path (file-truename (expand-file-name "../funcs" doom-user-dir)))
(add-to-list 'load-path (expand-file-name "aam" aam/spacemacs-root))
(add-to-list 'load-path (expand-file-name "org-extras" aam/spacemacs-root))
(add-to-list 'load-path (expand-file-name "org-roam-extras" aam/spacemacs-root))
(add-to-list 'load-path (expand-file-name "latex-extras" aam/spacemacs-root))
(add-to-list 'load-path (expand-file-name "cpp-extras" aam/spacemacs-root))

(require 'config-ui)
(setq doom-theme aam/theme
      doom-font (font-spec :family aam/monospace-font-family
                           :size (aam/font-size))
      doom-variable-pitch-font (font-spec :family aam/proportional-font-family :size 14)
      doom-serif-font (font-spec :family aam/proportional-font-family :size 14))

(when (native-comp-available-p)
  (setq package-native-compile t
        native-comp-deferred-compilation t))

(setq max-lisp-eval-depth 10000
      inhibit-x-resources t
      find-file-visit-truename t
      lsp-use-plists t)
(setenv "LSP_USE_PLISTS" "true")

(setq org-enable-delve t
      org-enable-gcal nil
      org-enable-roam-ui nil
      aam-enable-explain-pause-at-startup nil
      ai-extras-autostart-gptcommit-mode t)

(load! "../config/autoloads")
(load! "keybindings")

(load! "../funcs/aam-core")
(aam/require-supported-emacs)
(aam/configure-org-paths)
(load! "../funcs/aam-org")
(load! "../funcs/aam-org-roam")
(load! "../funcs/aam-latex")
(load! "../funcs/aam-cpp")

(use-package! keychain-environment
	      :config
	      (keychain-refresh-environment))

(use-package! explain-pause-mode
	      :commands explain-pause-mode
	      :init
	      (when aam-enable-explain-pause-at-startup
		(explain-pause-mode))
	      :config
	      (setf (cadr (assq 'explain-pause-mode minor-mode-alist)) ""))

(use-package! gcmh
	      :config
	      (gcmh-mode 1))

(use-package! fish-completion
	      :if (executable-find "fish")
	      :config
	      (global-fish-completion-mode))

(use-package! popper
	      :bind (("C-`" . popper-toggle)
		     ("M-`" . popper-cycle)
		     ("C-M-`" . popper-toggle-type))
	      :init
	      (setq popper-group-function #'popper-group-by-directory
		    popper-reference-buffers
		    '("\\*Messages\\*"
		      "Output\\*$"
		      "^\\*Python\\*$" inferior-python-mode
		      "\\*Async Shell Command\\*"
		      "^\\*eshell.*\\*$" eshell-mode
		      "^\\*shell.*\\*$" shell-mode
		      "^\\*term.*\\*$" term-mode
		      "^\\*vterm.*\\*$" vterm-mode
		      flycheck-error-list-mode
		      help-mode
		      compilation-mode))
	      :config
	      (popper-mode 1)
	      (popper-echo-mode 1))

(use-package! prescient
	      :config
	      (push 'prescient completion-styles))

(use-package! key-chord
	      :config
	      (key-chord-mode 1))

(use-package! key-seq
	      :after evil
	      :config
	      (key-seq-define evil-normal-state-map "wh" #'evil-window-left)
	      (key-seq-define evil-normal-state-map "wj" #'evil-window-down)
	      (key-seq-define evil-normal-state-map "wk" #'evil-window-up)
	      (key-seq-define evil-normal-state-map "wl" #'evil-window-right)
	      (key-seq-define evil-normal-state-map "wy" #'split-window-right)
	      (key-seq-define evil-normal-state-map "wu" #'aam/doom-split-window-below-and-focus)
	      (key-seq-define evil-normal-state-map "wi" #'split-window-below)
	      (key-seq-define evil-normal-state-map "wo" #'aam/doom-split-window-right-and-focus)
	      (key-seq-define evil-normal-state-map "wm" #'doom/window-maximize-buffer)
	      (key-seq-define evil-normal-state-map "kf" #'delete-frame)
	      (key-seq-define evil-normal-state-map "kw" #'evil-quit)
	      (key-seq-define evil-normal-state-map "kb" #'kill-this-buffer))

;; Keep the legacy Helm entry points from the Spacemacs profile available while
;; Vertico remains Doom's primary completion interface.
(use-package! helm-icons
	      :after helm
	      :config
	      (helm-icons-enable))

(use-package! helm-ls-git
	      :commands helm-ls-git
	      :init
	      (aam/ported-leader! "gff" #'helm-ls-git)
	      :config
	      ;; Magit owns rebase todo buffers; helm-ls-git must not steal them.
	      (setq auto-mode-alist
		    (delete '("/git-rebase-todo$" . helm-ls-git-rebase-todo-mode)
			    auto-mode-alist)
		    helm-ls-git-status-command #'magit-status-setup-buffer))

(use-package! helm-posframe
	      :after helm
	      :config
	      (setq helm-posframe-poshandler #'posframe-poshandler-frame-center
		    helm-posframe-parameters '((internal-border-width . 2)
					       (left-fringe . 4)
					       (right-fringe . 4)
					       (undecorated . nil)))
	      (defun aam/update-helm-posframe-dimensions (&rest _)
		(setq helm-posframe-width (round (* 0.618 (frame-width)))
		      helm-posframe-height (round (* 0.618 (frame-height)))))
	      (aam/update-helm-posframe-dimensions)
	      (add-hook 'window-size-change-functions #'aam/update-helm-posframe-dimensions)
	      (helm-posframe-enable))

(use-package! harpoon
	      :commands (harpoon-quick-menu-hydra harpoon-toggle-quick-menu harpoon-add-file
						  harpoon-delete-item harpoon-go-to-1 harpoon-go-to-2 harpoon-go-to-3
						  harpoon-go-to-4 harpoon-go-to-5 harpoon-go-to-6 harpoon-go-to-7
						  harpoon-go-to-8 harpoon-go-to-9)
	      :init
	      (aam/ported-leader!
               (:prefix ("ah" . "harpoon")
			"." #'harpoon-quick-menu-hydra
			"m" #'harpoon-toggle-quick-menu
			"a" #'harpoon-add-file
			"d" #'harpoon-delete-item)
               "M-1" #'harpoon-go-to-1
               "M-2" #'harpoon-go-to-2
               "M-3" #'harpoon-go-to-3
               "M-4" #'harpoon-go-to-4
               "M-5" #'harpoon-go-to-5
               "M-6" #'harpoon-go-to-6
               "M-7" #'harpoon-go-to-7
               "M-8" #'harpoon-go-to-8
               "M-9" #'harpoon-go-to-9))

(use-package! helpful
	      :commands (helpful-callable helpful-variable helpful-key)
	      :init
	      (aam/ported-leader!
               "hf" #'helpful-callable
               "hv" #'helpful-variable
               "hk" #'helpful-key))

(use-package! bm
	      :commands (bm-toggle bm-next bm-previous bm-show-all)
	      :init
	      (aam/ported-leader!
               "bm" #'bm-toggle
               "bn" #'bm-next
               "bp" #'bm-previous
               "bl" #'bm-show-all))

(use-package! magit-delta
	      :after magit
	      :config
	      (magit-delta-mode 1))

(use-package! magit-gitflow
	      :after magit)

(use-package! casual
	      :after org-agenda
	      :init
	      (map! :map org-agenda-mode-map :localleader "A" #'casual-agenda-tmenu))

(use-package! biblio
	      :commands biblio-lookup
	      :init
	      (map! :map bibtex-mode-map :localleader "lb" #'biblio-lookup)
	      :config
	      (evil-set-initial-state 'biblio-selection-mode 'emacs))

(use-package! gscholar-bibtex
	      :commands gscholar-bibtex
	      :init
	      (map! :map bibtex-mode-map :localleader "ls" #'gscholar-bibtex)
	      :config
	      (evil-set-initial-state 'gscholar-bibtex-mode 'emacs))

(use-package! ewmctrl
	      :commands ewmctrl
	      :init
	      (aam/ported-leader! "Aw" #'ewmctrl)
	      :config
	      (map! :map ewmctrl-mode-map :n
		    "n" #'next-line "p" #'previous-line "g" #'ewmctrl-refresh
		    ";" #'ewmctrl-toggle-single-key-to-focus
		    "RET" #'ewmctrl-focus-window "D" #'ewmctrl-delete-window
		    "I" #'ewmctrl-change-window-icon-name "m" #'ewmctrl-move-window-to-other-desktop
		    "M" #'ewmctrl-move-window-to-current-desktop-and-focus
		    "N" #'ewmctrl-change-window-name "r" #'ewmctrl-resize-window
		    "fc" #'ewmctrl-filters-clear "fd" #'ewmctrl-filter-by-desktop-number
		    "fD" #'ewmctrl-filter-desktop-number-clear "fn" #'ewmctrl-filter-by-name
		    "fN" #'ewmctrl-filter-name-clear "fp" #'ewmctrl-filter-by-pid
		    "fP" #'ewmctrl-filter-pid-clear "Sd" #'ewmctrl-sort-by-desktop-number
		    "SD" #'ewmctrl-sort-by-desktop-number-reversed "Sn" #'ewmctrl-sort-by-name
		    "SN" #'ewmctrl-sort-by-name-reversed "Sp" #'ewmctrl-sort-by-pid
		    "SP" #'ewmctrl-sort-by-pid-reversed))

(use-package! ultra-scroll
	      :init
	      (setq scroll-conservatively 101
		    scroll-margin 0)
	      :config
	      (ultra-scroll-mode 1))

(use-package! write-or-die
	      :commands write-or-die-mode
	      :hook (text-mode . write-or-die-mode)
	      :init
	      (aam/ported-leader! "xD" #'write-or-die-mode))

(use-package! mw-thesaurus
	      :hook (variable-pitch-mode . mw-thesaurus-mode))

(use-package! jinx
	      :hook (doom-first-input . global-jinx-mode)
	      :init
	      (map! [remap ispell-word] #'jinx-correct
		    [remap evil-next-flyspell-error] #'jinx-next
		    [remap evil-prev-flyspell-error] #'jinx-previous))

(use-package! synosaurus
	      :hook ((text-mode markdown-mode) . synosaurus-mode)
	      :init
	      (aam/ported-leader! "Stw" #'synosaurus-lookup)
	      :config
	      (setq synosaurus-choose-method 'default))

(use-package! words
	      :commands (words words-hydra/body)
	      :init
	      (aam/ported-leader! "Sw" #'words-hydra/body))

(after! flycheck
	(flycheck-define-checker proselint
				 "A linter for prose."
				 :command ("proselint" source-inplace)
				 :error-patterns
				 ((warning line-start (file-name) ":" line ":" column ": "
					   (id (one-or-more (not (any " "))))
					   (message (one-or-more not-newline)
						    (zero-or-more "\n" (any " ") (one-or-more not-newline)))
					   line-end))
				 :modes (text-mode latex-mode LaTeX-mode org-mode markdown-mode gfm-mode))
	(add-to-list 'flycheck-checkers 'proselint)
	(flycheck-define-checker textlint
				 "A linter for textlint."
				 :command ("npx" "textlint"
					   "--config" "/home/amatyasko/.textlintrc"
					   "--format" "unix"
					   "--rule" "write-good"
					   "--rule" "no-start-duplicated-conjunction"
					   "--rule" "max-comma"
					   "--rule" "terminology"
					   "--rule" "period-in-list-item"
					   "--rule" "abbr-within-parentheses"
					   "--rule" "alex"
					   "--rule" "common-misspellings"
					   "--rule" "en-max-word-count"
					   "--rule" "diacritics"
					   "--rule" "stop-words"
					   "--plugin"
					   (eval (if (derived-mode-p 'tex-mode) "latex" "@textlint/text"))
					   source-inplace)
				 :error-patterns
				 ((warning line-start (file-name) ":" line ":" column ": "
					   (message (one-or-more not-newline)
						    (zero-or-more "\n" (any " ") (one-or-more not-newline)))
					   line-end))
				 :modes (text-mode latex-mode LaTeX-mode org-mode markdown-mode gfm-mode))
	(add-to-list 'flycheck-checkers 'textlint))

(use-package! flycheck-vale
	      :after flycheck
	      :config
	      (setq flycheck-vale-modes '(text-mode markdown-mode rst-mode org-mode latex-mode LaTeX-mode))
	      (flycheck-vale-setup)
	      (dolist (mode flycheck-vale-modes)
		(flycheck-add-mode 'vale mode)))

(after! writeroom-mode
	(setq writeroom-width 90)
	(aam/ported-leader! "xW" #'writeroom-mode))

(after! writegood-mode
	(aam/ported-leader! "xG" #'writegood-mode))

(aam/ported-leader!
 "Sts" #'powerthesaurus-lookup-synonyms-dwim
 "Sta" #'powerthesaurus-lookup-antonyms-dwim
 "Str" #'powerthesaurus-lookup-related-dwim
 "Std" #'powerthesaurus-lookup-definitions-dwim
 "Ste" #'powerthesaurus-lookup-sentences-dwim
 "Stl" #'le-thesaurus-get-synonyms
 "StL" #'le-thesaurus-get-antonyms
 "Stm" #'mw-thesaurus-lookup-dwim)

(autoload 'aam/common-setup "config-common" nil t)
(autoload 'aam/lsp-setup "config-lsp" nil t)
(autoload 'aam/secure-setup "config-secure" nil t)
(autoload 'aam/ai-setup "config-ai" nil t)
(autoload 'aam/org-setup "config-org" nil t)
(autoload 'aam/python-setup "config-python" nil t)
(autoload 'aam/tex-setup "config-tex" nil t)
(autoload 'aam/bibtex-setup "config-bibtex" nil t)
(autoload 'aam/mail-setup "config-mail" nil t)

(aam/common-setup)

(condition-case err
    (aam/secure-setup)
  (error
   (message "Secure config was not loaded: %s" (error-message-string err))))

(after! lsp-mode
	(when (aam/lsp-client-p)
	  (aam/lsp-setup)))

;; Spacemacs used DAP for C/C++ debugging.  Doom's debugger module provides
;; Dape, but retaining dap-mode keeps the configured LLDB and cpptools flows.
(use-package! dap-mode
	      :after lsp-mode
	      :commands (dap-debug dap-hydra)
	      :config
	      (require 'dap-cpptools nil t)
	      (require 'dap-lldb nil t))

(after! org
	(aam/org-setup)
	(require 'org-protocol)
	(add-to-list 'org-modules 'org-protocol)
	(add-to-list 'org-modules 'org-roam-protocol)
	(org-super-agenda-mode 1)
	(add-hook 'org-mode-hook #'turn-on-org-cdlatex)
	(add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append)
	(map! :map org-mode-map
              :localleader
              "Sr" #'aam/org-remove-all-overlays
              "Sy" #'aam/org-sort-entries-by-year
              "SY" #'aam/org-filter-entries-by-year
              "Sc" #'aam/org-sort-entries-by-citations
              "SC" #'aam/org-filter-entries-by-citations
              "Su" #'aam/org-citations-update-at-point
              "uf" #'aam/org-convert-org-id-link-to-file-link
	      "N" #'orb-note-actions))

(use-package! org-protocol-capture-html
	      :after org)

(use-package! org-contacts
	      :after org
	      :demand t)

(use-package! org-remark
	      :after org
	      :demand t
	      :config
	      (org-remark-global-tracking-mode 1))

(use-package! org-download
	      :after org
	      :hook (org-mode . org-download-enable))

(use-package! org-cliplink
	      :after org)

(use-package! org-mime
	      :after org)

(use-package! ox-epub
	      :after org
	      :demand t)

(use-package! org-re-reveal
	      :after org
	      :demand t)

(after! org
	(aam/ported-leader! "aon" #'org-noter)
	(map! :map org-mode-map :localleader "n" #'org-noter)
	(map! :map org-mode-map :localleader
              "Dd" #'delve
              "Dc" #'delve-minor-mode-collect-actions
              "De" #'delve-minor-mode-edit-actions
              "Di" #'delve-minor-mode-inspect-actions)
	(setq org-file-apps (delete '("\\.pdf\\'" . default) org-file-apps))
	(add-to-list 'org-file-apps
		     '("\\.pdf\\'" . (lambda (_file link) (aam/org-pdfview-open link)))))

(use-package! magit-org-todos
	      :after magit
	      :config
	      (magit-org-todos-autoinsert))

(use-package! ob-async
	      :after org
	      :config
	      (setq ob-async-no-async-languages-alist '("ipython")))

(use-package! org-gcal
	      :when org-enable-gcal
	      :after org
	      :init
	      (aam/ported-leader!
               "aogs" #'org-gcal-sync
               "aogf" #'org-gcal-fetch
               "aogp" #'org-gcal-post-at-point
               "aogr" #'org-gcal-refresh-token)
	      (map! :map org-mode-map :localleader
		    "gs" #'org-gcal-sync
		    "gf" #'org-gcal-fetch
		    "gp" #'org-gcal-post-at-point
		    "gr" #'org-gcal-refresh-token)
	      :config
	      (setq org-gcal-dir (expand-file-name "org-gcal" doom-cache-dir)))

(use-package! org-pdftools
	      :after org
	      :hook (org-load . org-pdftools-setup-link))

(use-package! org-noter-pdftools
	      :after org-noter
	      :config
	      (with-eval-after-load 'pdf-annot
		(add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package! org-ref
	      :after org
	      :demand t
	      :config
	      (dolist (feature '(openalex doi-utils org-ref-pdf org-ref-url-utils org-ref-bibtex
					  org-ref-arxiv org-ref-pubmed org-ref-isbn org-ref-wos org-ref-scopus
					  x2bib org-ref-scifinder org-ref-worldcat))
		(unless (require feature nil t)
		  (message "Optional org-ref feature unavailable: %s" feature))))

(use-package! org-modern
	      :after org
	      :hook (org-mode . org-modern-mode))

(use-package! org-appear
	      :after org
	      :hook (org-mode . org-appear-mode))

(use-package! valign
	      :after org
	      :hook (org-mode . valign-mode))

(use-package! org-sticky-header
	      :after org
	      :hook (org-mode . org-sticky-header-mode))

(after! org-roam
	(require 'org-roam-protocol)
	(org-roam-db-autosync-mode 1)
	(require 'org-roam-bibtex nil t)
	(org-roam-bibtex-mode 1)
	(when (and org-enable-roam-ui
	           (require 'org-roam-ui nil t))
	  (org-roam-ui-mode 1)))

(after! org-roam
	(aam/ported-leader! "aorT" #'aam/org-roam-toggle-properties)
	(map! :map org-mode-map :localleader "rT" #'aam/org-roam-toggle-properties))

(use-package! vulpea
	      :after org-roam
	      :config
	      (setq vulpea-db-sync-directories (list org-directory))
	      (vulpea-db-autosync-mode 1)
	      (aam/ported-leader!
	       "aorf" #'vulpea-find
	       "aorF" #'org-roam-node-find
	       "aori" #'vulpea-insert
	       "aorI" #'org-roam-node-insert
	       "aorb" #'vulpea-find-backlink)
	      (map! :map org-mode-map
		    :localleader
		    "rf" #'vulpea-find
		    "rF" #'org-roam-node-find
		    "ri" #'vulpea-insert
		    "rI" #'org-roam-node-insert
		    "rb" #'vulpea-find-backlink))

(use-package! org-mru-clock
	      :after org
	      :config
	      (setq org-mru-clock-how-many 100)
	      (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook)
	      (aam/ported-leader!
	       "aoCi" #'org-mru-clock-in
	       "aoCg" #'org-mru-clock-goto
	       "aoCs" #'org-mru-clock-select-recent-task)
	      (map! :map org-mode-map
		    :localleader
		    "Ci" #'org-mru-clock-in
		    "Cg" #'org-mru-clock-goto
		    "Cs" #'org-mru-clock-select-recent-task))

(use-package! delve
	      :after org-roam
	      :hook ((delve-mode . delve-compact-view-mode)
		     (delve-mode . hl-line-mode))
	      :config
	      (delve-global-minor-mode 1))

(after! delve
	(map! :map delve-mode-map :n
              "RET" #'delve--key--toggle-preview
              "+" #'delve--key--add-tags "-" #'delve--key--remove-tags
              "T" #'delve--key--insert-node-by-tags "b" #'delve--key--backlinks
              "c" #'delve--key--collect-into-buffer "f" #'delve--key--fromlinks
              "g" #'delve--key--refresh "h" #'delve--key--insert-heading
              "i" #'delve--key--insert-query-or-pile "n" #'delve--node-transient-key
              "o" #'delve--key--open-zettel "p" #'delve--key--collect-into-pile
              "q" #'bury-buffer "s" #'delve--key--sort "t" #'delve--key--insert-tagged
              "v" #'delve-compact-view-mode
              "C-<left>" #'delve--key--backlinks
              "C-<return>" #'delve--key--open-zettel
              "C-<right>" #'delve--key--fromlinks
              "<delete>" #'delve--key--multi-delete)
	(map! :map delve-mode-map :localleader
              "+" #'delve--key--add-tags "-" #'delve--key--remove-tags
              "T" #'delve--key--insert-node-by-tags "b" #'delve--key--backlinks
              "c" #'delve--key--collect-into-buffer "f" #'delve--key--fromlinks
              "g" #'delve--key--refresh "h" #'delve--key--insert-heading
              "i" #'delve--key--insert-query-or-pile "n" #'delve--node-transient-key
              "o" #'delve--key--open-zettel "p" #'delve--key--collect-into-pile
              "q" #'bury-buffer "s" #'delve--key--sort "t" #'delve--key--insert-tagged
              "v" #'delve-compact-view-mode))

(use-package! org-similarity
	      :after org
	      :config
	      (setq org-similarity-directory org-directory
		    org-similarity-file-extension-pattern "*.org"
		    org-similarity-language "english"
		    org-similarity-algorithm "tfidf"
		    org-similarity-number-of-documents 10
		    org-similarity-min-chars 0
		    org-similarity-show-scores t
		    org-similarity-threshold 0.05
		    org-similarity-use-id-links t
		    org-similarity-recursive-search t
		    org-similarity-custom-python-interpreter nil
		    org-similarity-remove-first t
		    org-similarity-heading "** Related notes"
		    org-similarity-prefix "- "
		    org-similarity-ignore-frontmatter nil)
	      (map! :map org-mode-map
		    :localleader
		    "Ss" #'org-similarity-sidebuffer
		    "Sq" #'org-similarity-query))

(use-package! org-fragtog
	      :after org
	      :hook (org-mode . org-fragtog-mode))

(after! pdf-tools
	(map! :map pdf-view-mode-map
              :localleader
              "e" #'aam-extract-pdf-text-from-current-buffer
              "N" #'org-noter))

(after! python
	(aam/python-setup)
	(set-formatter! 'ruff :modes '(python-mode python-ts-mode)))

(after! tex
	(require 'polymode nil t)
	(aam/tex-setup))

(use-package! adaptive-wrap
	      :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
	      :init
	      (setq-default adaptive-wrap-extra-indent 0))

(use-package! auctex-cont-latexmk
	      :after latex
	      :init
	      (map! :map LaTeX-mode-map :localleader "Tc" #'auctex-cont-latexmk-toggle))

(use-package! auctex-label-numbers
	      :hook ((plain-TeX-mode . auctex-label-numbers-mode)
		     (LaTeX-mode . auctex-label-numbers-mode)))

(use-package! preview-auto
	      :after latex
	      :hook ((plain-TeX-mode . preview-auto-conditionally-enable)
		     (LaTeX-mode . preview-auto-conditionally-enable))
	      :config
	      (setq preview-locating-previews-message nil
		    preview-protect-point t
		    preview-leave-open-previews-visible t))

(use-package! texpresso
	      :commands texpresso
	      :init
	      (map! :map LaTeX-mode-map :localleader "t" #'texpresso))

(after! cdlatex
	(setq cdlatex-use-dollar-to-ensure-math nil)
	(dolist (key '("$" "(" "{" "[" "|" "<" "^" "_"))
	  (define-key cdlatex-mode-map (kbd key) nil)))

(use-package! twauctex
	      :after latex
	      :init
	      (remove-hook 'LaTeX-mode-hook #'latex/auto-fill-mode)
	      :config
	      (twauctex-global-mode))

(use-package! doxymacs
	      :commands doxymacs-mode
	      :hook (c++-mode . doxymacs-mode))

(after! clang-format
	(map! :map c-mode-map :localleader "=" #'aam/cpp-format-region-or-buffer)
	(map! :map c++-mode-map :localleader "=" #'aam/cpp-format-region-or-buffer))

(use-package! langtool
	      :commands (langtool-check langtool-correct-buffer)
	      :init
	      (setq langtool-default-language "en-US"
		    langtool-http-server-host "localhost"
		    langtool-http-server-port 8088)
	      (aam/ported-leader! "Sl" #'langtool-check))

(use-package! engine-mode
	      :config
	      (engine-mode 1))

(use-package! xclip
	      :if (display-graphic-p)
	      :config
	      (xclip-mode 1))

(after! bibtex
	(aam/bibtex-setup))

(after! gptel
	(aam/ai-setup))

(use-package! copilot
	      :hook (prog-mode . copilot-mode)
	      :config
	      (map! :map copilot-completion-map
		    "TAB" #'copilot-accept-completion
		    "<tab>" #'copilot-accept-completion
		    "C-TAB" #'copilot-accept-completion-by-word
		    "C-<tab>" #'copilot-accept-completion-by-word))

(use-package! shell-maker
	      :demand t)

(defun aam/doom-bury-and-kill-buffer ()
  "Dismiss the current Copilot Chat buffer and its window."
  (interactive)
  (bury-buffer)
  (delete-window))

(use-package! copilot-chat
	      :commands (copilot-chat-display copilot-chat-switch-to-buffer copilot-chat-reset)
	      :init
	      (aam/ported-leader!
	       "$cc" #'copilot-chat-switch-to-buffer
	       "$cr" #'copilot-chat-reset
	       "$cd" #'copilot-chat-display
	       "$cM" #'copilot-chat-set-model
	       "$cee" #'copilot-chat-explain
	       "$ceE" #'copilot-chat-explain-defun
	       "$ces" #'copilot-chat-explain-symbol-at-line
	       "$cid" #'copilot-chat-doc
	       "$cif" #'copilot-chat-fix
	       "$cio" #'copilot-chat-optimize
	       "$cit" #'copilot-chat-test
	       "$cir" #'copilot-chat-review
	       "$cib" #'copilot-chat-review-whole-buffer
	       "$cba" #'copilot-chat-add-current-buffer
	       "$cbx" #'copilot-chat-del-current-buffer
	       "$cbl" #'copilot-chat-list
	       "$cpp" #'copilot-chat-custom-prompt-selection
	       "$cpf" #'copilot-chat-custom-prompt-function
	       "$cpi" #'copilot-chat-ask-and-insert
	       "$cmi" #'copilot-chat-insert-commit-message
	       "$chp" #'copilot-chat-prompt-history-previous
	       "$chn" #'copilot-chat-prompt-history-next)
	      (map! :map copilot-chat-mode-map :localleader
		    "l" #'copilot-chat-prompt-split-and-list
		    "n" #'copilot-chat-prompt-history-next
		    "p" #'copilot-chat-prompt-history-previous
		    "r" #'copilot-chat-review "d" #'copilot-chat-doc
		    "f" #'copilot-chat-fix "o" #'copilot-chat-optimize
		    "t" #'copilot-chat-test "q" #'bury-buffer)
	      (map! :map copilot-chat-shell-mode-map :localleader
		    "l" #'copilot-chat-prompt-split-and-list
		    "n" #'copilot-chat-prompt-history-next
		    "p" #'copilot-chat-prompt-history-previous
		    "r" #'copilot-chat-review "d" #'copilot-chat-doc
		    "f" #'copilot-chat-fix "o" #'copilot-chat-optimize
		    "t" #'copilot-chat-test "q" #'bury-buffer))

(after! copilot-chat
	(map! :map copilot-chat-mode-map :n
              "C-c q" #'aam/doom-bury-and-kill-buffer)
	(map! :map copilot-chat-shell-mode-map :n
              "C-c q" #'aam/doom-bury-and-kill-buffer)
	(map! :map copilot-chat-list-mode-map :n
              "RET" #'copilot-chat-list-add-or-remove-buffer
              "C" #'copilot-chat-list-clear-buffers
              "g" #'copilot-chat-list-refresh
              "q" #'aam/doom-bury-and-kill-buffer))

(use-package! esi-dictate
	      :commands esi-dictate-start
	      :bind (:map esi-dictate-mode-map ("C-g" . esi-dictate-stop))
	      :hook (esi-dictate-speech-final . esi-dictate-fix-context)
	      :init
	      (aam/ported-leader! "$d" #'esi-dictate-start)
	      :config
	      (setq llm-warn-on-nonfree nil))

(use-package! khoj
	      :commands khoj
	      :init
	      (aam/ported-leader! "$k" #'khoj))

(use-package! magit-gptcommit
	      :after (magit llm)
	      :init
	      (when ai-extras-autostart-gptcommit-mode
		(magit-gptcommit-mode -1)
		(magit-gptcommit-status-buffer-setup)))

(when (eq system-type 'gnu/linux)
  (after! mu4e
	  (aam/mail-setup)))

(add-hook! 'emacs-startup-hook
	   (defun aam/doom-startup-journal ()
	     (when (file-exists-p (aam-org-weekly-journal-file))
	       (save-selected-window
		 (split-window-horizontally)
		 (other-window 1)
		 (aam-org-weekly-journal-find-location)))))
