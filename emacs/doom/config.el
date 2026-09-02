;;; config.el -*- lexical-binding: t; -*-

;; Doom-specific integration for the shared Emacs configuration.

(setq user-full-name "Alexander Matyasko"
      user-mail-address "alexander.matyasko@gmail.com"
      display-line-numbers-type 'relative
      shell-file-name (or (executable-find "bash") shell-file-name))

(setq-default vterm-shell "/bin/fish"
              explicit-shell-file-name "/bin/fish")

(defconst aam/shared-config-dir
  (file-truename (expand-file-name "../config" doom-user-dir)))

(defconst aam/shared-funcs-dir
  (file-truename (expand-file-name "../funcs" doom-user-dir)))

(add-to-list 'load-path aam/shared-config-dir)
(add-to-list 'load-path aam/shared-funcs-dir)

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
      org-enable-gcal t
      org-enable-roam-ui t
      aam-enable-explain-pause-at-startup nil
      aam-enable-magit-gptcommit t)

;; The Org-roam index is derived data.  Keep Doom's copy under its isolated
;; profile state so it can run alongside a Spacemacs profile without SQLite
;; locking the shared database.
(setq org-roam-db-location (expand-file-name "org-roam.db" doom-cache-dir))

(load! "../config/autoloads")

(load! "../funcs/aam-core")
(aam/require-supported-emacs)
(aam/configure-org-paths)
(load! "../funcs/aam-org")
(load! "../funcs/aam-org-roam")
(load! "../funcs/aam-latex")
(load! "../funcs/aam-cpp")

(use-package! explain-pause-mode
              :commands explain-pause-mode
              :init
              (when aam-enable-explain-pause-at-startup
                (explain-pause-mode))
              :config
              (setf (cadr (assq 'explain-pause-mode minor-mode-alist)) ""))

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

;; Vertico remains Doom's primary completion interface; Helm is kept only for
;; the few workflows that are still useful on their own.
(use-package! helm-icons
              :after helm
              :config
              (helm-icons-enable))

(use-package! helm-ls-git
              :commands helm-ls-git
              :init
              (map! :leader
                    :desc "Find project file (Helm)" "p l" #'helm-ls-git)
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
              (map! :leader
                    (:prefix ("p h" . "harpoon")
                             :desc "Quick menu" "." #'harpoon-quick-menu-hydra
                             :desc "Toggle menu" "m" #'harpoon-toggle-quick-menu
                             :desc "Add file" "a" #'harpoon-add-file
                             :desc "Delete item" "d" #'harpoon-delete-item
                             :desc "Go to slot 1" "1" #'harpoon-go-to-1
                             :desc "Go to slot 2" "2" #'harpoon-go-to-2
                             :desc "Go to slot 3" "3" #'harpoon-go-to-3
                             :desc "Go to slot 4" "4" #'harpoon-go-to-4
                             :desc "Go to slot 5" "5" #'harpoon-go-to-5
                             :desc "Go to slot 6" "6" #'harpoon-go-to-6
                             :desc "Go to slot 7" "7" #'harpoon-go-to-7
                             :desc "Go to slot 8" "8" #'harpoon-go-to-8
                             :desc "Go to slot 9" "9" #'harpoon-go-to-9)))

(use-package! helpful
              :commands (helpful-callable helpful-variable helpful-key)
              :init
              (map! :leader
                    (:prefix ("h" . "help")
                             :desc "Describe callable" "f" #'helpful-callable
                             :desc "Describe variable" "v" #'helpful-variable
                             :desc "Describe key" "k" #'helpful-key)))

(use-package! bm
              :commands (bm-toggle bm-next bm-previous bm-show-all)
              :init
              (map! :leader
                    (:prefix ("b j" . "buffer marks")
                             :desc "Toggle mark" "t" #'bm-toggle
                             :desc "Next mark" "n" #'bm-next
                             :desc "Previous mark" "p" #'bm-previous
                             :desc "List marks" "l" #'bm-show-all)))

(use-package! magit-gitflow
              :after magit)

(use-package! casual
              :after org-agenda
              :init
              (map! :map org-agenda-mode-map :localleader
                    :desc "Casual agenda menu" "A" #'casual-agenda-tmenu))

(use-package! biblio
              :commands biblio-lookup
              :init
              (map! :map bibtex-mode-map :localleader
                    :desc "Biblio lookup" "b" #'biblio-lookup)
              :config
              (evil-set-initial-state 'biblio-selection-mode 'emacs))

(use-package! gscholar-bibtex
              :commands gscholar-bibtex
              :init
              (map! :map bibtex-mode-map :localleader
                    :desc "Google Scholar lookup" "s" #'gscholar-bibtex)
              :config
              (evil-set-initial-state 'gscholar-bibtex-mode 'emacs))

(map! :map bibtex-mode-map :localleader
      :desc "Generate citation key" "g" #'aam/bibtex-generate-autokey
      :desc "Entry actions" "h" #'org-ref-bibtex-entry-menu
      :desc "New entry" "i" #'org-ref-bibtex-new-entry-menu
      :desc "Next entry" "j" #'org-ref-bibtex-next-entry
      :desc "Previous entry" "k" #'org-ref-bibtex-previous-entry
      :desc "Open bibliography notes" "n" #'org-ref-open-bibtex-notes
      :desc "Open bibliography PDF" "p" #'org-ref-open-bibtex-pdf
      :desc "Open in browser (org-ref)" "B" #'org-ref-open-in-browser
      :desc "Sort entry (org-ref)" "S" #'org-ref-sort-bibtex-entry)

(use-package! helm-bibtex
              :commands helm-bibtex
              :init
              (map! :map bibtex-mode-map :localleader
                    :desc "Helm BibTeX" "m" #'helm-bibtex))

(use-package! ewmctrl
              :commands ewmctrl
              :init
              (map! :leader
                    :desc "Window manager" "o w" #'ewmctrl)
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
              :commands (write-or-die-mode write-or-die-toggle)
              :init
              (map! :leader
                    :desc "Toggle Write or Die" "t W d" #'write-or-die-toggle))

(use-package! writeroom-mode
              :commands writeroom-mode
              :init
              (map! :leader
                    :desc "Toggle writeroom" "t W r" #'writeroom-mode)
              :config
              (setq writeroom-width 90))

(use-package! writegood-mode
              :commands writegood-mode
              :init
              (map! :leader
                    :desc "Toggle writegood" "t W g" #'writegood-mode))

(use-package! mw-thesaurus
              :commands mw-thesaurus-lookup-dwim
              :hook (variable-pitch-mode . mw-thesaurus-mode))

(use-package! jinx
              :hook (doom-first-input . global-jinx-mode)
              :init
              (map! [remap ispell-word] #'jinx-correct
                    [remap evil-next-flyspell-error] #'jinx-next
                    [remap evil-prev-flyspell-error] #'jinx-previous))

(use-package! synosaurus
              :commands synosaurus-lookup
              :hook ((text-mode markdown-mode) . synosaurus-mode)
              :config
              (setq synosaurus-choose-method 'default))

(use-package! words
              :commands (words words-hydra/body))

(use-package! powerthesaurus
              :commands (powerthesaurus-lookup-synonyms-dwim
                         powerthesaurus-lookup-antonyms-dwim
                         powerthesaurus-lookup-related-dwim
                         powerthesaurus-lookup-definitions-dwim
                         powerthesaurus-lookup-sentences-dwim))

(use-package! le-thesaurus
              :commands (le-thesaurus-get-synonyms le-thesaurus-get-antonyms))

(use-package! academic-phrases
              :commands (academic-phrases academic-phrases-by-section))

(map! :leader
      (:prefix ("s w" . "words")
               :desc "Synonyms" "s" #'powerthesaurus-lookup-synonyms-dwim
               :desc "Antonyms" "a" #'powerthesaurus-lookup-antonyms-dwim
               :desc "Related words" "r" #'powerthesaurus-lookup-related-dwim
               :desc "Definitions" "d" #'powerthesaurus-lookup-definitions-dwim
               :desc "Example sentences" "e" #'powerthesaurus-lookup-sentences-dwim
               :desc "Libre thesaurus synonyms" "l" #'le-thesaurus-get-synonyms
               :desc "Libre thesaurus antonyms" "L" #'le-thesaurus-get-antonyms
               :desc "Merriam-Webster thesaurus" "m" #'mw-thesaurus-lookup-dwim
               :desc "Synosaurus" "y" #'synosaurus-lookup
               :desc "Words menu" "w" #'words-hydra/body
               :desc "Academic phrases" "p" #'academic-phrases
               :desc "Academic phrases by section" "P" #'academic-phrases-by-section))

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
					   "--config" (eval (expand-file-name "~/.textlintrc"))
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

;; Dape is the primary Doom debugger. Keep dap-mode as a secondary entry point
;; for the configured LLDB and cpptools flows.
(use-package! dap-mode
              :after lsp-mode
              :commands (dap-debug dap-hydra)
              :init
              (map! :leader
                    (:prefix ("d a" . "dap")
                             :desc "Start DAP debug" "d" #'dap-debug
                             :desc "DAP hydra" "h" #'dap-hydra))
              :config
              (require 'dap-cpptools nil t)
              (require 'dap-lldb nil t))

(use-package! ebib
              :commands ebib
              :init
              (map! :leader
                    :desc "Ebib bibliography manager" "n B" #'ebib))

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
              (:prefix ("R" . "research")
                       :desc "Remove overlays" "o" #'aam/org-remove-all-overlays
                       :desc "Sort by year" "y" #'aam/org-sort-entries-by-year
                       :desc "Filter by year" "Y" #'aam/org-filter-entries-by-year
                       :desc "Sort by citations" "c" #'aam/org-sort-entries-by-citations
                       :desc "Filter by citations" "C" #'aam/org-filter-entries-by-citations
                       :desc "Update citations" "u" #'aam/org-citations-update-at-point)
              :desc "Convert ID link to file link" "l f" #'aam/org-convert-org-id-link-to-file-link
              :desc "Insert org-ref link" "l r" #'org-ref-insert-ref-link
              :desc "Insert org-ref label" "l R" #'org-ref-insert-label-link
              :desc "Open org-ref note" "l n" #'org-ref-open-notes-at-point
              (:prefix ("B" . "bibliography")
                       :desc "Insert citation" "i" #'org-cite-insert
                       :desc "Open citation resources" "o" #'citar-open
                       :desc "Open citation note" "n" #'citar-open-note
                       :desc "Insert legacy org-ref citation" "r" #'org-ref-insert-link)
              (:prefix ("C" . "recent clocks")
                       :desc "Clock in" "i" #'org-mru-clock-in
                       :desc "Go to clock" "g" #'org-mru-clock-goto
                       :desc "Select recent task" "s" #'org-mru-clock-select-recent-task)
              (:prefix ("O" . "noter")
                       :desc "Org noter" "n" #'org-noter)
              :desc "Roam bibliography actions" "N" #'orb-note-actions))

(use-package! org-protocol-capture-html
              :after org)

(use-package! org-doing
              :after org
              :commands org-doing
              :init
              (map! :leader
                    :desc "Org doing" "n D" #'org-doing))

(use-package! org-transclusion
              :after org
              :commands org-transclusion-transient-menu
              :init
              (map! :map org-mode-map :localleader
                    :desc "Transclusion menu" "l x" #'org-transclusion-transient-menu))

(use-package! org-contacts
              :after org
              :commands (org-contacts org-contacts-agenda org-contacts-completing-read))

(use-package! org-remark
              :after org
              ;; Desktop restoration may reactivate `org-remark-icon-mode' before
              ;; the first input, so this feature must precede saved-buffer restore.
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
              :commands org-epub-export-to-epub)

(use-package! org-re-reveal
              :after org
              :commands (org-re-reveal-export-to-html
                         org-re-reveal-export-to-html-and-browse
                         org-re-reveal-export-current-subtree))

(after! org
        (map! :map org-mode-map :localleader
              (:prefix ("D" . "delve")
                       :desc "Open Delve" "d" #'delve
                       :desc "Collect actions" "c" #'delve-minor-mode-collect-actions
                       :desc "Edit actions" "e" #'delve-minor-mode-edit-actions
                       :desc "Inspect actions" "i" #'delve-minor-mode-inspect-actions))
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
              :commands (org-gcal-sync org-gcal-fetch org-gcal-post-at-point
                                       org-gcal-refresh-token)
              :after org
              :init
              (map! :leader
                    (:prefix ("n g" . "org calendar")
                             :desc "Sync" "s" #'org-gcal-sync
                             :desc "Fetch" "f" #'org-gcal-fetch
                             :desc "Post at point" "p" #'org-gcal-post-at-point
                             :desc "Refresh token" "r" #'org-gcal-refresh-token))
              (map! :map org-mode-map :localleader
                    (:prefix ("G" . "calendar")
                             :desc "Sync" "s" #'org-gcal-sync
                             :desc "Fetch" "f" #'org-gcal-fetch
                             :desc "Post at point" "p" #'org-gcal-post-at-point
                             :desc "Refresh token" "r" #'org-gcal-refresh-token))
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

(defun aam/org-ref-enable ()
  "Load legacy org-ref link support when an Org buffer is first visited."
  (require 'org-ref))

(use-package! org-ref
              :after org
              :commands (org-ref-insert-ref-link org-ref-insert-label-link
                                                 org-ref-insert-link org-ref-open-notes-at-point
                                                 org-ref-open-in-browser org-ref-bibtex-entry-menu
                                                 org-ref-bibtex-new-entry-menu org-ref-bibtex-next-entry
                                                 org-ref-bibtex-previous-entry org-ref-open-bibtex-notes
                                                 org-ref-open-bibtex-pdf org-ref-sort-bibtex-entry)
              :hook (org-mode . aam/org-ref-enable)
              :config
              (advice-add 'org-ref-open-notes-at-point :override #'aam/org-ref-open-roam-note)
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

(defcustom aam/org-roam-ui-port-search-limit 100
  "Number of HTTP ports to try when starting Org-roam UI."
  :type 'integer
  :group 'org-roam)

(defvar aam/org-roam-ui-websocket-port nil
  "WebSocket port selected for the current Org-roam UI session.")

(defvar aam/org-roam-ui-original-app-build-dir nil
  "Unmodified Org-roam UI web build used to make port-specific copies.")

(defvar aam/org-roam-ui-default-port nil
  "Configured Org-roam UI HTTP port before fallback selection occurs.")

(defun aam/org-roam-ui--web-build-for-ports (http-port websocket-port)
  "Return an Org-roam UI web build configured for HTTP-PORT and WEBSOCKET-PORT."

  (let* ((source (or aam/org-roam-ui-original-app-build-dir
                     (setq aam/org-roam-ui-original-app-build-dir
                           org-roam-ui-app-build-dir)))
         (target (expand-file-name
                  (format "org-roam-ui-%d-%d" http-port websocket-port)
                  doom-cache-dir))
         (marker (expand-file-name ".aam-port-configured" target)))
    (unless (file-exists-p marker)
      (make-directory target t)
      (copy-directory source target nil t t)
      ;; The upstream static client hard-codes its service endpoints.  Rewrite
      ;; only the cached copy so a fallback port remains fully functional.
      (dolist (file (directory-files-recursively target "\\.\\(?:html\\|js\\)$"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (search-forward "localhost:35901" nil t)
            (replace-match (format "localhost:%d" http-port) t t))
          (goto-char (point-min))
          (while (search-forward "localhost:35903" nil t)
            (replace-match (format "localhost:%d" websocket-port) t t))
          (write-region (point-min) (point-max) file nil 'silent)))
      (write-region "" nil marker nil 'silent))
    target))

(defun aam/org-roam-ui--enable-with-ports (http-port websocket-port)
  "Enable Org-roam UI with HTTP-PORT and WEBSOCKET-PORT.

Org-roam UI currently hard-codes its WebSocket port internally, so bind its
server constructor only while enabling the mode."
  (require 'cl-lib)
  (let ((websocket-server-function (symbol-function 'websocket-server)))
    (setq org-roam-ui-port http-port
          aam/org-roam-ui-websocket-port websocket-port
          org-roam-ui-app-build-dir
          (aam/org-roam-ui--web-build-for-ports http-port websocket-port))
    (cl-letf (((symbol-function 'websocket-server)
               (lambda (port &rest args)
                 (apply websocket-server-function
                        (if (= port 35903) websocket-port port)
                        args))))
      (org-roam-ui-mode 1))))

(defun aam/org-roam-ui-start ()
  "Start Org-roam UI on the first free localhost port pair.

The default HTTP port is tried first.  Each subsequent attempt increments the
HTTP port by one and keeps the WebSocket offset used by Org-roam UI."
  (interactive)
  (require 'org-roam-ui)
  (if org-roam-ui-mode
      (when (called-interactively-p 'interactive)
        (org-roam-ui-open))
    (let ((initial-port
           (or aam/org-roam-ui-default-port
               (setq aam/org-roam-ui-default-port org-roam-ui-port)))
          (attempt 0)
          started)
      (while (and (not started) (< attempt aam/org-roam-ui-port-search-limit))
        (let* ((http-port (+ initial-port attempt))
               (websocket-port (+ http-port 2)))
          (unless (or (aam-check-localhost-port http-port)
                      (aam-check-localhost-port websocket-port))
            (condition-case err
                (progn
                  (aam/org-roam-ui--enable-with-ports http-port websocket-port)
                  (setq started t)
                  (message "Org-roam UI started on http://localhost:%d" http-port))
              (error
               ;; A competing process can claim a port after the availability
               ;; check.  Clean up and continue with the next candidate.
               (when org-roam-ui-mode
                 (ignore-errors (org-roam-ui-mode -1)))
               (message "Org-roam UI port %d unavailable: %s" http-port
                        (error-message-string err))))))
        (setq attempt (1+ attempt)))
      (unless started
        (user-error "Org-roam UI could not find a free port after %d attempts"
                    aam/org-roam-ui-port-search-limit))
      (when (called-interactively-p 'interactive)
        (org-roam-ui-open)))))

(after! org-roam
        (require 'org-roam-protocol)
        (org-roam-db-autosync-mode 1)
        (require 'org-roam-bibtex nil t)
        (org-roam-bibtex-mode 1)
        (when (and org-enable-roam-ui
                   (require 'org-roam-ui nil t))
          (aam/org-roam-ui-start)))

(use-package! citar-org-roam
              :after (citar org-roam)
              :config
              (setq citar-org-roam-capture-template-key "c"
                    citar-org-roam-note-title-template "${author editor}, ${title}"
                    citar-org-roam-template-fields
                    '((:citar-title . ("title"))
                      (:citar-author . ("author" "editor"))
                      (:citar-date . ("date" "year" "issued"))
                      (:citar-journal . ("journaltitle" "journal"))
                      (:citar-doi . ("doi"))
                      (:citar-url . ("url"))))
              (citar-org-roam-mode 1))

(after! org-roam
        (map! :map org-mode-map :localleader
              :desc "Vulpea find" "m v" #'vulpea-find
              :desc "Vulpea insert" "m V" #'vulpea-insert
              :desc "Vulpea backlinks" "m b" #'vulpea-find-backlink
              :desc "Toggle roam properties" "m T" #'aam/org-roam-toggle-properties)
        (map! :leader
              :desc "Toggle roam properties" "n r T" #'aam/org-roam-toggle-properties))

(use-package! vulpea
              :after org-roam
              :commands (vulpea-find vulpea-insert vulpea-find-backlink)
              :config
              (setq vulpea-db-sync-directories (list org-directory))
              (vulpea-db-autosync-mode 1)
              (map! :leader
                    :desc "Vulpea find" "n r v" #'vulpea-find
                    :desc "Vulpea insert" "n r V" #'vulpea-insert
                    :desc "Vulpea backlinks" "n r b" #'vulpea-find-backlink))

(use-package! org-mru-clock
              :after org
              :commands (org-mru-clock-in org-mru-clock-goto org-mru-clock-select-recent-task)
              :config
              (setq org-mru-clock-how-many 100)
              (add-hook 'minibuffer-setup-hook #'org-mru-clock-embark-minibuffer-hook)
              (map! :leader
                    (:prefix ("n M" . "recent clocks")
                             :desc "Clock in" "i" #'org-mru-clock-in
                             :desc "Go to clock" "g" #'org-mru-clock-goto
                             :desc "Select recent task" "s" #'org-mru-clock-select-recent-task)))

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
                    :desc "Similarity sidebuffer" "R s" #'org-similarity-sidebuffer
                    :desc "Similarity query" "R q" #'org-similarity-query))

(use-package! org-fragtog
              :after org
              :hook (org-mode . org-fragtog-mode))

(after! pdf-tools
        (map! :map pdf-view-mode-map
              :localleader
              :desc "Extract text" "e" #'aam-extract-pdf-text-from-current-buffer
              :desc "Org noter" "N" #'org-noter))

(after! python
        (aam/python-setup)
        (when (aam/eglot-client-p)
          ;; `set-eglot-client!' tries alternatives from left to right.
          (set-eglot-client! '(python-mode python-ts-mode)
                             '("ty" "server")
                             '("pyrefly" "lsp")))
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
              (map! :map LaTeX-mode-map :localleader
                    :desc "Toggle continuous latexmk" "C" #'auctex-cont-latexmk-toggle))

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
              (map! :map LaTeX-mode-map :localleader
                    :desc "Texpresso preview" "x" #'texpresso))

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
        (map! :leader
              :desc "Clang format buffer/region" "c F" #'aam/cpp-format-region-or-buffer))

(use-package! langtool
              :commands (langtool-check langtool-correct-buffer)
              :init
              (setq langtool-default-language "en-US"
                    langtool-http-server-host "localhost"
                    langtool-http-server-port 8088)
              (map! :leader
                    :desc "LanguageTool check" "s w g" #'langtool-check))

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

(use-package! ellama
              :commands ellama
              :init
              (map! :leader
                    :desc "Ellama" "o l E" #'ellama))

(use-package! whisper
              :commands (whisper-transcribe-fast whisper-transcribe)
              :init
              (map! :leader
                    :desc "Whisper fast transcription" "o l w" #'whisper-transcribe-fast
                    :desc "Whisper accurate transcription" "o l W" #'whisper-transcribe))

(use-package! copilot
              :hook (prog-mode . copilot-mode)
              :config
              (map! :map copilot-completion-map
                    "TAB" #'copilot-accept-completion
                    "<tab>" #'copilot-accept-completion
                    "C-TAB" #'copilot-accept-completion-by-word
                    "C-<tab>" #'copilot-accept-completion-by-word))

(use-package! shell-maker
              :defer t)

(use-package! copilot-chat
              :commands (copilot-chat-display copilot-chat-switch-to-buffer copilot-chat-reset)
              :init
              (map! :leader
                    :desc "Open Copilot Chat" "o l c" #'copilot-chat-switch-to-buffer
                    :desc "Display Copilot Chat" "o l C" #'copilot-chat-display
                    (:prefix ("c A" . "copilot")
                             :desc "Explain selection" "e" #'copilot-chat-explain
                             :desc "Explain defun" "E" #'copilot-chat-explain-defun
                             :desc "Explain symbol" "s" #'copilot-chat-explain-symbol-at-line
                             :desc "Document" "d" #'copilot-chat-doc
                             :desc "Fix" "f" #'copilot-chat-fix
                             :desc "Optimize" "o" #'copilot-chat-optimize
                             :desc "Test" "t" #'copilot-chat-test
                             :desc "Review selection" "r" #'copilot-chat-review
                             :desc "Review buffer" "R" #'copilot-chat-review-whole-buffer
                             :desc "Custom prompt" "p" #'copilot-chat-custom-prompt-selection
                             :desc "Prompt function" "P" #'copilot-chat-custom-prompt-function
                             :desc "Ask and insert" "i" #'copilot-chat-ask-and-insert)
                    (:prefix ("b A" . "copilot context")
                             :desc "Add current buffer" "a" #'copilot-chat-add-current-buffer
                             :desc "Remove current buffer" "d" #'copilot-chat-del-current-buffer
                             :desc "List buffers" "l" #'copilot-chat-list)
                    :desc "Copilot commit message" "g c m" #'copilot-chat-insert-commit-message)
              (map! :map copilot-chat-mode-map :localleader
                    "M" #'copilot-chat-set-model
                    "R" #'copilot-chat-reset
                    "l" #'copilot-chat-prompt-split-and-list
                    "n" #'copilot-chat-prompt-history-next
                    "p" #'copilot-chat-prompt-history-previous
                    "r" #'copilot-chat-review "d" #'copilot-chat-doc
                    "f" #'copilot-chat-fix "o" #'copilot-chat-optimize
                    "t" #'copilot-chat-test "q" #'bury-buffer)
              (map! :map copilot-chat-shell-mode-map :localleader
                    "M" #'copilot-chat-set-model
                    "R" #'copilot-chat-reset
                    "l" #'copilot-chat-prompt-split-and-list
                    "n" #'copilot-chat-prompt-history-next
                    "p" #'copilot-chat-prompt-history-previous
                    "r" #'copilot-chat-review "d" #'copilot-chat-doc
                    "f" #'copilot-chat-fix "o" #'copilot-chat-optimize
                    "t" #'copilot-chat-test "q" #'bury-buffer))

(after! copilot-chat
        (map! :map copilot-chat-mode-map :n
              "C-c q" #'aam/bury-buffer-and-delete-window)
        (map! :map copilot-chat-shell-mode-map :n
              "C-c q" #'aam/bury-buffer-and-delete-window)
        (map! :map copilot-chat-list-mode-map :n
              "RET" #'copilot-chat-list-add-or-remove-buffer
              "C" #'copilot-chat-list-clear-buffers
              "g" #'copilot-chat-list-refresh
              "q" #'aam/bury-buffer-and-delete-window))

(use-package! esi-dictate
              :commands esi-dictate-start
              :bind (:map esi-dictate-mode-map ("C-g" . esi-dictate-stop))
              :hook (esi-dictate-speech-final . esi-dictate-fix-context)
              :init
              (map! :leader
                    :desc "Start dictation" "o l d" #'esi-dictate-start)
              :config
              (setq llm-warn-on-nonfree nil))

(use-package! khoj
              :commands khoj
              :init
              (map! :leader
                    :desc "Open Khoj" "o l k" #'khoj))

(use-package! magit-gptcommit
              :after (magit llm)
              :init
              (when aam-enable-magit-gptcommit
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
