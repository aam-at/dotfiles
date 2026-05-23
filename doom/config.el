;;; config.el -*- lexical-binding: t; -*-

;; Doom port of the Spacemacs configuration in this repository.

(setq user-full-name "Alexander Matyasko"
      user-mail-address "alexander.matyasko@gmail.com"
      doom-theme 'doom-one
      display-line-numbers-type 'relative
      doom-localleader-key ","
      shell-file-name (or (executable-find "bash") shell-file-name))

(setq-default vterm-shell "/bin/fish"
              explicit-shell-file-name "/bin/fish")

(defconst aam/spacemacs-root
  (file-truename (expand-file-name "../spacemacs" doom-user-dir)))

(defconst aam/spacemacs-config-dir
  (expand-file-name "config" aam/spacemacs-root))

(add-to-list 'load-path aam/spacemacs-config-dir)
(add-to-list 'load-path (expand-file-name "aam" aam/spacemacs-root))
(add-to-list 'load-path (expand-file-name "org-extras" aam/spacemacs-root))
(add-to-list 'load-path (expand-file-name "org-roam-extras" aam/spacemacs-root))
(add-to-list 'load-path (expand-file-name "latex-extras" aam/spacemacs-root))
(add-to-list 'load-path (expand-file-name "cpp-extras" aam/spacemacs-root))

(setq doom-font
      (font-spec :family "JetBrains Mono"
                 :size (if (and (display-graphic-p)
                                (> (display-pixel-width) 3000))
                           20
                         14))
      doom-variable-pitch-font (font-spec :family "iA Writer Mono S" :size 14)
      doom-serif-font (font-spec :family "iA Writer Mono S" :size 14))

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq package-native-compile t
        native-comp-deferred-compilation t))

(setq max-lisp-eval-depth 10000
      inhibit-x-resources t
      find-file-visit-truename t
      lsp-use-plists t)
(setenv "LSP_USE_PLISTS" "true")

(setq aam/org-root (file-truename (expand-file-name "~/Dropbox/Org/")))
(defun aam/org-path (path)
  (expand-file-name path aam/org-root))

(setq aam/bib-root (expand-file-name "~/Google Drive/Research/Bibliography/"))
(defun aam/bib-path (path)
  (expand-file-name path aam/bib-root))

(setq aam/bibtex-files (list (aam/bib-path "refs.bib")
                             (aam/bib-path "review.bib")
                             (aam/bib-path "books.bib")
                             (aam/bib-path "myrefs.bib"))
      org-directory aam/org-root
      org-roam-directory aam/org-root
      org-contacts-files (list (aam/org-path "contacts.org"))
      org-projectile-file "TODOs.org")

(setq org-enable-delve t
      org-enable-doing-notifier t
      org-enable-gcal nil
      org-enable-roam-ui nil
      aam-enable-explain-pause-at-startup nil
      ai-extras-autostart-gptcommit-mode t)

;; Compatibility helpers for the Spacemacs Lisp this Doom config reuses.
(defvar spacemacs-cache-directory doom-cache-dir)
(defvar layouts-enable-autosave nil)
(defvar spacemacs-buffer-startup-lists-length nil)

(defun spacemacs/system-is-linux ()
  (eq system-type 'gnu/linux))

(defun spacemacs/system-is-mswindows ()
  (memq system-type '(windows-nt ms-dos cygwin)))

(defmacro spacemacs/set-leader-keys (&rest bindings)
  `(map! :leader ,@bindings))

(defmacro spacemacs/set-leader-keys-for-major-mode (mode &rest bindings)
  (let ((map (intern (format "%s-map" mode))))
    `(map! :localleader :map ,map ,@bindings)))

(defmacro spacemacs/declare-prefix (&rest _)
  nil)

(defmacro spacemacs/declare-prefix-for-mode (&rest _)
  nil)

(defmacro spacemacs|diminish (mode &optional lighter _short-lighter)
  `(with-eval-after-load 'diminish
     (when (fboundp 'diminish)
       (diminish ',mode ,lighter))))

(defmacro spacemacs|do-after-display-system-init (&rest body)
  `(if (daemonp)
       (add-hook 'after-make-frame-functions
                 (lambda (_frame) ,@body))
     (when (display-graphic-p)
       ,@body)))

(defun spacemacs/set-default-font (font)
  (setq doom-font (apply #'font-spec :family (car font) (cdr font)))
  (when (display-graphic-p)
    (set-frame-font doom-font t t)))

(defun spacemacs/toggle-display-fill-column-indicator-on ()
  (display-fill-column-indicator-mode 1))

(defun spacemacs/toggle-relative-line-numbers-on ()
  (setq-local display-line-numbers 'relative)
  (display-line-numbers-mode 1))

(defun spacemacs/toggle-auto-fill-mode-on ()
  (auto-fill-mode 1))

(unless (fboundp 'split-window-below-and-focus)
  (defun split-window-below-and-focus ()
    (interactive)
    (split-window-below)
    (other-window 1)))

(unless (fboundp 'split-window-right-and-focus)
  (defun split-window-right-and-focus ()
    (interactive)
    (split-window-right)
    (other-window 1)))

(unless (fboundp 'spacemacs/toggle-maximize-buffer)
  (defalias 'spacemacs/toggle-maximize-buffer #'doom/window-maximize-buffer))

(unless (fboundp 'ensure-list)
  (defun ensure-list (object)
    (if (listp object) object (list object))))

(load! "../spacemacs/aam/funcs")
(load! "../spacemacs/org-extras/funcs")
(load! "../spacemacs/org-roam-extras/funcs")
(load! "../spacemacs/latex-extras/funcs")
(load! "../spacemacs/cpp-extras/funcs")

(use-package! keychain-environment
  :config
  (keychain-refresh-environment))

(use-package! gcmh
  :config
  (gcmh-mode 1))

(use-package! direnv
  :config
  (direnv-mode))

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

(use-package! company-prescient
  :after company
  :hook (company-mode . company-prescient-mode))

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
  (key-seq-define evil-normal-state-map "wu" #'split-window-below-and-focus)
  (key-seq-define evil-normal-state-map "wi" #'split-window-below)
  (key-seq-define evil-normal-state-map "wo" #'split-window-right-and-focus)
  (key-seq-define evil-normal-state-map "wm" #'spacemacs/toggle-maximize-buffer)
  (key-seq-define evil-normal-state-map "kf" #'delete-frame)
  (key-seq-define evil-normal-state-map "kw" #'evil-quit)
  (key-seq-define evil-normal-state-map "kb" #'kill-this-buffer))

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
  (map! :leader "xD" #'write-or-die-mode))

(use-package! jinx
  :hook (doom-first-input . global-jinx-mode)
  :init
  (map! [remap ispell-word] #'jinx-correct
        [remap evil-next-flyspell-error] #'jinx-next
        [remap evil-prev-flyspell-error] #'jinx-previous))

(use-package! synosaurus
  :hook ((text-mode markdown-mode) . synosaurus-mode)
  :init
  (map! :leader "Stw" #'synosaurus-lookup)
  :config
  (setq synosaurus-choose-method 'default))

(use-package! words
  :commands (words words-hydra/body)
  :init
  (map! :leader "Sw" #'words-hydra/body))

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
  (map! :leader "xW" #'writeroom-mode))

(after! writegood-mode
  (map! :leader "xG" #'writegood-mode))

(map! :leader
      "Sts" #'powerthesaurus-lookup-synonyms-dwim
      "Sta" #'powerthesaurus-lookup-antonyms-dwim
      "Str" #'powerthesaurus-lookup-related-dwim
      "Std" #'powerthesaurus-lookup-definitions-dwim
      "Ste" #'powerthesaurus-lookup-sentences-dwim
      "Stl" #'le-thesaurus-get-synonyms
      "StL" #'le-thesaurus-get-antonyms
      "Stm" #'mw-thesaurus-lookup-dwim)

(autoload 'my-common-setup "config-common" nil t)
(autoload 'my-lsp-setup "config-lsp" nil t)
(autoload 'my-secure-setup "config-secure" nil t)
(autoload 'my-ai-setup "config-ai" nil t)
(autoload 'my-org-setup "config-org" nil t)
(autoload 'my-python-setup "config-python" nil t)
(autoload 'my-tex-setup "config-tex" nil t)
(autoload 'my-bibtex-setup "config-bibtex" nil t)
(autoload 'my-mail-setup "config-mail" nil t)

(my-common-setup)

(condition-case err
    (my-secure-setup)
  (error
   (message "Secure config was not loaded: %s" (error-message-string err))))

(after! lsp-mode
  (my-lsp-setup))

(after! org
  (my-org-setup)
  (require 'org-protocol)
  (add-to-list 'org-modules 'org-protocol)
  (add-to-list 'org-modules 'org-roam-protocol)
  (org-super-agenda-mode 1)
  (add-hook 'org-mode-hook #'turn-on-org-cdlatex)
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append)
  (map! :map org-mode-map
        :localleader
        "Sr" #'org-extras/remove-all-overlays
        "Sy" #'org-extras/sort-entries-by-year
        "SY" #'org-extras/filter-entries-by-year
        "Sc" #'org-extras/sort-entries-by-citations
        "SC" #'org-extras/filter-entries-by-citations
        "Su" #'org-extras/citations-update-at-point
        "uf" #'org-extras/convert-org-id-link-to-file-link
        "N" #'orb-note-actions))

(after! org-roam
  (require 'org-roam-protocol)
  (org-roam-db-autosync-mode 1)
  (require 'org-roam-bibtex nil t)
  (org-roam-bibtex-mode 1)
  (when (require 'org-roam-ui nil t)
    (org-roam-ui-mode 1)))

(use-package! vulpea
  :after org-roam
  :config
  (setq vulpea-db-sync-directories (list org-directory))
  (vulpea-db-autosync-mode 1)
  (map! :leader
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
  (map! :leader
        "aoCi" #'org-mru-clock-in
        "aoCg" #'org-mru-clock-goto
        "aoCs" #'org-mru-clock-select-recent-task)
  (map! :map org-mode-map
        :localleader
        "Ci" #'org-mru-clock-in
        "Cg" #'org-mru-clock-goto
        "Cs" #'org-mru-clock-select-recent-task))

(use-package! org-doing-notifier
  :commands (org-doing-notifier-start org-doing-notifier-stop org-doing-notifier-toggle)
  :init
  (org-doing-notifier-start))

(use-package! delve
  :after org-roam
  :hook ((delve-mode . delve-compact-view-mode)
         (delve-mode . hl-line-mode))
  :config
  (delve-global-minor-mode 1))

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
  (my-python-setup)
  (set-formatter! 'ruff :modes '(python-mode python-ts-mode)))

(after! tex
  (require 'polymode nil t)
  (my-tex-setup))

(after! bibtex
  (my-bibtex-setup))

(after! gptel
  (my-ai-setup))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :config
  (map! :map copilot-completion-map
        "TAB" #'copilot-accept-completion
        "<tab>" #'copilot-accept-completion
        "C-TAB" #'copilot-accept-completion-by-word
        "C-<tab>" #'copilot-accept-completion-by-word))

(use-package! copilot-chat
  :commands (copilot-chat-display copilot-chat-switch-to-buffer copilot-chat-reset)
  :init
  (map! :leader
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
        "$cmi" #'copilot-chat-insert-commit-message))

(when (eq system-type 'gnu/linux)
  (after! mu4e
    (my-mail-setup)))

(add-hook! 'emacs-startup-hook
  (defun aam/doom-startup-journal ()
    (when (and (fboundp 'org-weekly-journal-file)
               (file-exists-p (org-weekly-journal-file)))
      (save-selected-window
        (split-window-horizontally)
        (other-window 1)
        (org-weekly-journal-find-location)))))
