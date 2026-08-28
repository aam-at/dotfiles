;;; packages.el --- aam Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defconst aam-packages
  '(
    activity-watch-mode
    biblio
    cape
    casual
    cloc
    direnv
    (explain-pause-mode :location (recipe
                                   :fetcher github
                                   :repo "lastquestion/explain-pause-mode"))
    ewmctrl
    fish-completion
    gscholar-bibtex
    (helm-system-packages :requires helm)
    memoize
    ;; (nova :location (recipe
    ;;                  :fetcher github
    ;;                  :repo "thisisran/nova"))
    popper
    pdf-tools
    corfu
    (unicode-math-input :location (recipe
                                   :fetcher github
                                   :repo "astoff/unicode-math-input.el"))
    (ultra-scroll :location (recipe
                             :fetcher github
                             :repo "jdtsmith/ultra-scroll"))
    pretty-hydra
    yasnippet-capf))

(defun aam/init-activity-watch-mode()
  (use-package activity-watch-mode
    :defer t
    :config
    (spacemacs|diminish activity-watch-mode " Ⓐ" " A")))

(defun aam/post-init-biblio ()
  (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode "lb" 'biblio-lookup)
  (evil-set-initial-state 'biblio-selection-mode 'emacs))

(defun aam/init-casual()
  (use-package casual
    :defer t
    :commands casual-agenda-tmenu
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
					      "A" #'casual-agenda-tmenu)))

(defun aam//disable-company ()
  "Keep Corfu as the sole completion user interface."
  (when company-mode
    (company-mode -1)))

(defun aam/init-cape ()
  (use-package cape
    :defer t
    :init
    (add-hook 'prog-mode-hook #'aam//add-cape-file 0)
    (add-hook 'org-mode-hook #'aam//add-cape-elisp-block 0)
    (add-hook 'markdown-mode-hook #'aam//add-cape-elisp-block 0)
    (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook comint-mode-hook
                                   minibuffer-setup-hook eshell-mode-hook))
      (add-hook hook #'aam//add-cape-dabbrev 20))
    (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
    (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
    (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
    (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
    (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)))

(defun aam//add-cape-dabbrev ()
  (add-hook 'completion-at-point-functions #'cape-dabbrev 20 t))

(defun aam//add-cape-elisp-block ()
  (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t))

(defun aam//add-cape-file ()
  (add-hook 'completion-at-point-functions #'cape-file -10 t))

(defun aam/init-corfu ()
  (use-package corfu
    :init
    ;; Vertico owns minibuffer completion.  Keeping Corfu's child frame out of
    ;; minibuffers avoids face recalculation failures while previewing themes.
    (setq global-corfu-minibuffer nil)
    (global-corfu-mode 1)
    :config
    (setq corfu-auto t
          corfu-cycle t
          corfu-preselect 'prompt
          corfu-count 16
          corfu-max-width 120
          corfu-on-exact-match nil
          corfu-quit-at-boundary 'separator
          corfu-quit-no-match 'separator)
    (add-hook 'evil-insert-state-exit-hook #'corfu-quit)))

(defun aam/post-init-orderless ()
  (use-package orderless
    :demand t
    :config
    (setq completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles orderless partial-completion)))
          orderless-component-separator #'orderless-escapable-split-on-space)))

(defun aam/init-yasnippet-capf ()
  (use-package yasnippet-capf
    :defer t
    :init
    (add-hook 'yas-minor-mode-hook #'aam//add-yasnippet-capf)))

(defun aam//add-yasnippet-capf ()
  (add-hook 'completion-at-point-functions #'yasnippet-capf 30 t))

(defun aam/post-init-company ()
  (use-package company
    :config
    (add-hook 'after-change-major-mode-hook #'aam//disable-company 100)))

(defun aam/init-cloc()
  (use-package cloc
    :defer t))

(defun aam/init-direnv()
  (use-package direnv
    :defer t
    :config
    (direnv-mode)))

(defun aam/init-explain-pause-mode()
  (use-package explain-pause-mode
    :defer t
    :init
    (when aam-enable-explain-pause-at-startup
      (explain-pause-mode))
    :config
    (setf (cadr (assoc 'explain-pause-mode minor-mode-alist)) "")))

(defun aam/init-ewmctrl()
  (use-package ewmctrl
    :defer t
    :init
    (spacemacs/set-leader-keys "Aw" 'ewmctrl)
    :config
    (progn
      (evilified-state-evilify-map ewmctrl-mode-map
				   :mode ewmctrl-mode
				   :bindings
				   ;; General
				   "n"   'next-line
				   "p"   'previous-line
				   "g"   'ewmctrl-refresh
				   ";"   'ewmctrl-toggle-single-key-to-focus
				   ;; Window actions
				   "RET" 'ewmctrl-focus-window
				   "D"   'ewmctrl-delete-window
				   "I"   'ewmctrl-change-window-icon-name
				   "m"   'ewmctrl-move-window-to-other-desktop
				   "M"   'ewmctrl-move-window-to-current-desktop-and-focus
				   "N"   'ewmctrl-change-window-name
				   "r"   'ewmctrl-resize-window
				   ;; Filtering
				   "fc"  'ewmctrl-filters-clear
				   "fd"  'ewmctrl-filter-by-desktop-number
				   "fD"  'ewmctrl-filter-desktop-number-clear
				   "fn"  'ewmctrl-filter-by-name
				   "fN"  'ewmctrl-filter-name-clear
				   "fp"  'ewmctrl-filter-by-pid
				   "fP"  'ewmctrl-filter-pid-clear
				   ;; Sorting
				   "Sd"  'ewmctrl-sort-by-desktop-number
				   "SD"  'ewmctrl-sort-by-desktop-number-reversed
				   "Sn"  'ewmctrl-sort-by-name
				   "SN"  'ewmctrl-sort-by-name-reversed
				   "Sp"  'ewmctrl-sort-by-pid
				   "SP"  'ewmctrl-sort-by-pid-reversed))))

(defun aam/init-fish-completion()
  (use-package fish-completion
    :defer t
    :config
    (when (and (executable-find "fish")
               (require 'fish-completion nil t))
      (global-fish-completion-mode))))

(defun aam/init-gscholar-bibtex()
  (use-package gscholar-bibtex
    :defer t
    :commands gscholar-bibtex
    :init
    (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode "ls" #'gscholar-bibtex)
    :config
    (evil-set-initial-state 'gscholar-bibtex-mode 'emacs)))

(defun aam/init-helm-system-packages()
  (use-package helm-system-packages
    :defer t))

(defun aam/init-memoize ()
  (use-package memoize))

;; (defun aam/init-nova()
;;   (use-package nova
;;     :after vertico-posframe
;;     :config
;;     (require 'nova-vertico)
;;     (nova-vertico-mode -1)
;;     (vertico-posframe-mode 1)
;;     (nova-vertico-mode 1)))

(defun aam/post-init-pdf-tools ()
  (spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode "e" 'aam-extract-pdf-text-from-current-buffer))

(defun aam/init-popper()
  (use-package popper
    :defer t
    :bind (("C-`"   . popper-toggle)
           ("M-`"   . popper-cycle)
           ("C-M-`" . popper-toggle-type))
    :init
    (setq popper-group-function #'popper-group-by-directory
          popper-reference-buffers
          '("\\*Messages\\*"
            "Output\\*$"
            "^\\*Python\\*$" inferior-python-mode
            "\\*Async Shell Command\\*"
            "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
            "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
            "^\\*term.*\\*$"   term-mode   ;term as a popup
            "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
            flycheck-error-list-mode
            haskell-interactive-mode
            help-mode
            compilation-mode))
    (popper-mode t)
    (popper-echo-mode t)))

(defun aam/init-unicode-math-input ()
  (use-package unicode-math-input
    :defer t))

(defun aam/init-ultra-scroll()
  (use-package ultra-scroll
    :init
    (setq scroll-conservatively 101
          scroll-margin 0)
    :config
    (ultra-scroll-mode 1)))

(defun aam/init-pretty-hydra()
  (use-package pretty-hydra
    :defer t))
