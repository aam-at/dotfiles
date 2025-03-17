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
    key-chord
    key-seq
    memoize
    popper
    pdf-tools
    prescient
    company-prescient
    (unicode-math-input :location (recipe
                                   :fetcher github
                                   :repo "astoff/unicode-math-input.el"))
    (ultra-scroll :location local)
    pretty-hydra))

(defun aam/init-activity-watch-mode()
  :defer t
  :config
  (spacemacs|diminish activity-watch-mode " Ⓐ" " A"))

(defun aam/post-init-biblio ()
  (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode "lb" 'biblio-lookup)
  (evil-set-initial-state 'biblio-selection-mode 'emacs))

(defun aam/init-casual()
  :defer t
  :init
  (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
    "A" 'casual-agenda-tmenu))

(defun aam/init-cloc()
  :defer t)

(defun aam/init-direnv()
  :defer t
  :config
  (direnv-mode))

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
  :defer t
  :config
  (progn
    (when (and (executable-find "fish")
               (require 'fish-completion nil t))
      (global-fish-completion-mode))))

(defun aam/init-gscholar-bibtex()
  :defer t
  :init
  (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode "ls" 'gscholar-bibtex)
  :config
  (evil-set-initial-state 'gscholar-bibtex-mode 'emacs))

(defun aam/init-helm-system-packages()
  :defer t)

(defun aam/init-key-chord()
  :defer t
  :init
  (key-chord-mode 1))

(defun aam/init-key-seq ()
  :defer t
  :config
  ;; easy window navigation
  (key-seq-define evil-normal-state-map "wh" 'evil-window-left)
  (key-seq-define evil-normal-state-map "wj" 'evil-window-down)
  (key-seq-define evil-normal-state-map "wk" 'evil-window-up)
  (key-seq-define evil-normal-state-map "wl" 'evil-window-right)
  ;; ;; easy window splitting
  (key-seq-define evil-normal-state-map "wy" 'split-window-right)
  (key-seq-define evil-normal-state-map "wu" 'split-window-below-and-focus)
  (key-seq-define evil-normal-state-map "wi" 'split-window-below)
  (key-seq-define evil-normal-state-map "wo" 'split-window-right-and-focus)
  (key-seq-define evil-normal-state-map "wm" 'spacemacs/toggle-maximize-buffer)
  ;; easy kill
  (key-seq-define evil-normal-state-map "kf" 'delete-frame)
  (key-seq-define evil-normal-state-map "kw" 'evil-quit)
  (key-seq-define evil-normal-state-map "kb" 'kill-this-buffer))

(defun aam/init-memoize ()
  (use-package memoize))


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

(defun aam/init-prescient ()
  (use-package prescient
    :defer t
    :config
    ;; Configure Emacs to use prescient for all completions
    (push 'prescient completion-styles)))

(defun aam/init-company-prescient ()
  (use-package company-prescient
    :after company
    :hook
    (company-mode . company-prescient-mode)))

(defun aam/init-unicode-math-input ())

(defun aam/init-ultra-scroll()
  (use-package ultra-scroll
    :init
    (setq scroll-conservatively 101
          scroll-margin 0)
    :config
    (ultra-scroll-mode 1)))

(defun aam/init-pretty-hydra())
