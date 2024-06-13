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
        biblio
        (copilot :requires company
                 :location (recipe
                            :fetcher github
                            :repo "zerolfx/copilot.el"
                            :files ("*.el" "dist")))
        (unicode-math-input :location (recipe
                                       :fetcher github
                                       :repo "astoff/unicode-math-input.el"))
        cloc
        ewmctrl
        fish-completion
        gscholar-bibtex
        helm-system-packages
        key-chord
        key-seq))

(defun aam/post-init-biblio ()
  (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode "lb" 'biblio-lookup)
  (evil-set-initial-state 'biblio-selection-mode 'emacs))

(defun aam/init-copilot ()
  (use-package copilot
    :defer t
    :init
    ;; Define the custom tab function
    (defun aam/my-tab ()
      (interactive)
      (or (copilot-accept-completion)
          (company-indent-or-complete-common nil)))

    ;; Configure company-mode key bindings
    (defun aam/setup-company-key-bindings ()
      ;; Disable inline previews
      (setq company-frontends (delq 'company-preview-if-just-one-frontend company-frontends))
      ;; Enable tab completion
      (define-key company-mode-map (kbd "<tab>") 'aam/my-tab)
      (define-key company-mode-map (kbd "TAB") 'aam/my-tab)
      (define-key company-active-map (kbd "<tab>") 'aam/my-tab)
      (define-key company-active-map (kbd "TAB") 'aam/my-tab))

    (with-eval-after-load 'company
      (aam/setup-company-key-bindings))

    ;; Enable copilot in programming modes
    (add-hook 'prog-mode-hook 'copilot-mode)
    (define-key evil-insert-state-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)
    (define-key evil-insert-state-map (kbd "C-TAB") 'copilot-accept-completion-by-word)

    (spacemacs|diminish copilot-mode "" "C")))

(defun aam/init-unicode-math-input ())

(defun aam/init-cloc()
  :defer t)

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
  (key-seq-define evil-normal-state-map "wm" 'toggle-maximize-buffer)
  ;; easy kill
  (key-seq-define evil-normal-state-map "kf" 'delete-frame)
  (key-seq-define evil-normal-state-map "kw" 'evil-quit)
  (key-seq-define evil-normal-state-map "kb" 'kill-this-buffer))
