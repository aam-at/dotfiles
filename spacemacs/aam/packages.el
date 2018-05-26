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
(setq aam-packages
      '(
        ;; FIXME disable for now as it clobbers match data in org-mode buffers
        ;; key-chord key-seq
        ;; citations
        gscholar-bibtex biblio
        ;; provide djvu support
        djvu
        sr-speedbar
        hackernews sx
        helm-system-packages
        (hide-lines :location (recipe
                               :fetcher github
                               :repo "emacsmirror/hide-lines"))
        ;; for viewsing log files
        (syslog-mode :location (recipe
                                :fetcher github
                                :repo "vapniks/syslog-mode"))
        ewmctrl
        cloc))

;; List of packages to exclude.
(setq aam-excluded-packages '())

;; For each package, define a function aam/init-<package-name>
;;
;; (defun aam/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
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

(defun aam/init-gscholar-bibtex()
  :defer t
  :init
  (spacemacs/declare-prefix "As" "scholar")
  (spacemacs/set-leader-keys "Asg" 'gscholar-bibtex)
  :config
  (evil-set-initial-state 'gscholar-bibtex-mode 'emacs))

(defun aam/post-init-biblio ()
  (spacemacs/set-leader-keys "Asb" 'biblio-lookup)
  (evil-set-initial-state 'biblio-selection-mode 'emacs))

(defun aam/init-djvu()
  :defer t)

(defun aam/init-sr-speedbar ()
  :defer t)

(defun aam/init-hackernews()
  :defer t
  :init
  (spacemacs/set-leader-keys "AH" 'hackernews)
  :config
  (evil-set-initial-state 'hackernews-mode 'emacs))

(defun aam/init-sx()
  :defer t
  :config
  (setq sx-cache-directory (concat spacemacs-cache-directory "sx"))
  (when (not (file-directory-p sx-cache-directory))
    (make-directory sx-cache-directory)))

(defun aam/init-helm-system-packages()
  :defer t)

(defun aam/init-hide-lines()
  :defer t)

(defun aam/init-syslog-mode()
  :defer t)

(defun aam/init-ewmctrl()
  :defer t
  :init
  (spacemacs/set-leader-keys "Aw" 'ewmctrl)
  :config
  (progn
    (evilified-state-evilify ewmctrl-mode ewmctrl-mode-map
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
      "SP"  'ewmctrl-sort-by-pid-reversed)))

(defun aam/init-cloc()
  :defer t)
