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
    helm-bibtex gscholar-bibtex
    ;; provide djvu support
    djvu
    sr-speedbar
    hackernews sx
    ;; for viewsing log files
    (hide-lines :location (recipe
                           :fetcher github
                           :repo "emacsmirror/hide-lines"))
    (syslog-mode :location (recipe
                            :fetcher github
                            :repo "vapniks/syslog-mode"))
    protobuf-mode
    password-store
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

(defun aam/post-init-helm-bibtex()
  (spacemacs/set-leader-keys "hc" 'helm-bibtex)
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
  (setq bibtex-completion-additional-search-fields '(keywords tags))

  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (start-process "xournal" "*helm-bibtex-xournal*" "/usr/bin/xournal" fpath)))
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default))))

(defun aam/init-gscholar-bibtex()
  :defer t
  :init
  (spacemacs/set-leader-keys "ol" 'gscholar-bibtex)
  :config
  (evil-set-initial-state 'gscholar-bibtex-mode 'emacs))

(defun aam/init-djvu()
  :defer t)

(defun aam/init-sr-speedbar ()
  :defer t)

(defun aam/init-hackernews()
  :defer t
  :init
  (spacemacs/set-leader-keys "ah" 'hackernews))

(defun aam/init-sx()
  :defer t
  :config
  (setq sx-cache-directory (concat spacemacs-cache-directory "sx"))
  (when (not (file-directory-p sx-cache-directory))
    (make-directory sx-cache-directory)))

(defun aam/init-hide-lines()
  :defer t)

(defun aam/init-syslog-mode()
  :defer t)

(defun aam/init-protobuf-mode()
  :defer t)

(defun aam/init-password-store()
  :defer t
  :config
  (spacemacs/declare-prefix "op" "password store")
  (spacemacs/set-leader-keys
    "opy" 'password-store-copy
    "opg" 'password-store-generate
    "ops" 'password-store-url
    "opi" 'password-store-insert
    "ope" 'password-store-edit
    "opr" 'password-store-rename
    "opI" 'password-store-init
    "opc" 'password-store-clear
    "opD" 'password-store-remove))

(defun aam/init-ewmctrl()
  :defer t
  :init
  (spacemacs/set-leader-keys "aw" 'ewmctrl)
  :config
  (progn
    (evilified-state-evilify ewmctrl-mode ewmctrl-mode-map
      "RET" 'ewmctrl-focus-window
      "D" 'ewmctrl-delete-window
      "g" 'ewmctrl-refresh
      "I" 'ewmctrl-change-window-icon-name
      "fc" 'ewmctrl-filters-clear
      "fd" 'ewmctrl-filter-by-desktop-number
      "fD" 'ewmctrl-filter-desktop-number-clear
      "fn" 'ewmctrl-filter-by-name
      "fN" 'ewmctrl-filter-name-clear
      "fp" 'ewmctrl-filter-by-pid
      "fP" 'ewmctrl-filter-pid-clear
      "m" 'ewmctrl-move-window-to-other-desktop
      "M" 'ewmctrl-move-window-to-current-desktop-and-focus
      "n" 'next-line
      "N" 'ewmctrl-change-window-name
      "p" 'previous-line
      "r" 'ewmctrl-resize-window
      "Sd" 'ewmctrl-sort-by-desktop-number
      "SD" 'ewmctrl-sort-by-desktop-number-reversed
      "Sn" 'ewmctrl-sort-by-name
      "SN" 'ewmctrl-sort-by-name-reversed
      "Sp" 'ewmctrl-sort-by-pid
      "SP" 'ewmctrl-sort-by-pid-reversed
      ";" 'ewmctrl-toggle-single-key-to-focus)
    ))

(defun aam/init-cloc()
  :defer t)
