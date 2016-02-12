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
    ;; integration for orgmode and habitrpg.
    (habitrpg :location (recipe
                            :fetcher github
                            :repo "ryjm/habitrpg.el"))
    key-chord key-seq
    helm-bibtex
    ;; provide djvu support
    djvu
    ;; Twitter hackernews stackexchange
    twittering-mode hackernews sx
    ;; some evil extras
    evil-visual-mark-mode
    ;; for viewsing log files
    syslog-mode
    password-store
    cloc
    ztree))

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
(defun aam/init-habitrpg()
  (use-package habitrpg
    :defer t
    :init
    (setq
     habitrpg-api-url "https://habitica.com/api/v2")))

(defun aam/init-key-chord()
  (use-package key-chord
    :defer t
    :init
    (key-chord-mode 1)))

(defun aam/init-key-seq ()
  (use-package key-seq
    :defer t))

(defun aam/init-helm-bibtex()
  (use-package helm-bibtex
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "hc" 'helm-bibtex))
    :config
    (progn
      (setq helm-bibtex-pdf-symbol "⌘")
      (setq helm-bibtex-notes-symbol "✎")
      (setq helm-bibtex-additional-search-fields '(keywords tags))
      (setq helm-bibtex-bibliography "~/Dropbox/Research/Bibliography/references.bib")
      (setq helm-bibtex-library-path "~/Dropbox/Research/Bibliography/Papers/")
      (setq helm-bibtex-pdf-open-function
            (lambda (fpath)
              (start-process "xournal" "*helm-bibtex-xournal*" "/usr/bin/xournal" fpath)))
      (setq helm-bibtex-format-citation-functions
            '((org-mode      . helm-bibtex-format-citation-org-link-to-PDF)
              (latex-mode    . helm-bibtex-format-citation-cite)
              (markdown-mode . helm-bibtex-format-citation-pandoc-citeproc)
              (default       . helm-bibtex-format-citation-default)))
      )))

(defun aam/init-djvu()
  (use-package djvu
    :defer t))

(defun aam/init-twittering-mode ()
  (use-package twittering-mode
    :defer t
    :commands twit
    :init
    (spacemacs/set-leader-keys "at" 'twit)
    (when (configuration-layer/package-usedp 'flyspell)
      (add-hook 'twittering-edit-mode-hook (lambda () (flyspell-mode 1))))
    (push 'twittering-edit-mode evil-insert-state-modes)
    :config
    (setq twitter-images-directory
          (expand-file-name
           (concat spacemacs-cache-directory "twitter-images")))
    (unless (file-exists-p twitter-images-directory)
      (make-directory twitter-images-directory))
    (setq twittering-icon-mode t)
    (setq twittering-url-show-status nil)
    (setq twittering-use-master-password t)
    (setq twittering-use-icon-storage 1)))

(defun aam/init-hackernews()
  (use-package hackernews
    :defer t
    :init
    (spacemacs/set-leader-keys "ah" 'hackernews)))

(defun aam/init-sx()
  (use-package sx
    :defer t
    :config
    (setq sx-cache-directory (concat spacemacs-cache-directory "sx"))
    (when (not (file-directory-p sx-cache-directory))
      (make-directory sx-cache-directory))))

(defun aam/init-syslog-mode()
  (use-package syslog-mode
    :defer t))

(defun aam/init-evil-visual-mark-mode()
  (use-package evil-visual-mark-mode
    :defer t
    :config
    (spacemacs/set-leader-keys "tM" 'evil-visual-mark-mode)))

(defun aam/init-password-store()
  :defer t)

(defun aam/init-cloc()
  :defer t)


(defun aam/init-ztree()
  (use-package ztree
    :defer t
    :config
    (set-face-attribute 'ztreep-diff-model-add-face  nil :foreground "deep sky blue")
    (setq ztree-draw-unicode-lines t)
    (bind-keys :map ztreediff-mode-map
                   ("p" . previous-line)
                   ("k" . previous-line)
                   ("j" . next-line)
                   ("n" . next-line))

    (when (package-installed-p 'hydra)
        (bind-keys :map ztreediff-mode-map
                   ("\\" . hydra-ztree/body))
        (defhydra hydra-ztree (:color blue :hint nil)
            "
                                                                        ╭────────────┐
         Move      File                 Do                              │ Ztree diff │
      ╭─────────────────────────────────────────────────────────────────┴────────────╯
        _k_/_p_   [_C_] copy                  [_h_] toggle equal files
        ^ ^↑^ ^   [_D_] delete                [_x_] toggle subtree
        ^_TAB_^   [_v_] view                  [_r_] partial rescan
        ^ ^↓^ ^   [_d_] simple diff           [_R_] full rescan
        _j_/_n_   [_RET_] diff/expand         [_g_] refresh
        ^ ^ ^ ^   [_SPC_] simple diff/expand
      --------------------------------------------------------------------------------
            "
           ("\\" hydra-master/body "back")
           ("<ESC>" nil "quit")
           ("p" previous-line)
           ("k" previous-line)
           ("j" next-line)
           ("n" next-line)
           ("C" ztree-diff-copy)
           ("h" ztree-diff-toggle-show-equal-files)
           ("D" ztree-diff-delete-file)
           ("v" ztree-diff-view-file)
           ("d" ztree-diff-simple-diff-files)
           ("r" ztree-diff-partial-rescan)
           ("R" ztree-diff-full-rescan)
           ("RET" ztree-perform-action)
           ("SPC" ztree-perform-soft-action)
           ("TAB" ztree-jump-side)
           ("g" ztree-refresh-buffer)
           ("x" ztree-toggle-expand-subtree)))))
