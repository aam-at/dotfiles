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
    visual-marks
    ;; for viewsing log files
    syslog-mode))

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
     habitrpg-api-url "https://habitica.com/api/v2"
     habitrpg-api-user "caa7c046-2e41-4233-acba-1880eb789c8a"
     habitrpg-api-token "api-token")))

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
      (evil-leader/set-key
        "hc" 'helm-bibtex))
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

(defun aam/init-twittering-mode()
  (use-package twittering-mode
    :defer t))

(defun aam/init-hackernews()
  (use-package hackernews
    :defer t
    :init
    (progn
      (evil-leader/set-key
        "ah" 'hackernews))))

(defun aam/init-sx()
  (use-package sx
    :defer t))

(defun aam/init-syslog-mode()
  (use-package syslog-mode
    :defer t))

(defun aam/init-evil-visual-mark-mode ()
  :defer t
  :config
  (spacemacs/set-leader-keys "tM" 'evil-visual-mark-mode))
