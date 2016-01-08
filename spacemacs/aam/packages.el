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
    ;; Additional package for org-mode
    org-dashboard org-journal
    ;; provides synchronization with google calendar.
    (org-caldav :location (recipe
                            :fetcher github
                            :repo "dengste/org-caldav"))
    ;; integration for orgmode and habitrpg.
    (habitrpg :location (recipe
                            :fetcher github
                            :repo "ryjm/habitrpg.el"))
    (hydra :location (recipe
                     :fetcher github
                     :repo "abo-abo/hydra"))
    ;; ebib and key-chord dependecy for orgref
    (ebib :location (recipe
                     :fetcher github
                     :repo "joostkremers/ebib"))
    key-chord key-seq
    ;; org mode reference management
    (org-ref :location (recipe
                         :fetcher github
                         :repo "jkitchin/org-ref"))
    helm-bibtex
    ;; Replace default doc-view and provide djvu support
    pdf-tools djvu
    ;; Twitter hackernews stackexchange
    twittering-mode hackernews sx
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
(defun aam/init-org-dashboard()
  (use-package org-dashboard
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "mD" 'org-dashboard-display))))

(defun aam/init-org-journal()
  (use-package org-journal
    :defer t
    :init
    (progn
      (setq org-journal-dir "~/Dropbox/Notes/journal")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "mj" 'org-journal-new-entry)
      (spacemacs/set-leader-keys-for-major-mode 'org-journal-mode
        "mj" 'org-journal-new-entry
        "mn" 'org-journal-open-next-entry
        "mp" 'org-journal-open-previous-entry))))

(defun aam/init-org-caldav()
  (use-package org-caldav
    :defer t
    :init
    (setq
    org-caldav-calendars '(:calendar-id "iu5alt927aue6hsjis25qhsark@group.calendar.google.com"
                            :files "~/Dropbox/Notes/work.org"
                            :inbox "~/Dropbox/Notes/fromwork.org"))))

(defun aam/init-habitrpg()
  (use-package habitrpg
    :defer t
    :init
    (setq
     habitrpg-api-url "https://habitica.com/api/v2"
     habitrpg-api-user "caa7c046-2e41-4233-acba-1880eb789c8a"
     habitrpg-api-token "api-token")))

(defun aam/init-hydra()
  (use-package hydra
    :defer t))

(defun aam/init-ebib()
  (use-package ebib
    :defer t))

(defun aam/init-key-chord()
  (use-package key-chord
    :defer t
    :init
    (key-chord-mode 1)))

(defun aam/init-key-seq ()
  (use-package key-seq
    :defer t))

(defun aam/init-org-ref()
  (use-package org-ref
    :defer t
    :init
    (progn
      (require 'org)
      (require 'hydra)
      (setq hydra-is-helpful t)

      (require 'key-chord)
      (key-chord-define-global
       "zz"
       (defhydra org-ref-hydra ()
         "org-ref"
         ("c" org-ref-helm-insert-cite-link "cite")
         ("r" org-ref-helm-insert-ref-link "ref")
         ("l" org-ref-helm-insert-label-link "label")
         ("R" org-ref "org-ref")))
      ;; optional but very useful libraries in org-ref
      (require 'org-ref-isbn)
      (require 'org-ref-pdf)
      (require 'org-ref-url-utils)
      (require 'doi-utils)
      (require 'org-ref-worldcat)
      (require 'org-ref-scifinder)
      (require 'org-ref-pubmed)
      (require 'org-ref-arxiv)
      (require 'org-ref-sci-id)
      (require 'org-ref-bibtex)
      (require 'org-ref-scopus)
      (require 'org-ref-wos))
    :config
    ;; Org-ref configuration
    (setq reftex-default-bibliography '("~/Dropbox/Research/Bibliography/references.bib"))
    (setq org-ref-bibliography-notes "~/Dropbox/Notes/papers.org"
          org-ref-default-bibliography '("~/Dropbox/Research/Bibliography/references.bib")
          org-ref-pdf-directory "~/Dropbox/Research/Bibliography/Papers")))

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

(defun aam/init-pdf-tools()
  (use-package pdf-tools
    :defer t
    :init
    (evilified-state-evilify pdf-view-mode pdf-view-mode-map
             "/"  'isearch-forward
             "?"  'isearch-backward
             "gg" 'pdf-view-first-page
             "G"  'pdf-view-last-page
             "gt" 'pdf-view-goto-page
             "h"  'pdf-view-previous-page
             "j"  'pdf-view-next-line-or-next-page
             "k"  'pdf-view-previous-line-or-previous-page
             "K"  'kill-this-buffer
             "l"  'pdf-view-next-page
             ;; "n"  'isearch-repeat-forward
             ;; "N"  'isearch-repeat-backward
             (kbd "C-d") 'pdf-view-scroll-up-or-next-page
             ;; (kbd "C-k") 'doc-view-kill-proc
             (kbd "C-u") 'pdf-view-scroll-down-or-previous-page)
    :config
    (progn
      (defvar prefer-pdf-tools (fboundp 'pdf-view-mode))
      (defun start-pdf-tools-if-pdf ()
        (when (and prefer-pdf-tools
                   (eq doc-view-doc-type 'pdf))
          (pdf-view-mode)))

      (add-hook 'doc-view-mode-hook 'start-pdf-tools-if-pdf))))

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
