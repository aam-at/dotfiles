;;; packages.el --- org-extras Layer packages File for Spacemacs
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
(setq org-extras-packages
    '(
      org-dashboard org-journal
      org-download
      org-doing
      org-trello
      ;; provides synchronization with google calendar.
      (org-caldav :location (recipe
                             :fetcher github
                             :repo "dengste/org-caldav"))

      ;; ebib and key-chord dependecy for orgref
      (hydra :location (recipe
                        :fetcher github
                        :repo "abo-abo/hydra"))
      (ebib :location (recipe
                       :fetcher github
                       :repo "joostkremers/ebib"))
      ;; org mode reference management
      (org-ref :location (recipe
                          :fetcher github
                          :repo "jkitchin/org-ref"))
      ob-ipython
      org-eww
    ))

;; List of packages to exclude.
(setq org-extras-excluded-packages '())

;; For each package, define a function org-extras/init-<package-name>
;;
;; (defun org-extras/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
(defun org-extras/init-org-dashboard()
  (use-package org-dashboard
    :defer t
    :init
    (spacemacs/set-leader-keys "oD" 'org-dashboard-display)))

(defun org-extras/init-org-journal()
  (use-package org-journal
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "oj" 'org-journal-new-entry)
      (spacemacs/set-leader-keys-for-major-mode 'org-journal-mode
        "j" 'org-journal-new-entry
        "n" 'org-journal-open-next-entry
        "p" 'org-journal-open-previous-entry))))

(defun org-extras/init-org-download()
  (use-package org-download))

(defun org-extras/init-org-doing()
  (use-package org-doing
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "od" 'org-doing))))

(defun org-extras/init-org-trello ()
  :defer t
  :init
  ;; org-trello major mode for all .trello files
  (add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))

  ;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
  (add-hook 'org-mode-hook
            (lambda ()
              (let ((filename (buffer-file-name (current-buffer))))
                (when (and filename (string= "trello" (file-name-extension filename)))
                  (org-trello-mode)))))

  :config
  (progn
    (spacemacs/set-leader-keys
      "otv" 'org-trello-version
      "oti" 'org-trello-install-key-and-token
      "otI" 'org-trello-install-board-metadata
      "otc" 'org-trello-sync-card
      "ots" 'org-trello-sync-buffer
      "ota" 'org-trello-assign-me
      "otd" 'org-trello-check-setup
      "otD" 'org-trello-delete-setup
      "otb" 'org-trello-create-board-and-install-metadata
      "otk" 'org-trello-kill-entity
      "otK" 'org-trello-kill-cards
      "ota" 'org-trello-archive-card
      "otA" 'org-trello-archive-cards
      "otj" 'org-trello-jump-to-trello-card
      "otJ" 'org-trello-jump-to-trello-board
      "otC" 'org-trello-add-card-comments
      "otc" 'org-trello-show-card-comments
      "otl" 'org-trello-show-card-labels
      "otu" 'org-trello-update-board-metadata
      "oth" 'org-trello-help-describing-bindings)))

(defun org-extras/init-org-caldav()
  (use-package org-caldav
    :defer t
    :commands org-caldav-sync
    :init
    (spacemacs/set-leader-keys "oS" 'org-caldav-sync)))

(defun org-extras/init-hydra()
  (use-package hydra
    :defer t))

(defun org-extras/init-ebib()
  (use-package ebib
    :defer t))

(defun org-extras/init-org-ref()
  (use-package org-ref
    :defer t
    :init
    (require 'org)
    (require 'hydra)
    (setq hydra-is-helpful t)

    (defhydra org-ref-hydra ()
      "org-ref"
      ("c" org-ref-helm-insert-cite-link "cite")
      ("r" org-ref-helm-insert-ref-link "ref")
      ("l" org-ref-helm-insert-label-link "label")
      ("R" org-ref "org-ref"))
    (spacemacs/set-leader-keys
      "or" 'org-ref-hydra/body
      "oc" 'org-ref-cite-hydra/body
      "ob" 'org-ref-bibtex-hydra/body)
    (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode
      ;; Navigation
      "j" 'org-ref-bibtex-next-entry
      "k" 'org-ref-bibtex-previous-entry

      ;; Open
      "b" 'org-ref-open-in-browser
      "n" 'org-ref-open-bibtex-notes
      "p" 'org-ref-open-bibtex-pdf

      ;; Misc
      "h" 'org-ref-bibtex-hydra/body
      "i" 'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
      "s" 'org-ref-sort-bibtex-entry

      ;; Lookup utilities
      "lA" 'arxiv-add-bibtex-entry
      "la" 'arxiv-get-pdf-add-bibtex-entry
      "ld" 'doi-utils-add-bibtex-entry-from-doi
      "li" 'isbn-to-bibtex
      "lp" 'pubmed-insert-bibtex-from-pmid)
      ;; optional but very useful libraries from org-ref
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
      (require 'org-ref-wos)))

(defun org-extras/init-ob-ipython()
  (spacemacs|use-package-add-hook org
    (use-package ob-ipython)
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)))

(defun org-extras/init-org-eww()
  (use-package org-eww
    :defer t
    :config 
    (spacemacs/set-leader-keys
      "op" 'org-eww/turn-on-preview-at-save
      "oP" 'org-eww/turn-off-preview-at-save)))
