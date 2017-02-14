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
(defconst org-extras-packages
    '(
      org-dashboard org-journal
      org-doing
      org-trello
      ;; provides synchronization with google calendar.
      (org-gcal :location (recipe
                             :fetcher github
                             :repo "myuhe/org-gcal.el"))
      (emacs-calfw :location (recipe
                              :fetcher github
                              :repo "kiwanami/emacs-calfw"))
      (ebib :location (recipe
                       :fetcher github
                       :repo "joostkremers/ebib"))
      ob-ipython
      org-ref
    ))

;; List of packages to exclude.
(defconst org-extras-excluded-packages '())

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
  :defer t
  :init
  (spacemacs/set-leader-keys "oD" 'org-dashboard-display))

(defun org-extras/init-org-journal()
  :defer t
  :init
  (spacemacs/set-leader-keys "oj" 'org-journal-new-entry)
  (spacemacs/set-leader-keys-for-major-mode 'org-journal-mode
    "j" 'org-journal-new-entry
    "n" 'org-journal-open-next-entry
    "p" 'org-journal-open-previous-entry))

(defun org-extras/init-org-doing()
  :defer t
  :init
  (progn
    (spacemacs/set-leader-keys "od" 'org-doing)))

(defun org-extras/init-org-trello ()
  :defer t
  :init
  ;; org-trello major mode for all .trello files
  (add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))
  :config
  (spacemacs/set-leader-keys-for-minor-mode 'org-trello-mode
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
    "oth" 'org-trello-help-describing-bindings))

(defun org-extras/init-org-gcal()
  :init
  (require 'org-gcal)
  (spacemacs/set-leader-keys "aCs" 'org-gcal-sync)
  (spacemacs/set-leader-keys "aCf" 'org-gcal-fetch)
  (spacemacs/set-leader-keys "aCp" 'org-gcal-post-at-point)
  (spacemacs/set-leader-keys "aCr" 'org-gcal-refresh-token))

(defun org-extras/init-emacs-calfw()
  :init
  (progn
    (require 'calfw-ical)
    (require 'calfw-org)
    (spacemacs/set-leader-keys "aCo" 'cfw:open-org-calendar))
  :config
  (evil-set-initial-state 'cfw:calendar-mode 'emacs))

(defun org-extras/init-ebib() :defer t)

(defun org-extras/init-ob-ipython ()
  :defer t
  :init
  (org-babel-do-load-languages 'org-babel-load-languages '((ipython . t))))

(defun org-extras/post-init-org()
    ;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
    (add-hook 'org-mode-hook
              (lambda ()
                (let ((filename (buffer-file-name (current-buffer))))
                  (when (and filename (string= "trello" (file-name-extension filename)))
                    (org-trello-mode)))))
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  )

(defun org-extras/post-init-org-ref()
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
    "or" 'org-ref-hydra/body)
  ;; optional but very useful libraries from org-ref
  (require 'doi-utils)
  (require 'org-ref-pdf)
  (require 'org-ref-url-utils)
  (require 'org-ref-bibtex)
  (require 'org-ref-arxiv)
  (require 'org-ref-pubmed)
  (require 'org-ref-isbn)
  (require 'org-ref-wos)
  (require 'org-ref-scopus)
  (require 'x2bib)
  (require 'org-ref-scifinder)
  (require 'org-ref-worldcat))
