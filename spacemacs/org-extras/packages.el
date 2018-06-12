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
    helm-org-rifle
    org-dashboard
    org-doing
    magit-org-todos
    (emacs-calfw :location (recipe
                            :fetcher github
                            :repo "kiwanami/emacs-calfw"))
    org-noter
    pdf-tools
    org-trello
    ;; provides synchronization with google calendar.
    (org-gcal :location (recipe
                         :fetcher github
                         :repo "myuhe/org-gcal.el"))
    (ebib :location (recipe
                     :fetcher github
                     :repo "joostkremers/ebib"))
    org-ref))


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
(defun org-extras/init-helm-org-rifle ()
  :defer t
  :init
  (spacemacs/declare-prefix "aor" "rifle")
  (spacemacs/set-leader-keys
    "aorr" 'helm-org-rifle
    "aorR" 'helm-org-rifle-occur
    "aorc" 'helm-org-rifle-current-buffer
    "aorC" 'helm-org-rifle-occur-current-buffer
    "aord" 'helm-org-rifle-directories
    "aorD" 'helm-org-rifle-occur-directories
    "aorf" 'helm-org-rifle-files
    "aorF" 'helm-org-rifle-occur-files
    "aora" 'helm-org-rifle-agenda-files
    "aorA" 'helm-org-rifle-occur-agenda-files)
  (spacemacs/declare-prefix-for-mode 'org-mode "aor" "rifle")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "rr" 'helm-org-rifle
    "rR" 'helm-org-rifle-occur
    "rc" 'helm-org-rifle-current-buffer
    "rC" 'helm-org-rifle-occur-current-buffer
    "rd" 'helm-org-rifle-directories
    "rD" 'helm-org-rifle-occur-directories
    "rf" 'helm-org-rifle-files
    "rF" 'helm-org-rifle-occur-files
    "ra" 'helm-org-rifle-agenda-files
    "rA" 'helm-org-rifle-occur-agenda-files))

(defun org-extras/init-org-dashboard ()
  :defer t
  :init
  (spacemacs/set-leader-keys "aoD" 'org-dashboard-display)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "D" 'org-dashboard-display))

(defun org-extras/init-org-doing ()
  :defer t
  :init
  (progn
    (spacemacs/set-leader-keys "aod" 'org-doing)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "Cd" 'org-doing)))

(defun org-extras/init-magit-org-todos()
  :defer t
  :config
  (magit-org-todos-autoinsert))

(defun org-extras/init-emacs-calfw ()
  :init
  (progn
    (require 'calfw-ical)
    (require 'calfw-org)
    (spacemacs/set-leader-keys "aoC" 'cfw:open-org-calendar))
  :config
  (evil-set-initial-state 'cfw:calendar-mode 'emacs))

(defun org-extras/init-org-noter ()
  :defer t
  :commands (org-noter)
  :init
  (spacemacs/set-leader-keys "aon" 'org-noter))

(defun org-extras/post-init-pdf-tools ()
  (spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode "N" 'org-noter))

(defun org-extras/init-org-trello ()
  (use-package org-trello
    :defer t
    :mode (("\\.trello\\'" . org-mode))
    :config
    (setq org-trello--config-dir (concat spacemacs-cache-directory "trello")
          org-trello--config-file (concat org-trello--config-dir "/%s.el"))
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
      "oth" 'org-trello-help-describing-bindings)))

(defun org-extras/init-org-gcal ()
  :defer t
  :init
  (progn
    (require 'org-gcal)
    (spacemacs/declare-prefix "aog" "gcal")
    (spacemacs/set-leader-keys
      "aogs" 'org-gcal-sync
      "aogf" 'org-gcal-fetch
      "aogp" 'org-gcal-post-at-point
      "aogr" 'org-gcal-refresh-token)
    (spacemacs/declare-prefix-for-mode 'org-mode "mg" "gcal")
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "gs" 'org-gcal-sync
      "gf" 'org-gcal-fetch
      "gp" 'org-gcal-post-at-point
      "gr" 'org-gcal-refresh-token))
  :config
  (setq org-gcal-dir (concat spacemacs-cache-directory "org-gcal")))

(defun org-extras/init-ebib ()
  :defer t)

(defun org-extras/post-init-org ()
  ;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
  (add-hook 'org-mode-hook
            (lambda ()
              (let ((filename (buffer-file-name (current-buffer))))
                (when (and filename
                           (string= "trello"
                                    (file-name-extension filename)))
                  (org-trello-mode)))))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images
            'append))

(defun org-extras/post-init-org-ref ()
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "iL" 'org-ref-helm-insert-label-link
    "ir" 'org-ref-helm-insert-ref-link)
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
