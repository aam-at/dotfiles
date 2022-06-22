;;; packages.el --- Org-roam Layer packages File for Spacemacs

(defconst org-roam-packages
  '(
    org
    org-roam
    org-roam-bibtex
    org-roam-ui
    websocket))

(defun org-roam/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    :after org-roam
    :hook (after-init . org-roam-bibtex-mode)
    :diminish org-roam-bibtex-mode
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "N" 'orb-note-actions)))

(defun org-roam/post-init-org-roam-ui ()
  (with-eval-after-load 'org
    (org-roam-ui-mode)))

(defun org-roam/post-init-org ()
  (require 'org-protocol)
  (add-to-list 'org-modules 'org-protocol))

(defun org-roam/init-websocket ()
  (use-package websocket
    :after org-roam))

(defun org-roam/post-init-org-roam ()
  (require 'org-roam-protocol)
  (add-to-list 'org-modules 'org-roam-protocol)
  (spacemacs/set-leader-keys
    "aorT" 'org-roam/org-toggle-properties)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "rT" 'org-roam/org-toggle-properties))
