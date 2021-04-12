;;; packages.el --- Org-roam Layer packages File for Spacemacs

(defconst org-roam-packages
  '(
    org
    org-roam
    org-roam-bibtex
    org-roam-server))

(defun org-roam/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    :after org-roam
    :diminish org-roam-bibtex-mode
    :hook
    (org-roam-mode . org-roam-bibtex-mode)
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "N" 'orb-note-actions)))

(defun org-roam/post-init-org-roam-server ()
  (add-hook 'org-mode-hook (lambda () (org-roam-server-mode))))

(defun org-roam/post-init-org ()
  (require 'org-protocol)
  (add-to-list 'org-modules 'org-protocol))

(defun org-roam/post-init-org-roam ()
  (require 'org-roam-protocol)
  (add-to-list 'org-modules 'org-roam-protocol))
