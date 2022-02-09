;;; packages.el --- Org-roam Layer packages File for Spacemacs

(defconst org-roam-packages
  '(
    org
    org-roam
    org-roam-bibtex
    websocket
    (org-roam-ui :requires org-roam
                 :location (recipe :repo "org-roam/org-roam-ui"
                                   :fetcher github
                                   :files ("*.el" "out")))))

(defun org-roam/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    :after org-roam
    :hook (after-init . org-roam-bibtex-mode)
    :diminish org-roam-bibtex-mode
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "N" 'orb-note-actions)))

(defun org-roam/post-init-org-roam-server ()
  (add-hook 'org-mode-hook (lambda () (org-roam-server-mode))))

(defun org-roam/post-init-org ()
  (require 'org-protocol)
  (add-to-list 'org-modules 'org-protocol))

(defun org-roam/init-websocket ()
  (use-package websocket
    :after org-roam))

(defun org-roam/init-org-roam-ui ()
  (use-package org-roam-ui
    :after org-roam
    :hook (after-init . org-roam-ui-mode)
    :diminish org-roam-ui-mode
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)))

(defun org-roam/post-init-org-roam ()
  (require 'org-roam-protocol)
  (add-to-list 'org-modules 'org-roam-protocol)
  (spacemacs/set-leader-keys
    "aorT" 'org-roam/org-toggle-properties)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "rT" 'org-roam/org-toggle-properties))
