;;; packages.el --- Org-roam Layer packages File for Spacemacs

(defconst org-roam-packages
  '(
    org-roam
    org-roam-bibtex
    org-roam-server
    company-org-roam))

(defun org-roam/init-org-roam ()
  (use-package org-roam
    :after org
    :hook
    (org-mode . org-roam-mode)
    :init
    (progn
      (spacemacs/declare-prefix "aoR" "org-roam")
      (spacemacs/set-leader-keys
        "aoRl" 'org-roam
        "aoRb" 'org-roam-switch-to-buffer
        "aoRf" 'org-roam-find-file
        "aoRF" 'org-roam-find-file-immediate
        "aoRi" 'org-roam-insert
        "aoRI" 'org-roam-insert-immediate
        "aoRR" 'org-roam-random-note
        "aoRg" 'org-roam-graph)
      (spacemacs/declare-prefix-for-mode 'org-mode "aoR" "org-roam")

      (defun aam/org-roam-init-keys (mode)
        (progn
          (spacemacs/declare-prefix-for-mode mode "mR" "org-roam")
          (spacemacs/set-leader-keys-for-major-mode mode
            "Rl" 'org-roam
            "Rb" 'org-roam-switch-to-buffer
            "Rf" 'org-roam-find-file
            "RF" 'org-roam-find-file-immediate
            "Ri" 'org-roam-insert
            "RI" 'org-roam-insert-immediate
            "RR" 'org-roam-random-note
            "Rg" 'org-roam-graph)))

      (aam/org-roam-init-keys 'org-mode)
      (aam/org-roam-init-keys 'org-journal-mode))))


(defun org-roam/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    :after org-roam
    :diminish org-roam-bibtex-mode
    :hook
    (org-roam-mode . org-roam-bibtex-mode)
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "N" 'orb-note-actions)))

(defun org-roam/init-org-roam-server ()
  (use-package org-roam-server
    :after org-roam
    :hook
    (org-roam-mode . org-roam-server-mode)
    :ensure t))

(defun org-roam/init-company-org-roam ()
  (use-package company-org-roam
    :after org-roam
    :init
    (progn
      (spacemacs|add-company-backends :backends 'company-org-roam :modes org-mode))))

(defun org-roam/post-init-org ()
  (require 'org-protocol)
  (add-to-list 'org-modules 'org-protocol))

(defun org-roam/post-init-org-roam ()
  (require 'org-roam-protocol)
  (add-to-list 'org-modules 'org-roam-protocol))
