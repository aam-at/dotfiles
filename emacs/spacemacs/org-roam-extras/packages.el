;;; packages.el --- org-roam-extras layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Alexander Matyasko <amatyasko@amatyasko-PC>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defconst org-roam-extras-packages
  '(org
    org-roam
    org-roam-bibtex
    org-roam-ui
    vulpea
    websocket))

(defun org-roam-extras/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    :after org-roam
    :hook (after-init . org-roam-bibtex-mode)
    :diminish org-roam-bibtex-mode
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "N" 'orb-note-actions)))

(defconst aam/org-roam-ui-http-port 35902
  "HTTP port for this profile's Org-roam UI instance.")

(defconst aam/org-roam-ui-websocket-port 35904
  "WebSocket port for this profile's Org-roam UI instance.")

(defvar aam/org-roam-ui--source-build-dir nil
  "Unmodified Org-roam UI build directory supplied by the package.")

(defun aam/org-roam-ui--prepare-app-build ()
  "Copy Org-roam UI's frontend and point it at this profile's WebSocket port."
  (let* ((source (or aam/org-roam-ui--source-build-dir
                     (setq aam/org-roam-ui--source-build-dir
                           org-roam-ui-app-build-dir)))
         (target (expand-file-name "org-roam-ui-spacemacs/"
                                   spacemacs-cache-directory)))
    ;; The upstream frontend hard-codes ws://localhost:35903.  A private copy
    ;; lets Spacemacs use the next HTTP/WebSocket pair without changing Doom's
    ;; UI or files managed by the package manager.
    (when (or (not (file-directory-p target))
              (file-newer-than-file-p source target))
      (when (file-exists-p target)
        (delete-directory target t))
      (make-directory target t)
      (copy-directory source target t t t)
      (let ((replacements 0))
        (dolist (file (directory-files-recursively target "\\.js\\'"))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (search-forward "ws://localhost:35903" nil t)
              (replace-match
               (format "ws://localhost:%d" aam/org-roam-ui-websocket-port)
               t t)
              (setq replacements (1+ replacements)))
            (write-region (point-min) (point-max) file nil 'silent)))
        (unless (> replacements 0)
          (error "Could not set Org-roam UI's WebSocket port in %s" target))))
    (setq org-roam-ui-app-build-dir target)))

(defun aam/org-roam-ui--redirect-websocket-port (original port &rest args)
  "Use this profile's WebSocket port for Org-roam UI's hard-coded default."
  (apply original
         (if (= port 35903) aam/org-roam-ui-websocket-port port)
         args))

(defun aam/org-roam-ui-enable ()
  "Start Org-roam UI on ports that do not collide with the Doom profile."
  (aam/org-roam-ui--prepare-app-build)
  (setq org-roam-ui-port aam/org-roam-ui-http-port)
  ;; Current Org-roam UI hard-codes 35903 for its server, so redirect it while
  ;; retaining the package's normal mode lifecycle and restart behavior.
  (unless (advice-member-p #'aam/org-roam-ui--redirect-websocket-port
                           'websocket-server)
    (advice-add 'websocket-server :around
                #'aam/org-roam-ui--redirect-websocket-port))
  (org-roam-ui-mode 1))

(defun org-roam-extras/post-init-org-roam-ui ()
  (with-eval-after-load 'org
    (aam/org-roam-ui-enable)))

(defun org-roam-extras/post-init-org ()
  (require 'org-protocol)
  (add-to-list 'org-modules 'org-protocol))

(defun org-roam-extras/post-init-org-roam ()
  (require 'org-roam-protocol)
  (add-to-list 'org-modules 'org-roam-protocol)
  (spacemacs/set-leader-keys
   "aorT" 'aam/org-roam-toggle-properties)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
					    "rT" 'aam/org-roam-toggle-properties))

(defun org-roam-extras/init-vulpea()
  (use-package vulpea
    :after org-roam
    :init
    (spacemacs/set-leader-keys
     "aorf" 'vulpea-find
     "aorF" 'org-roam-node-find
     "aori" 'vulpea-insert
     "aorI" 'org-roam-node-insert
     "aorb" 'vulpea-find-backlink)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
					      "rf" 'vulpea-find
					      "rF" 'org-roam-node-find
					      "ri" 'vulpea-insert
					      "rI" 'org-roam-node-insert
					      "rb" 'vulpea-find-backlink)))

(defun org-roam-extras/init-websocket ()
  (use-package websocket
    :after org-roam))
