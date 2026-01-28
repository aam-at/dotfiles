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

(defun org-roam-extras/post-init-org-roam-ui ()
  (with-eval-after-load 'org
    (org-roam-ui-mode)))

(defun org-roam-extras/post-init-org ()
  (require 'org-protocol)
  (add-to-list 'org-modules 'org-protocol))

(defun org-roam-extras/post-init-org-roam ()
  (require 'org-roam-protocol)
  (add-to-list 'org-modules 'org-roam-protocol)
  (spacemacs/set-leader-keys
    "aorT" 'org-roam-extras/org-toggle-properties)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "rT" 'org-roam-extras/org-toggle-properties))

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
