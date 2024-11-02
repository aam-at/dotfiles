;;; packages.el --- helm-legacy layer packages file for Spacemacs.
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

(defconst helm-legacy-packages
  '(helm
    helm-icons
    (helm-ls-git :toggle (configuration-layer/layer-used-p 'git))
    helm-posframe
    helm-projectile))

(defun helm-legacy/init-helm ()
  :defer (spacemacs/defer))

(defun helm-legacy/init-helm-icons ()
  (use-package helm-icons
    :defer t
    :init
    (helm-icons-enable)))

(defun helm-legacy/init-helm-ls-git ()
  (use-package helm-ls-git
    :defer t
    :init
    (spacemacs/set-leader-keys "gff" 'helm-ls-git)
    (when (configuration-layer/package-usedp 'magit)
      ;; Do not use helm-ls-git-rebase-todo-mode for git-rebase-todo,
      ;; instead let it be handled by magit
      (setq auto-mode-alist
            (delete '("/git-rebase-todo$" . helm-ls-git-rebase-todo-mode)
                    auto-mode-alist)))
    :config
    (when (configuration-layer/package-usedp 'magit)
      ;; Undo the forced action of adding helm-ls-git-rebase-todo-mode to
      ;; auto-mode-alist by helm-ls-git.
      (setq auto-mode-alist
            (delete '("/git-rebase-todo$" . helm-ls-git-rebase-todo-mode)
                    auto-mode-alist))
      ;; Set `helm-ls-git-status-command' conditonally on `git' layer
      ;; If `git' is in use, use default `\'magit-status-setup-buffer'
      ;; Otherwise, use defaault `\'vc-dir'
      (setq helm-ls-git-status-command 'magit-status-setup-buffer))))

(defun helm-legacy/init-helm-posframe ()
  (use-package helm-posframe
    :defer t
    :init
    (setq helm-posframe-poshandler 'posframe-poshandler-frame-center)
    (setq helm-posframe-width (round (* 0.618 (frame-width))))
    (setq helm-posframe-height (round (* 0.618 (frame-height))))
    (setq helm-posframe-parameters
          '((internal-border-width . 2)
            (left-fringe . 4)
            (right-fringe . 4)
            (undecorated . nil)))
    (helm-posframe-enable)))

(defun helm-legacy/init-helm-projectile ())
