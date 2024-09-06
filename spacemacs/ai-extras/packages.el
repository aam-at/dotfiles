;;; packages.el --- ai-extras layer packages file for Spacemacs.
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

;;; Code:

(defconst ai-extras-packages
  '(
    (copilot :requires company
             :location (recipe
                        :fetcher github
                        :repo "zerolfx/copilot.el"
                        :files ("*.el" "dist")))
    elisa
    (khoj :location (recipe
                     :fetcher github
                     :repo "aam-at/khoj"
                     :files ("src/interface/emacs/*.el")))
    llm
    magit-gptcommit))

(defun ai-extras/init-copilot ()
  (use-package copilot
    :defer t
    :init
    ;; accept completion from copilot and fallback to company
    (with-eval-after-load 'company
      ;; disable inline previews
      (delq 'company-preview-if-just-one-frontend company-frontends))
    (with-eval-after-load 'copilot
      (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
      (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
      (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
      (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word))
    (add-hook 'prog-mode-hook 'copilot-mode)
    (spacemacs|diminish copilot-mode "" "C")))

(defun ai-extras/init-elisa ()
  (use-package elisa
    :demand t))

(defun ai-extras/init-khoj ()
  (use-package khoj
    :defer t
    :init
    (spacemacs/set-leader-keys
      "$k" 'khoj)))

(defun ai-extras/init-llm ())

(defun ai-extras/init-magit-gptcommit ()
  (use-package magit-gptcommit
    :defer t
    :after magit llm
    :init
    (if ai-extras-autostart-gptcommit-mode
        (progn
          (magit-gptcommit-mode -1)
          (magit-gptcommit-status-buffer-setup)))))
