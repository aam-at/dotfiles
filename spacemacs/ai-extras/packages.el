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
    llm
    (magit-gptcommit :location (recipe
                                :fetcher github
                                :repo "avishefi/magit-gptcommit"
                                :branch "llm"))))


(defun ai-extras/init-llm ())

(defun ai-extras/init-magit-gptcommit ()
  (use-package magit-gptcommit
    :demand t
    :after magit llm
    :config
    (if ai-extras-autostart-gptcommit-mode
        (progn
          (magit-gptcommit-mode -1)
          (magit-gptcommit-status-buffer-setup)))))
