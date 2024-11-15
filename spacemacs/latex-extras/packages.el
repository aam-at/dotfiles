;;; packages.el --- latex-extras layer packages file for Spacemacs.
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
(defconst latex-extras-packages
  '(cdlatex
    (texpresso :location (recipe
                          :fetcher github
                          :repo "let-def/texpresso"
                          :files ("emacs/*.el")))))


(defun latex-extras/init-cdlatex()
  (use-package cdlatex
    :defer t
    :commands cdlatex-mode
    :diminish cdlatex-mode
    :init
    (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
    (add-hook 'latex-mode-hook 'turn-on-cdlatex)))

(defun latex-extras/init-texpresso()
  :defer t
  :init
  (require 'texpresso)
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode
    "t" 'texpresso))
