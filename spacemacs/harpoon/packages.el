;;; packages.el --- harpoon layer packages file for Spacemacs.
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

(defconst harpoon-packages
  '(harpoon))

(defun harpoon/init-harpoon()
  (use-package harpoon
    :init
    (spacemacs/declare-prefix "ah" "harpoon")
    (spacemacs/set-leader-keys
      "ah." 'harpoon-quick-menu-hydra
      "ahm" 'harpoon-toggle-quick-menu
      "aha" 'harpoon-add-file
      "ahd" 'harpoon-delete-item
      "M-1" 'harpoon-go-to-1
      "M-2" 'harpoon-go-to-2
      "M-3" 'harpoon-go-to-3
      "M-4" 'harpoon-go-to-4
      "M-5" 'harpoon-go-to-5
      "M-6" 'harpoon-go-to-6
      "M-7" 'harpoon-go-to-7
      "M-8" 'harpoon-go-to-8
      "M-9" 'harpoon-go-to-9)))
