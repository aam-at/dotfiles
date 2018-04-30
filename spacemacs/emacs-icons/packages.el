;;; packages.el --- emacs-icons layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Alexander Matyasko <alexander.matyasko@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `emacs-icons-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `emacs-icons/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `emacs-icons/pre-init-PACKAGE' and/or
;;   `emacs-icons/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst emacs-icons-packages
  '(
    all-the-icons
    all-the-icons-dired
    all-the-icons-ivy
    neotree))


;;; packages.el ends here

(defun emacs-icons/init-all-the-icons ()
  :defer t
  :init
  (require 'font-lock+))

(defun emacs-icons/init-all-the-icons-dired ()
  :defer t
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  :config
  (spacemacs|diminish all-the-icons-dired-mode))

(defun emacs-icons/init-all-the-icons-ivy ()
  :defer t
  :config
  (all-the-icons-ivy-setup))

(defun emacs-icons/post-init-neotree()
  (setq neo-theme 'icons))
