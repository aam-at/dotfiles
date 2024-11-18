;;; packages.el --- cpp-extras layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
;; added to `cpp-extras-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `cpp-extras/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `cpp-extras/pre-init-PACKAGE' and/or
;;   `cpp-extras/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst cpp-extras-packages
  '(
    clang-format
    (doxymacs :location local)))

(defun cpp-extras/post-init-clang-format()
  (spacemacs/set-leader-keys-for-major-mode 'c-mode
    "=" 'c-c++/format-region-or-buffer)
  (spacemacs/set-leader-keys-for-major-mode 'c++-mode
    "=" 'c-c++/format-region-or-buffer))

(defun cpp-extras/init-doxymacs ()
  (use-package doxymacs
    :commands doxymacs-mode
    :init
    (add-hook 'c++-mode-hook #'doxymacs-mode)
    :config
    (spacemacs|diminish doxymacs-mode)))
