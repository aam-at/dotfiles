;;; packages.el --- latex-extras layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Alexander Matyasko <amatyasko@amatyasko-steng>
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
;; added to `latex-extras-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `latex-extras/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `latex-extras/pre-init-PACKAGE' and/or
;;   `latex-extras/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst latex-extras-packages
  '((lsp-latex :requires lsp-mode)))



(defun latex-extras/init-lsp-latex()
  (use-package lsp-latex
    :defer t
    :init
    (require 'lsp-latex)
    (add-hook 'tex-mode-hook 'lsp)
    (add-hook 'LaTeX-mode-hook 'lsp)
    (add-hook 'bibtex-mode-hook 'lsp)))
;;; packages.el ends here
