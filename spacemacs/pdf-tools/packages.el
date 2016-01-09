;;; packages.el --- pdf-tools Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq pdf-tools-packages
    '(
      pdf-tools
      ))

;; List of packages to exclude.
(setq pdf-tools-excluded-packages '())

;; For each package, define a function pdf-tools/init-<package-name>
;;
;; (defun pdf-tools/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
(defun pdf-tools/init-pdf-tools()
  (use-package pdf-tools
    :defer t
    :init
    (evilified-state-evilify pdf-view-mode pdf-view-mode-map
             "/"  'isearch-forward
             "?"  'isearch-backward
             "gg" 'pdf-view-first-page
             "G"  'pdf-view-last-page
             "gt" 'pdf-view-goto-page
             "h"  'pdf-view-previous-page
             "j"  'pdf-view-next-line-or-next-page
             "k"  'pdf-view-previous-line-or-previous-page
             "K"  'kill-this-buffer
             "l"  'pdf-view-next-page
             ;; "n"  'isearch-repeat-forward
             ;; "N"  'isearch-repeat-backward
             (kbd "C-d") 'pdf-view-scroll-up-or-next-page
             ;; (kbd "C-k") 'doc-view-kill-proc
             (kbd "C-u") 'pdf-view-scroll-down-or-previous-page)
    :config
    (progn
      (defvar prefer-pdf-tools (fboundp 'pdf-view-mode))
      (defun start-pdf-tools-if-pdf ()
        (when (and prefer-pdf-tools
                   (eq doc-view-doc-type 'pdf))
          (pdf-view-mode)))

      (add-hook 'doc-view-mode-hook 'start-pdf-tools-if-pdf))))
