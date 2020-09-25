;;; packages.el --- python-extras Layer packages File for Spacemacs
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
(defconst python-extras-packages
  '(
    sphinx-doc))

;; List of packages to exclude.
(defconst python-extras-excluded-packages '())

;; For each package, define a function python-extras/init-<package-name>
;;
;; (defun python-extras/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun python-extras/init-sphinx-doc ()
  (use-package sphinx-doc
    :diminish sphinx-doc-mode
    :defer t
    :init
    (add-hook 'python-mode-hook (lambda ()
                                  (require 'sphinx-doc)
                                  (sphinx-doc-mode t)))))
