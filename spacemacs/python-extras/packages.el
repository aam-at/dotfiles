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
        py-autopep8
        (ropemacs :location local)
        ))

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
(defun python-extras/init-py-autopep8()
  :defer t)

(defun python-extras/init-ropemacs()
  ;; Configure ropemacs
  ;; ropemacs rope pymacs needs to be installed manually
  :defer t
  :init
  (defvar is-ropemacs-loaded nil "True if ropemacs already loaded")
  (defun load-ropemacs()
    "Load pymacs and ropemacs"
    (interactive)
    (unless is-ropemacs-loaded
      (setq is-ropemacs-loaded t)
      (require 'pymacs)
      (pymacs-load "ropemacs" "rope-")))
  (add-hook 'python-mode-hook 'load-ropemacs)
  (evil-leader/set-key-for-mode 'python-mode
    ;; Rope project shortcuts
    "mpo" 'rope-open-project
    "mpk" 'rope-close-project
    "mpf" 'rope-find-file
    "mp4f" 'rope-find-file-other-window
    "mpu" 'rope-undo
    "mpr" 'rope-redo
    "mpc" 'rope-project-config
    "mpnm" 'rope-create-module
    "mpnp" 'rope-create-package
    "mpnf" 'rope-create-file
    "mpnd" 'rope-create-directory
    ;; Refactorings
    "mrr" 'rope-rename
    "mrl" 'rope-extract-variable
    "mrm" 'rope-extract-method
    "mrv" 'rope-move
    "mrx" 'rope-restructure
    "mru" 'rope-use-function
    "mrf" 'rope-introduce-factory
    "mrs" 'rope-change-signature
    "mr1r" 'rope-rename-current-module
    "mr1v" 'rope-move-current-module
    "mr1p" 'rope-module-to-package
    "mro" 'rope-organize-imports
    ;; Generate (variable|function|class|module|package)
    "mrnv" 'rope-generate-variable
    "mrnf" 'rope-generate-function
    "mrnc" 'rope-generate-class
    "mrnm" 'rope-generate-module
    "mrnp" 'rope-generate-package
    ;; Assist
    "mra/" 'rope-code-assist
    "mrag" 'rope-goto-definition
    "mrad" 'rope-show-doc
    "mraf" 'rope-find-occurrences
    "mrai" 'rope-find-implementations
    "mra?" 'rope-lucky-assist
    "mraj" 'rope-jump-to-global
    "mrac" 'rope-show-calltip
    "mraa" 'rope-analyze-module)
  :config
  (setq ropemacs-confirm-saving 'nil)
  (setq ropemacs-enable-autoimport 't))
