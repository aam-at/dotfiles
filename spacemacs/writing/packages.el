;;; packages.el --- writing Layer packages File for Spacemacs
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
(setq writing-packages
    '(
      writeroom-mode
      writegood-mode
      langtool
      ;; package names go here
      ))

;; List of packages to exclude.
(setq writing-excluded-packages '())

;; For each package, define a function writing/init-<package-name>
;;
;; (defun writing/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
(defun writing/init-writeroom-mode ()
  "Initialize writeroom-mode"

  (use-package writeroom-mode
    :init (spacemacs/set-leader-keys "xr" #'writeroom-mode)))

(defun writing/init-writegood-mode ()
  "Initialize writegood-mode"

  (use-package writegood-mode
    :init (spacemacs/set-leader-keys "xg" #'writegood-mode)))

(defun writing/init-langtool ()
  "Initialize writeroom-mode"

  (use-package langtool
    :init
    (spacemacs/set-leader-keys "x4w" #'langtool-check)
    (spacemacs/set-leader-keys "x4W" #'langtool-check-done)
    (spacemacs/set-leader-keys "x4l" #'langtool-switch-default-language)
    (spacemacs/set-leader-keys "x44" #'langtool-show-message-at-point)
    (spacemacs/set-leader-keys "x4c" #'langtool-correct-buffer)))
