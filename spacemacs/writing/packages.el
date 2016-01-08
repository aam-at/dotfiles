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
      ;; general writing
      writeroom-mode
      writegood-mode
      langtool
      ;; synonyms and thesaurus
      (synosaurus :location (recipe
                             :fetcher github
                             :repo "rootzlevel/synosaurus"))
      thesaurus synonymous
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
    :init (spacemacs/set-leader-keys "xW" #'writeroom-mode)))

(defun writing/init-writegood-mode ()
  "Initialize writegood-mode"

  (use-package writegood-mode
    :init (spacemacs/set-leader-keys "xG" #'writegood-mode)))

(defun writing/init-langtool ()
  "Initialize writeroom-mode"

  (use-package langtool
    :init
    (spacemacs/set-leader-keys
      "x4w" #'langtool-check
      "x4W" #'langtool-check-done
      "x4l" #'langtool-switch-default-language
      "x44" #'langtool-show-message-at-point
      "x4c" #'langtool-correct-buffer)))

(defun writing/init-synosaurus()
  (use-package synosaurus
    :defer t
    :init
    (progn
      (add-hook 'markdown-mode-hook 'synosaurus-mode)
      (add-hook 'text-mode-hook 'synosaurus-mode)
      (spacemacs/set-leader-keys
        "Sl" 'synosaurus-lookup
        "Sr" 'synosaurus-choose-and-replace))
    :config
    (progn
      (setq synosaurus-choose-method 'default))
    ))

(defun writing/init-thesaurus()
  (use-package thesaurus
    :defer t
    :init
    (progn
      (evil-leader/set-key
        "St" 'thesaurus-choose-synonym-and-replace))))

(defun writing/init-synonymous()
  (use-package synonymous
    :defer t))
