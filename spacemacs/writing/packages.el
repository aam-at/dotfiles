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
      cdlatex
      ;; general writing
      writeroom-mode
      writegood-mode
      flycheck-vale
      (write-or-die :location local)
      langtool
      ;; synonyms and thesaurus
      (mw-thesaurus :location (recipe)
                    :fetcher github
                    :repo "agzam/mw-thesaurus.el")
      synosaurus
      synonymous
      (words :location local)
      (textlint :location local)
      academic-phrases))

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
(defun writing/init-cdlatex()
  (use-package cdlatex
    :defer t
    :commands cdlatex-mode
    :diminish cdlatex-mode
    :init
    (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)))

(defun writing/post-init-writeroom-mode ()
  (setq writeroom-width 90)
  (spacemacs/set-leader-keys "xW" #'writeroom-mode))

(defun writing/init-writegood-mode ()
  "Initialize writegood-mode"
  :defer t
  :init
  (spacemacs/set-leader-keys "xG" #'writegood-mode))

(defun writing/init-flycheck-vale ()
  "Initialize flycheck-vale"
  :defer t
  :confi
  (setq flycheck-vale-modes '(text-mode markdown-mode rst-mode org-mode latex-mode))
  (flycheck-vale-setup))

(defun writing/init-write-or-die ()
  (use-package write-or-die
    :defer t
    :commands write-or-die-mode
    :init
    (add-hook 'text-mode-hook #'write-or-die-mode)
    (spacemacs/set-leader-keys "xD" #'write-or-die-mode)
    (spacemacs|add-toggle write-or-die
      :status (eq write-or-die-state 1)
      :on (write-or-die-go)
      :off (write-or-die-stop)
      :documentation "Activate `Write or Die!'"
      :evil-leader "C-t d")))

(defun writing/init-langtool ()
  :defer t
  :init
  (spacemacs/set-leader-keys
    "x4w" #'langtool-check
    "x4W" #'langtool-check-done
    "x4l" #'langtool-switch-default-language
    "x44" #'langtool-show-message-at-point
    "x4c" #'langtool-correct-buffer))

(defun writing/init-mw-thesaurus()
  :defer t
  :init
  (spacemacs/set-leader-keys
    "St" 'mw-thesaurus-lookup-dwim))

(defun writing/init-synosaurus()
  (use-package synosaurus
    :defer t
    :diminish synosaurus-mode
    :init
    (progn
      (add-hook 'markdown-mode-hook 'synosaurus-mode)
      (add-hook 'text-mode-hook 'synosaurus-mode)
      (spacemacs/set-leader-keys
        "Sl" 'synosaurus-lookup
        "Sr" 'synosaurus-choose-and-replace))
    :config
    (setq synosaurus-choose-method 'default)))

(defun writing/init-synonymous()
  :defer t
  :init
  (spacemacs/set-leader-keys
    "Ss" 'synonymous-synonyms
    "Sa" 'synonymous-antonyms))

(defun writing/init-words()
  (use-package words
    :defer t
    :commands (words words-hydra/body)
    :init
    (spacemacs/set-leader-keys
      "Sw" 'words-hydra/body)))

(defun writing/init-textlint()
  :config
  (require 'textlint)
  :init
  (spacemacs/set-leader-keys
    "S!" 'textlint-run))

(defun writing/init-academic-phrases()
  :defer t)
