;;; packages.el --- habitrpg layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Alexander Matyasko <alexander.matyasko@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;; ;; This file is not part of GNU Emacs.
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
;; added to `habitrpg-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `habitrpg/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `habitrpg/pre-init-PACKAGE' and/or
;;   `habitrpg/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst habitrpg-packages
  '(
    (habitrpg :location (recipe
                         :fetcher github
                         :repo "ryjm/habitrpg.el"))
    org
    )
  "The list of Lisp packages required by the habitrpg layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(defun habitrpg/init-habitrpg()
  (use-package habitrpg
    :defer t
    :commands (habitrpg-add)
    :init
    (spacemacs/set-leader-keys "oh" 'habitrpg-status)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "oha" 'habitrpg-add)
    :config
    (evil-set-initial-state 'habitrpg-status-mode 'emacs)
    (define-key habitrpg-status-mode-map
      (kbd dotspacemacs-leader-key) spacemacs-default-map)
    (evilified-state-evilify habitrpg-status-mode habitrpg-status-mode-map
      "j" 'habitrpg-goto-next-section
      "k" 'habitrpg-goto-previous-section
      "J" 'habitrpg-goto-next-sibling-section
      "K" 'habitrpg-goto-previous-sibling-section
      (kbd "C-h") 'habitrpg-goto-parent-section)
    (spacemacs/set-leader-keys-for-major-mode 'habitrpg-status-mode
      "i" 'habitrpg-clock-in-status
      "u" 'habitrpg-upvote-at-point
      "d" 'habitrpg-downvote-at-point
      "t" 'habitrpg-key-mode-popup-manage
      "g" 'habitrpg-refresh
      "G" 'habitrpg-refresh-all)
    (evil-set-initial-state 'habitrpg-key-mode 'emacs)
  ))

(defun habitrpg/post-init-org()
  (defun my-habitrpg-add()
    (interactive)
    (when (or (string= org-state "DONE") (string= org-state "TODO"))
      (habitrpg-add)))

  (add-hook 'org-after-todo-state-change-hook 'my-habitrpg-add 'append)
  (add-hook 'org-clock-in-hook 'habitrpg-clock-in)
  (add-hook 'org-clock-out-hook 'habitrpg-clock-out))
;;; packages.el ends here
