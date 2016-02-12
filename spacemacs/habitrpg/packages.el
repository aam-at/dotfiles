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
    :init
    (setq habitrpg-api-url "https://habitica.com/api/v2")
    (spacemacs/set-leader-keys "oh" 'habitrpg-status)
    (spacemacs/set-leader-keys-for-major-mode 'habitrpg-status-mode
      "n" 'habitrpg-goto-next-section
      "p" 'habitrpg-goto-previous-section
      "a" 'habitrpg-do-backlog
      "I" 'habitrpg-clock-in
      "i" 'habitrpg-clock-in-status
      "O" 'habitrpg-clock-out
      "u" 'habitrpg-upvote-at-point
      "d" 'habitrpg-downvote-at-point
      "t" 'habitrpg-key-mode-popup-manage
      "g" 'habitrpg-refresh
      "G" 'habitrpg-refresh-all
      "q" 'habitrpg-quit-window)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "oha" 'habitrpg-add)
    :config
    ;; (define-key habitrpg-mode-map (kbd "gr") 'habitrpg-refresh)
    ;; (define-key habitrpg-mode-map (kbd "gR") 'habitrpg-refresh-all))
  ))
;;; packages.el ends here
