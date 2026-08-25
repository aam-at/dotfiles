;;; config-ui.el --- Shared visual preferences -*- lexical-binding: t; -*-

;; This file deliberately contains only Emacs values.  Doom and Spacemacs
;; adapt them to their respective font and theme settings.

(defvar aam/theme 'doom-one
  "Theme selected by default in every Emacs distribution.")

(defvar aam/theme-variants '(doom-one doom-one-light)
  "Themes offered by distributions that support cycling themes.")

(defvar aam/monospace-font-family "JetBrains Mono"
  "Font family for fixed-pitch text.")

(defvar aam/proportional-font-family "iA Writer Mono S"
  "Font family for prose and variable-pitch text.")

(defun aam/font-size (&optional frame)
  "Return the preferred font size for FRAME.
High-density displays use a larger font while terminals and ordinary displays
use the regular size."
  (if (and (display-graphic-p frame)
           (> (display-pixel-width frame) 3000))
      20
    14))

(defun aam/default-font (&optional frame)
  "Return the default font specification list for FRAME."
  `(,aam/monospace-font-family
    :size ,(aam/font-size frame)
    :weight normal
    :width normal))

(provide 'config-ui)
;;; config-ui.el ends here
