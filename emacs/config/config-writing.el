;; -*- lexical-binding: t; -*-
;; Shared Write or Die and jinx settings for Doom and Spacemacs.

;;;###autoload
(defun aam/writing-setup ()
  "Configure Write or Die and jinx consistently across Emacs profiles."
  (setq write-or-die-gamification t
        write-or-die-launch-words 25
        write-or-die-chain-words 25
        write-or-die-recovery-words 20
        write-or-die-activity-kind 'insertion
        write-or-die-auto-stop-at-time nil
        write-or-die-consequence 'rescue
        write-or-die-stop-policy 'confirm
        write-or-die-draft-discipline 'free
        write-or-die-visual-style 'auto
        write-or-die-symbol-style 'auto
        write-or-die-sound-backend 'auto)
  ;; Windows Emacs derives LANG from the locale (en-SG -> "ENE"), which jinx
  ;; would take as a dictionary name.
  (setq jinx-languages "en_US")
  ;; Org uses a stdin-only vale checker: the package's own one passes the file
  ;; path and also pipes the buffer, so vale exits early and Emacs hits a closed pipe.
  (with-eval-after-load 'flycheck-vale
    (setq flycheck-vale-modes '(text-mode markdown-mode rst-mode latex-mode LaTeX-mode))
    (flycheck-vale-setup)
    (dolist (mode flycheck-vale-modes)
      (flycheck-add-mode 'vale mode))
    (flycheck-define-checker vale-stdin
			     "Vale prose linter reading the buffer from stdin."
			     :command ("vale" "--output=line" "--ext" ".org" "--no-wrap")
			     :standard-input t
			     :error-patterns
			     ((info line-start "stdin.org:" line ":" column ":" (id (+ (not ":"))) ":" (message) line-end))
			     :modes org-mode)
    (add-to-list 'flycheck-checkers 'vale-stdin)))

(provide 'config-writing)
;;; config-writing.el ends here
