;; -*- lexical-binding: t; -*-
;; Shared Write or Die settings for Doom and Spacemacs.

;;;###autoload
(defun aam/writing-setup ()
  "Configure Write or Die consistently across Emacs profiles."
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
        write-or-die-sound-backend 'auto))

(provide 'config-writing)
;;; config-writing.el ends here
