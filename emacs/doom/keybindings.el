;;; keybindings.el --- Doom-specific keybinding support -*- lexical-binding: t; -*-

;; Spacemacs leader paths are preserved below `SPC z` instead of replacing
;; Doom's existing leader groups.  Each character becomes one Doom key level:
;; e.g. Spacemacs's "gff" becomes `SPC z g f f`.  `$` is represented by `4`,
;; because `$` is not retained as a Doom leader-map key.
(defun aam/doom-legacy-leader-key (key)
  "Translate a compact Spacemacs leader KEY into a Doom key sequence."
  (cond
   ((string= key "$") "4")
   ((string-match-p "\\`[ACHMsS]-" key) key)
   (t (mapconcat (lambda (character)
                   (if (eq character ?$) "4" (char-to-string character)))
                 key " "))))

(defun aam/doom-legacy-leader-bindings (bindings &optional prefix)
  "Flatten compact Spacemacs BINDINGS below optional PREFIX.
`(:prefix (KEY . NAME) ...)` groups are converted to nested Doom key paths."
  (let (result)
    (while bindings
      (let ((entry (pop bindings)))
        (if (and (listp entry) (eq (car entry) :prefix))
            (let ((spec (cadr entry)))
              (unless (and (consp spec) (stringp (car spec)))
                (error "Expected a prefix key/name pair, got: %S" spec))
              (setq result
                    (append result
                            (aam/doom-legacy-leader-bindings
                             (cddr entry)
                             (append prefix (list (car spec)))))))
          (unless (stringp entry)
            (error "Expected a leader key string, got: %S" entry))
          (unless bindings
            (error "Missing command for leader key: %S" entry))
          (let ((command (pop bindings)))
            (setq result
                  (append result
                          (list (mapconcat #'identity
                                           (mapcar #'aam/doom-legacy-leader-key
                                                   (append prefix (list entry)))
                                           " ")
                                command)))))))
    result))

(map! :leader :prefix ("z" . "AAM"))

(defmacro aam/ported-leader! (&rest bindings)
  "Bind compact Spacemacs leader BINDINGS below Doom's `SPC z` prefix."
  `(map! :leader :prefix "z"
	 ,@(aam/doom-legacy-leader-bindings bindings)))

(defun aam/doom-split-window-below-and-focus ()
  "Split the selected window below and select the new window."
  (interactive)
  (select-window (split-window-below)))

(defun aam/doom-split-window-right-and-focus ()
  "Split the selected window right and select the new window."
  (interactive)
  (select-window (split-window-right)))

(provide 'aam-doom-keybindings)
;;; keybindings.el ends here
