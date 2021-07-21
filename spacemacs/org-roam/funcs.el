(defun org-roam/org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
    (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
      (overlay-put ov_this 'display "")
      (overlay-put ov_this 'hidden-prop-drawer t)))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun org-roam/org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun org-roam/org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (org-roam/org-show-properties)
    (org-roam/org-hide-properties)))
