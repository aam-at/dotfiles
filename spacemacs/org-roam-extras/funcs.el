(defun org-roam-extras/org-hide-properties ()
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

(defun org-roam-extras/org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun org-roam-extras/org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (org-roam-extras/org-show-properties)
    (org-roam-extras/org-hide-properties)))

(defun org-roam-extras/org-orphan-nodes-by-id ()
  "Return a list of all orphan nodes in `org-roam`."
  (org-roam-db-query "SELECT
id, title
FROM nodes
WHERE id NOT IN (
                  SELECT DISTINCT n.id
                  FROM nodes n
                  LEFT OUTER JOIN links l ON n.id = l.source OR n.id = l.dest
                  WHERE l.type LIKE '%%id%%'
                  )"))

(defun org-roam-extras/org-insert-orphan-nodes ()
  "Insert all orphan nodes in `org-roam' in the current buffer."
  (interactive)
  (let* ((orphans (org-roam-extras/org-orphan-nodes-by-id)))
    (dolist (orphan orphans)
      (let ((id (car orphan))
            (title (cadr orphan)))
        (insert "* ")
        (insert (org-link-make-string
                 (concat "id:" id)
                 title))
        (insert "\n")))))
