(defun org-extras/days-before-today (n)
  (let ((time-in-question (decode-time)))
    ;; time-in-question is the current time, decoded into convenient fields

    ;; decrease the field by one which represents the day -- make it "yesterday"
    (decf (nth 3 time-in-question) n)

    ;; now, re-encode that time
    (setq time-in-question (apply 'encode-time time-in-question))))

(defun org-extras/org-journal-file-for-date-before (n)
  "Return journal entry path for date n-days before"
  (require 'org-journal)
  (org-journal--get-entry-path (org-extras/days-before-today n)))

(defun org-extras/org-journal-list-agenda-files (n)
  "Add journal entries for the past n days"
  (if n
      (seq-filter
       'file-exists-p
       (mapcar 'org-extras/org-journal-file-for-date-before (number-sequence 1 n)))
    (eval org-journal-dir)))

(defun org-extras/org-narrow-to-subtree ()
  (interactive)
  (let ((org-indirect-buffer-display 'current-window))
    (if (not (boundp 'org-indirect-buffer-file-name))
        (let ((above-buffer (current-buffer))
              (org-filename (buffer-file-name)))
          (org-tree-to-indirect-buffer (1+ (org-current-level)))
          (setq-local org-indirect-buffer-file-name org-filename)
          (setq-local org-indirect-above-buffer above-buffer))
      (let ((above-buffer (current-buffer))
            (org-filename org-indirect-buffer-file-name))
        (org-tree-to-indirect-buffer (1+ (org-current-level)))
        (setq-local org-indirect-buffer-file-name org-filename)
        (setq-local org-indirect-above-buffer above-buffer)))))

(defun org-extras/org-widen-from-subtree ()
  (interactive)
  (let ((above-buffer org-indirect-above-buffer)
        (org-indirect-buffer-display 'current-window))
    (kill-buffer)
    (switch-to-buffer above-buffer)))

(defun org-extras/org-pdfview-open (link)
  "Open LINK in pdf-view-mode."
  (cond ((string-match "\\(.*\\)::\\([0-9]*\\)\\+\\+\\([[0-9]\\.*[0-9]*\\)"  link)
         (let* ((path (match-string 1 link))
                (page (string-to-number (match-string 2 link)))
                (height (string-to-number (match-string 3 link))))
           (org-open-file path 1)
           (pdf-view-goto-page page)
           (image-set-window-vscroll
            (round (/ (* height (cdr (pdf-view-image-size))) (frame-char-height))))))
        ((string-match "\\(.*\\)::\\([0-9]+\\)$"  link)
         (let* ((path (match-string 1 link))
                (page (string-to-number (match-string 2 link))))
           (org-open-file path 1)
           (pdf-view-goto-page page)))
        (t
         (org-open-file link 1))))
