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

(defun org-extras/org-update-attach-properties ()
  "Change properties for Org-Attach."
  (interactive)
  (org-with-point-at 1
    (while (outline-next-heading)
      (let ((DIR (org--property-local-values "ATTACH_DIR" nil)))
        (when DIR
          (org-set-property "DIR" (car DIR))
          (org-delete-property "ATTACH_DIR"))))
    (org-delete-property-globally "ATTACH_DIR_INHERIT")))

(defun org-extras/org-get-active-headline-files (file)
  (let ((value)
        (dir (file-name-directory file)))
    (dolist (element
             (with-current-buffer (find-file-noselect file)
               (let ((parsetree (org-element-parse-buffer 'element)))
                 (org-element-map parsetree 'headline
                   (lambda (hl)
                     (let (
                           (title (org-element-property :title hl))
                           (level (org-element-property :level hl))
                           (type (org-element-property :type hl))
                           (parent (org-element-property :parent hl)))
                       (and (eq level 2)
                            (let ((archived (org-element-property :archivedp parent)))
                              (not archived)) title))))))
             value)
      (setq value (cons
                   (concat
                    (file-name-as-directory dir)
                    (replace-regexp-in-string "\\[\\[.*:\\(.*\\\)\]\\[\\(.*\\)\\]\\]" "\\1" element))
                   value)))))

;; https://github.com/munen/emacs.d#convenience-functions-when-working-with-pdf-exports
(defun update-other-buffer ()
  (interactive)
  (other-window 1)
  (revert-buffer nil t)
  (other-window -1))

(defun org-compile-beamer-and-update-other-buffer ()
  "Has as a premise that it's run from an org-mode buffer and the
   other buffer already has the PDF open"
  (interactive)
  (org-beamer-export-to-pdf)
  (update-other-buffer))

(defun org-compile-latex-and-update-other-buffer ()
  "Has as a premise that it's run from an org-mode buffer and the
   other buffer already has the PDF open"
  (interactive)
  (org-latex-export-to-pdf)
  (update-other-buffer))

(defun org-id-remove-entry ()
  "Remove/delete the ID entry and update the databases.
Update the `org-id-locations' global hash-table, and update the
`org-id-locations-file'.  `org-id-track-globally' must be `t`."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (when (org-entry-delete (point) "ID")
      (org-id-update-id-locations nil 'silent))))

(defun org-extras/org-title-to-slug (title)
  "Convert TITLE to a filename-suitable slug."
  (cl-flet* ((nonspacing-mark-p (char)
                                (eq 'Mn (get-char-code-property char 'general-category)))
             (strip-nonspacing-marks (s)
                                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                                 (ucs-normalize-NFD-string s))))
             (cl-replace (title pair)
                         (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-")  ;; convert anything not alphanumeric
                    ("__*" . "_")  ;; remove sequential underscores
                    ("^_" . "")  ;; remove starting underscore
                    ("_$" . "")))  ;; remove ending underscore
           (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
      (downcase slug))))

(defun org-extras/org-get-filepath-for-title (&optional path template)
  "Return the filepath to the note with the specified title"
  (interactive)
  (or path (setq path (read-directory-name "Path: " org-directory)))
  (or template (setq template "%s.org"))
  (setq org-extras/org-capture-title (read-string "Title: "))
  (let ((org-extras/org-capture--slug
         (read-string "Slug: " (org-extras/org-title-to-slug org-extras/org-capture-title))))
    (expand-file-name
     (if (functionp template)
         (funcall template org-extras/org-capture--slug)
       (format template org-extras/org-capture--slug))
     path)))

(defun org-extras/org-get-datetime-filepath (slug)
  "Return the filepath with datetime prefix"
  (let ((datetime (format-time-string "%Y%m%d-%H%M")))
    (s-lex-format "${datetime}_${slug}.org")))

(defun org-extras/org-id-in-file (file)
  "Check if the file contains :ID: property"
  (let ((file-contents (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string))))
    (s-contains-p ":ID:" file-contents)))

(defun org-extras/org-id-list-files (&optional directory)
  "List the .org files with :ID: in DIRECTORY and in its sub-directories."
  (let (org-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    (while current-directory-list
      (cond
       ((and (equal ".org" (substring (car (car current-directory-list)) -4))
             (equal (org-extras/org-id-in-file (car (car current-directory-list))) t))
        (setq org-files-list
              (cons (car (car current-directory-list)) org-files-list)))
       ((eq t (car (cdr (car current-directory-list))))
        (if
            (equal "."
                   (substring (car (car current-directory-list)) -1))
            ()
          (setq org-files-list
                (append
                 (org-extras/org-all-files
                  (car (car current-directory-list)))
                 org-files-list)))))

      (setq current-directory-list (cdr current-directory-list)))
    org-files-list))

(defun org-extras/org-id-update-all ()
  "Update id locations for all files in org-directory"
  (interactive)
  (org-id-update-id-locations (org-extras/org-id-list-files org-directory)))

(defun org-extras/org-sort-by-year ()
  (string-to-number (org-entry-get nil "YEAR")))

(defun org-extras/org-sort-papers ()
  (interactive)
  (org-sort-entries nil ?F #'org-extras/org-sort-by-year))

(defun org-extras/org-sort-by-impact-factor ()
  (string-to-number (org-entry-get nil "IMPACT_FACTOR")))

(defun org-extras/org-sort-journals ()
  (interactive)
  (org-sort-entries nil ?F #'org-extras/org-sort-by-impact-factor))
