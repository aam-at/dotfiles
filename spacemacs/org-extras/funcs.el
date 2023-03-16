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
  "Get all active projects from the index file."
  (let ((value))
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
                    (car
                     (org-id-find
                      (replace-regexp-in-string "\\[\\[.*:\\(.*\\\)\]\\[\\(.*\\)\\]\\]" "\\1" element)))
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
                                                                 (string-glyph-compose s))))
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
                 (org-extras/org-id-list-files
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

(defun org-extras/org-convert-org-id-link-to-file-link ()
  "Replace org-id link with corresponding file link. Useful with
  org-roam and org-transclude."
  (interactive)
  (when (org-in-regexp "\\[\\[\\(.+\\):\\(.+\\)\\]\\[\\(.+\\)\\]\\]" 1)
    (let ((link (org-link-unescape (match-string-no-properties 0)))
          (type (match-string-no-properties 1))
          (target (match-string-no-properties 2))
          (text (match-string-no-properties 3)))
      (when (string-equal type "id")
        (goto-char (match-beginning 1))
        (delete-region (match-beginning 1) (match-end 2))
        (let* ((path (car (org-id-find target)))
               ;; Remove any prefix from the path that appears before Dropbox,
               ;; as the Dropbox folder is always located in user home dir.
               copy-               (index (string-match-p "Dropbox" path))
               (path (substring path index)))
          (insert (s-lex-format "file:~/${path}")))))))

(defun org-extras/org-remark-notes-file ()
  (interactive)
  (let* ((path (buffer-file-name))
         (dir (file-name-directory path))
         (file (file-name-nondirectory path))
         (name (file-name-base file))
         (ext (file-name-extension file)))
    (if (string-equal ext "org")
        path
      (concat dir "marginalia.org"))))

(defun org-extras/org-remark-highlight-save (buffer beg end props &optional title)
  (let* ((filename (org-remark-source-get-file-name buffer))
         (ext (file-name-extension buffer))
         (id (plist-get props 'org-remark-id))
         (text (org-with-wide-buffer (buffer-substring-no-properties beg end)))
         (notes-buf (find-file-noselect (org-remark-notes-get-file-name)))
         (main-buf (current-buffer))
         (line-num (org-current-line beg))
         (orgid (org-remark-highlight-get-org-id beg)))
    (with-current-buffer notes-buf
      (when (featurep 'org-remark-convert-legacy) (org-remark-convert-legacy-data))
      ;;`org-with-wide-buffer is a macro that should work for non-Org file'
      (org-with-wide-buffer
       (let ((file-headline (or (org-find-property
                                 org-remark-prop-source-file filename)
                                (progn
                                  ;; If file-headline does not exist, create one at the bottom
                                  (goto-char (point-max))
                                  ;; Ensure to be in the beginning of line to add a new headline
                                  (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
                                  (insert (if (string-equal ext "org")
                                              "* COMMENT Highlights"
                                            (concat "* " title "\n")))
                                  (org-set-property org-remark-prop-source-file filename)
                                  (org-up-heading-safe) (point))))
             (id-headline (org-find-property org-remark-prop-id id)))
         ;; Add org-remark-link with updated line-num as a property
         (plist-put props "org-remark-link" (concat
                                             "[[file:"
                                             filename
                                             (when line-num (format "::%d" line-num))
                                             "]]"))
         (if id-headline
             (progn
               (goto-char id-headline)
               ;; Update the existing headline and position properties
               ;; Don't update the headline text when it already exists
               ;; Let the user decide how to manage the headlines
               ;; (org-edit-headline text)
               ;; FIXME update the line-num in a normal link if any
               (org-remark-notes-set-properties beg end props))
           ;; No headline with the marginal notes ID property. Create a new one
           ;; at the end of the file's entry
           (goto-char file-headline)
           (org-narrow-to-subtree)
           (goto-char (point-max))
           ;; Ensure to be in the beginning of line to add a new headline
           (when (eolp) (open-line 1) (forward-line 1) (beginning-of-line))
           ;; Create a headline
           ;; Add a properties
           (insert (concat "** " text "\n"))
           (org-remark-notes-set-properties beg end props)
           (when (and orgid org-remark-use-org-id)
               (insert (concat "[[id:" orgid "]" "[" title "]]"))))))
      (cond
       ;; fix GH issue #19
       ;; Temporarily remove `org-remark-save' from the `after-save-hook'
       ;; When the marginal notes buffer is the source buffer
       ((eq notes-buf main-buf)
        (remove-hook 'after-save-hook #'org-remark-save t)
        (save-buffer)
        (add-hook 'after-save-hook #'org-remark-save nil t))
       ;; When marginal notes buffer is separate from the source buffer, save the
       ;; notes buffer
       ((buffer-modified-p)
        (save-buffer)))
      t)))
