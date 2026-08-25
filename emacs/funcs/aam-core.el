(defun aam-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun aam-open-pdf-external (key)
  (start-process "xournal" "*helm-bibtex-xournal*" "/usr/bin/xournal"
                 (expand-file-name (aam-get-cite-pdf-filename (car key)))))

(defun aam-get-cite-pdf-filename (key)
  (let ((pdf-files (-filter #'file-exists-p
                            (-map (lambda (pdf-path) (concat pdf-path (format "%s.pdf" key)))
                                  bibtex-completion-library-path))))
    (when (> (length pdf-files) 1)
      (warn (format "Multiple files detected for key %s" key)))
    (car pdf-files)))

(defun aam-reopen-file-as-real ()
  "Reopen the current file if it is a symbolic link."
  (interactive)
  (let ((file-name (buffer-file-name))
        (real-file-name (file-truename (buffer-file-name))))
    (when (and file-name (not (string= file-name real-file-name)))
      (find-alternate-file real-file-name)
      (message "Reopened '%s' as '%s'." file-name real-file-name))))

(defun aam-sort-selected-words (beg end)
  "Sort words in the selected region alphabetically, ignoring case and treating hyphens as single units."
  (interactive "r")
  (save-excursion
    (let* ((region-text (buffer-substring-no-properties beg end))
           (words (split-string region-text "\\s-+" t))
           (sorted-words (seq-sort-by #'downcase #'string-lessp words)))
      (delete-region beg end)
      (insert (string-join sorted-words " ")))))

(defun aam--extract-pdf-text-to-buffer (pdf-file)
  "Extract text from PDF-FILE and return a buffer with the content."
  (let ((temp-buffer (generate-new-buffer "*PDF Text*"))
        (coding-system-for-read 'utf-8))
    (condition-case err
        (progn
          (call-process "pdftotext" nil temp-buffer nil "-layout" "-nopgbrk" pdf-file "-")
          (with-current-buffer temp-buffer
            (set-buffer-modified-p nil)
            (goto-char (point-min)))
          temp-buffer)
      (error
       (kill-buffer temp-buffer)
       (error "Failed to extract PDF text: %s" (error-message-string err))))))

(defun aam--ensure-pdf-file (file)
  "Ensure FILE is a valid PDF file."
  (unless (and file (file-exists-p file) (string-match-p "\\.pdf$" file))
    (error "Invalid or non-existent PDF file: %s" file))
  file)

(defun aam-extract-pdf-text-from-file (file)
  "Extract text from a PDF file and display it in a temporary buffer."
  (interactive "fPDF file: ")
  (let ((pdf-file (aam--ensure-pdf-file file)))
    (switch-to-buffer (aam--extract-pdf-text-to-buffer pdf-file))))

(defun aam-extract-pdf-text-from-current-buffer ()
  "Extract text from the PDF file in the current pdf-tools buffer."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (error "This function only works in pdf-tools buffers. Current mode: %s" major-mode))
  (let ((pdf-file (buffer-file-name)))
    (unless pdf-file
      (error "No file associated with this buffer"))
    (switch-to-buffer (aam--extract-pdf-text-to-buffer (aam--ensure-pdf-file pdf-file)))))

(defun aam-delete-empty-lines ()
  "Delete empty lines in current buffer"
  (interactive)
  (flush-lines "^[[:space:]]*$"))

(defun aam-delete-xml-tags (&optional start-pos end-pos)
  "Delete all XML-style tags and their content.
Matches any XML tags like <tag>...</tag>.
Works on whole buffer or the selected region if START-POS and END-POS are provided."
  (interactive)
  (let ((count 0)
        (start (or start-pos (point-min)))
        (end (or end-pos (point-max)))
        (xml-start-regex "<\\([^/> ]+\\)\\(?:\\s-+[^>]*\\)?>"))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
                  (re-search-forward xml-start-regex end t))
        (let* ((tag-name (match-string 1))
               (begin (match-beginning 0))
               (end-tag (concat "</" tag-name ">"))
               (success (search-forward end-tag end t)))
          (if success
              (progn
                (delete-region begin (point))
                (when (looking-at "^\n")
                  (delete-char 1))
                (setq count (1+ count)))
            (message "Warning: Unmatched tag <%s> at position %d" tag-name begin)
            (goto-char (1+ begin))))))
    (when (> count 0)
      (message "Deleted %d XML tag region%s" count (if (= count 1) "" "s")))
    count))


(defun aam-cuda-available-p ()
  "Check if CUDA is available on the system."
  (zerop (call-process "nvidia-smi" nil nil nil)))

(defun aam-gpu-memory-gb ()
  "Get the amount of GPU memory in GB."
  (when (aam-cuda-available-p)
    (with-temp-buffer
      (call-process "nvidia-smi" nil t nil "--query-gpu=memory.total" "--format=csv,noheader,nounits")
      (/ (string-to-number (buffer-string)) 1024))))


(defun aam-check-localhost-port (port)
  "Check if localhost port is accepting connections. Returns t if port is open, nil otherwise."
  (condition-case nil
      (let ((proc (open-network-stream
                   "port-test"
                   nil
                   "localhost"
                   port)))
        (when proc
          (delete-process proc)
          t))
    (error nil)))

(provide 'aam-core)
(defvar aam/org-root (file-truename (expand-file-name "~/Dropbox/Org/"))
  "Root directory of the Org knowledge base.")

(defun aam/org-path (path)
  "Return PATH relative to `aam/org-root`."
  (expand-file-name path aam/org-root))

(defvar aam/bib-root (expand-file-name "~/Google Drive/Research/Bibliography/")
  "Root directory of the bibliography library.")

(defun aam/bib-path (path)
  "Return PATH relative to `aam/bib-root`."
  (expand-file-name path aam/bib-root))

(defvar aam/bibtex-files
  (list (aam/bib-path "refs.bib")
        (aam/bib-path "review.bib")
        (aam/bib-path "books.bib")
        (aam/bib-path "myrefs.bib"))
  "Bibliography files shared by citation packages.")

(defun aam/set-bibliography-root (root)
  "Set ROOT and refresh `aam/bibtex-files` beneath it."
  (setq aam/bib-root (file-truename (expand-file-name root))
        aam/bibtex-files
        (mapcar #'aam/bib-path '("refs.bib" "review.bib" "books.bib" "myrefs.bib"))))

(defvar org-projectile-file "TODOs.org"
  "Org file used for project TODO collection.")

(defun aam/configure-org-paths ()
  "Apply shared Org paths before Org-dependent packages initialize."
  (setq org-directory aam/org-root
        org-roam-directory aam/org-root
        org-contacts-files (list (aam/org-path "contacts.org"))))
(defconst aam/minimum-emacs-version "31.1"
  "Minimum Emacs version supported by this configuration.")

(defun aam/require-supported-emacs ()
  "Signal an error unless the running Emacs meets the supported baseline."
  (when (version< emacs-version aam/minimum-emacs-version)
    (error "This configuration requires Emacs %s or newer; running %s"
           aam/minimum-emacs-version emacs-version)))
