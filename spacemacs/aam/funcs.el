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
  "Sort words in the selected region alphabetically."
  (interactive "r")
  (let ((words (split-string (buffer-substring-no-properties beg end))))
    (delete-region beg end)
    (insert (mapconcat 'identity (sort words 'string<) " "))))

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


(defun cuda-available-p ()
  "Check if CUDA is available on the system."
  (zerop (call-process "nvidia-smi" nil nil nil)))

(defun gpu-memory-gb ()
  "Get the amount of GPU memory in GB."
  (when (cuda-available-p)
    (with-temp-buffer
      (call-process "nvidia-smi" nil t nil "--query-gpu=memory.total" "--format=csv,noheader,nounits")
      (/ (string-to-number (buffer-string)) 1024))))
