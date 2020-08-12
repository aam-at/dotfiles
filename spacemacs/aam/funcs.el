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
  (concat org-ref-pdf-directory (format "%s.pdf" key)))
