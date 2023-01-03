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
