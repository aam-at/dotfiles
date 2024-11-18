(defun c-c++/format-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (clang-format-region (region-beginning) (region-end))
          (message "Formatted selected region."))
      (progn
        (clang-format-buffer)
        (message "Formatted buffer.")))
    (whitespace-cleanup)))
