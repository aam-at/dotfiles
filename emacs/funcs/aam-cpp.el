;;; aam-cpp.el --- Shared C/C++ helpers -*- lexical-binding: t; -*-

(defun aam/cpp-format-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (clang-format-region (region-beginning) (region-end))
      (clang-format-buffer))
    (whitespace-cleanup)))

(provide 'aam-cpp)
;;; aam-cpp.el ends here
