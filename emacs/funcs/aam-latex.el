;;; aam-latex.el --- Shared LaTeX helpers -*- lexical-binding: t; -*-

(defun aam/latex-cycle-text-wrapping ()
  "Cycle between different text wrapping options:
1. Default Emacs paragraph filling
2. One sentence per line
3. Original unformatted text"
  (interactive)
  (let* ((cycle-state (or (get-text-property (point) 'cycle-state) 0))
         (orig-text (or (get-text-property (point) 'orig-text)
                        (buffer-substring-no-properties (point-min) (point-max))))
         (start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max))))

    (cond
     ;; State 0: Apply default Emacs paragraph filling
     ((= cycle-state 0)
      (fill-region start end)
      (setq cycle-state 1))

     ;; State 1: One sentence per line
     ((= cycle-state 1)
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (let ((sentence-start (point)))
            (forward-sentence)
            (when (< (point) end)
              (let ((sentence-end (point)))
                (goto-char sentence-start)
                (delete-horizontal-space)
                (while (search-forward "\n" sentence-end t)
                  (replace-match " " nil t))
                (goto-char sentence-end)
                (delete-horizontal-space)
                (insert "\n"))))))
      (setq cycle-state 2))

     ;; State 2: Restore original unformatted text
     ((= cycle-state 2)
      (delete-region start end)
      (insert orig-text)
      (setq cycle-state 0)))

    ;; Store the cycle state and original text as text properties
    (put-text-property start end 'cycle-state cycle-state)
    (put-text-property start end 'orig-text orig-text)

    (message "Text wrapping cycled to state %d" cycle-state)))

(defun aam/latex-split-to-one-sentence-per-line ()
  "Split the selected region or current paragraph into one sentence per line."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((sentence-start (point)))
          (forward-sentence)
          (when (< (point) end)
            (let ((sentence-end (point)))
              (goto-char sentence-start)
              (delete-horizontal-space)
              (while (search-forward "\n" sentence-end t)
                (replace-match " " nil t))
              (goto-char sentence-end)
              (delete-horizontal-space)
              (insert "\n"))))))))


;; Adapted from an external LaTeX helper implementation.
(defun aam/latex-empty-or-whitespace-region-p (beg end)
  "Whether the region between BEG and END is empty or contains only whitespace."
  (string-match-p
   "^\\s-*$"
   (buffer-substring-no-properties beg end)))

(defun aam/latex--insert-on-own-line (text)
  "Insert TEXT at point on a line of its own; return that line's start."
  (unless (aam/latex-empty-or-whitespace-region-p (line-beginning-position) (point))
    (insert "\n"))
  (insert text)
  (prog1 (line-beginning-position)
    (unless (aam/latex-empty-or-whitespace-region-p (point) (line-end-position))
      (insert "\n"))))

(defun aam/latex-toggle-math ()
  "Create or toggle LaTeX math ($'s or \\[ and \\]).

If region is active, surround it by \\[ and \\] on their own lines.

If point is in math surrounded by $'s, surround it by \\[ and \\]
instead, on their own lines.

If point is in math surrounded by \\[ and \\], surround it by $'s.

If point is in math delimited otherwise, signal an error.

If point is not in math, insert a \\[ \\] pair on new lines around an
empty line and put point there."
  (interactive)
  (cond
   ((use-region-p)
    (let ((beg (region-beginning))
          (end (copy-marker (region-end))))
      (deactivate-mark)
      (save-excursion
        (goto-char end)
        (aam/latex--insert-on-own-line "\\]")
        (setq end (point-marker))
        (goto-char beg)
        (indent-region (aam/latex--insert-on-own-line "\\[") end))))
   ((texmathp)
    (let ((open (copy-marker (cdr texmathp-why))))
      (pcase (car texmathp-why)
        ("$"
         (save-excursion
           (goto-char (1+ open))
           (re-search-forward "\\(?:^\\|[^\\]\\)\\$")
           (delete-char -1)
           (aam/latex--insert-on-own-line "\\]")
           (let ((end (point-marker)))
             (goto-char open)
             (delete-char 1)
             (indent-region (aam/latex--insert-on-own-line "\\[") end))))
        ("\\["
         (save-excursion
           (goto-char (+ 2 open))
           (re-search-forward "\\(?:^\\|[^\\]\\)\\\\\\]")
           (delete-char -2)
           (insert "$")
           (goto-char open)
           (delete-char 2)
           (insert "$")))
        (other (user-error "Point in math mode but surrounded by %s" other)))))
   (t
    (unless (aam/latex-empty-or-whitespace-region-p
             (line-beginning-position) (line-end-position))
      (end-of-line)
      (insert "\n"))
    (insert "\\[")
    (LaTeX-indent-line)
    (insert "\n\n\\]")
    (LaTeX-indent-line)
    (forward-line -1)
    (LaTeX-indent-line))))

(provide 'aam-latex)
;;; aam-latex.el ends here
