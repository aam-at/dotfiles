(defun latex-extras/cycle-text-wrapping ()
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

(defun latex-extras/split-to-one-sentence-per-line ()
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


;; based on https://github.com/rgemulla/spacemacs-layers/blob/master/%2Blang/latexp/funcs.el
(defun latex-extras/empty-or-whitespace-region-p (beg end)
  "Whether the region between BEG and END is empty or contains only whitespace."
  (string-match-p
   "^\\s-*$"
   (buffer-substring-no-properties beg end)))

(defun latex-extras/LaTeX-toggle-math ()
  "Create or toggle LaTeX math ($'s or \\[ and \\]).

  If region is active, surrounds it by \\[ and \\]. Also inserts
  appropriate newlines.

  If point is in LaTeX math mode and surrounded by $'s, surrounds
  it by \\[ and \\] instead. Also inserts appropriate newlines.

  If point is in LaTeX math mode and surrounded by \\[ and
  \\], surrounds it by $'s instead.

  If point is in LaTeX math mode and neither surrounded by $'s or
  \\[ and \\], does nothing and reports an error.

  If point is not in LaTeX math mode and on an empty line,
  inserts \\[ and \\] and puts the point in between. Also inserts
  appropriate newlines.

  If point is not in LaTeX math mode and not on an empty
  line, inserts a new line below the current line and proceeds as
  above.
  "
  (interactive)
  (cond
   ((use-region-p) ;; surround region with \[ \]
    (error "LaTeX-math on region not yet implemented"))
   ((texmathp)
    (cond
     ((string-equal (car texmathp-why) "$") ;; change $$ to \[\]
      (save-excursion
        (let* ((beg (cdr texmathp-why))
               (end (search-forward-regexp "^\\$\\|[^\\]\\$")))
          ;; update end marker
          (goto-char end)
          (backward-char 1)
          (unless (latex-extras/empty-or-whitespace-region-p (line-beginning-position) (point))
            (insert "\n"))
          (delete-char 1)
          (insert "\\]") ;; closing $
          (unless (latex-extras/empty-or-whitespace-region-p (point) (line-end-position))
            (insert "\n"))
          (setq end (+ 1 (line-end-position)))

          ;; update start marker
          (goto-char beg)
          (unless (latex-extras/empty-or-whitespace-region-p (line-beginning-position) beg)
            (insert "\n"))
          (delete-char 1) ;; opening $
          (insert "\\[")
          (setq beq (line-beginning-position))
          (unless (latex-extras/empty-or-whitespace-region-p (point) (line-end-position))
            (insert "\n"))
          (indent-region beg end))))
     ((string-equal (car texmathp-why) "\\[") ;; change \[\] to $$
      (save-excursion
        (let* ((beg (cdr texmathp-why))
               (end (search-forward-regexp "^\\\\\\]\\|[^\\]\\\\\\]")))
          (goto-char end)
          (delete-char -2)
          (insert "$")
          (goto-char beg)
          (delete-char 2)
          (insert "$"))))

     (t ;; nothing to do
      (error "Point in math mode but surrounded by %s" (car texmathp-why)))))
   (t ;; insert new \[ \] pair
    (progn
      (unless (latex-extras/empty-or-whitespace-region-p
               (line-beginning-position) (line-end-position))
        (end-of-line)
        (insert "\n"))
      (insert "\\[")
      (LaTeX-indent-line)
      (insert "\n\n\\]")
      (LaTeX-indent-line)
      (forward-line -1)
      (LaTeX-indent-line)))))
