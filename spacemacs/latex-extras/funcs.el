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
