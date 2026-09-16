;; -*- lexical-binding: t; -*-
;; This file configures tex for use.

;; `define-innermode' and `define-polymode' are macros, so load them first.
(require 'polymode)

(defun aam/poly-latex-keep-outline-vars ()
  "Stop polymode copying LaTeX outline settings into the Org-style chunk.
Polymode reads this list in the buffer it leaves, so set it in all of them."
  (setq-local polymode-move-these-vars-from-base-buffer
              (seq-difference polymode-move-these-vars-from-base-buffer
                              '(outline-regexp outline-level))))

;; Org-style notes inside LaTeX comment environments:
;;   \begin{comment}
;;   * Heading
;;   | a | b |
;;   \end{comment}
;; The inner mode is not `org-mode': Org parses elements in the base buffer
;; (`org-with-base-buffer'), which is the LaTeX buffer here, so Org commands
;; break.  `orgtbl-mode' edits tables and `outline-minor-mode' folds `*'
;; headings; neither parses the buffer.
(define-derived-mode aam/latex-comment-orgtbl-mode text-mode "OrgTbl"
  "Text mode with Org tables and `*' headings, for LaTeX comment environments.
TAB aligns tables and cycles heading visibility; M-<left>/<right> promote and
demote headings."
  (orgtbl-mode 1)
  (aam/poly-latex-keep-outline-vars)
  (setq-local outline-regexp "\\*+ "
              outline-minor-mode-cycle t
              outline-minor-mode-highlight 'override)
  (outline-minor-mode 1)
  (local-set-key (kbd "M-<left>") #'outline-promote)
  (local-set-key (kbd "M-<right>") #'outline-demote))

(define-innermode poly-latex-comment-orgtbl-innermode
		  :mode 'aam/latex-comment-orgtbl-mode
		  :head-mode 'host
		  :tail-mode 'host
		  :head-matcher "^[ \t]*\\\\begin{comment}.*\n"
		  :tail-matcher "^[ \t]*\\\\end{comment}.*$"
		  :head-adjust-face nil
		  :indent-offset 0)

(define-polymode poly-latex-mode
		 :hostmode 'poly-latex-hostmode
		 :innermodes '(poly-latex-comment-orgtbl-innermode)
		 (aam/poly-latex-keep-outline-vars))

(defun aam/poly-latex-maybe-enable ()
  "Enable `poly-latex-mode' when the buffer has a comment environment.
Run \\[poly-latex-mode] by hand after adding the first one."
  (unless (or (bound-and-true-p polymode-mode) (buffer-base-buffer))
    (when (save-excursion
            (goto-char (point-min))
            (re-search-forward "^[ \t]*\\\\begin{comment}" nil t))
      (poly-latex-mode 1))))

;;;###autoload
(defun aam/tex-setup ()
  ;; Auctex settings
  (setq-default TeX-master nil) ; Query for master file.
  (setq TeX-parse-self t ; parse on load
        TeX-auto-save t  ; parse on save
        TeX-auto-untabify t ; untabify when saving
        ;; Use hidden directories for AUCTeX files.
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ;; Don't start the Emacs server when correlating sources.
        TeX-source-correlate-start-server nil
        ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
        TeX-electric-sub-and-superscript t
        ;; Just save, don't ask before each compilation.
        TeX-save-query nil)
  ;; pdf-tools settings
  (with-eval-after-load 'pdf-tools
    (add-hook 'pdf-view-mode-hook #'pdf-sync-minor-mode))

  (add-hook 'LaTeX-mode-hook #'aam/poly-latex-maybe-enable)

  ;; relative line numbers (LaTeX is a text mode, so prog-mode settings skip it)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq-local display-line-numbers-type 'relative)
              (display-line-numbers-mode 1)))

  ;; PDF viewers.  C-c C-v uses PDF Tools; AUCTeX's built-in "Zathura" viewer
  ;; also forward-searches.  External viewers with forward search via C-c C-c:
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")
                                     (output-dvi "xdvi")))
  (with-eval-after-load 'tex
    ;; Inverse search (Ctrl+click in Zathura) goes back to this Emacs daemon.
    (add-to-list 'TeX-command-list
                 `("View Zathura"
                   ,(concat "zathura --synctex-forward %n:0:\"%b\" -x \"emacsclient -s "
                            server-name " +%{line} %{input}\" %o")
                   TeX-run-discard-or-function nil t
                   :help "Open document at point in Zathura"))
    ;; MuPDF has no SyncTeX support: ask synctex for the page and open it there.
    ;; ponytail: opens a new MuPDF window per call; MuPDF cannot move a running one.
    (add-to-list 'TeX-command-list
                 '("View MuPDF"
                   "mupdf %o $(synctex view -i %n:0:\"%b\" -o %o | sed -n 's/^Page://p' | head -n 1)"
                   TeX-run-discard-or-function nil t
                   :help "Open document at point's page in MuPDF")))

  ;; doc-view: reload the PDF on change and fit it to the window
  (add-hook 'doc-view-mode-hook #'auto-revert-mode)
  (define-advice doc-view-display (:after (&rest _) fit-width)
    "Fit document width to window after displaying."
    (doc-view-fit-width-to-window)))

(provide 'config-tex)
