;; This file configures tex for use.

;;;###autoload
(defun my-tex-setup ()
  ;; Latex/Tex settings
  (defun zeal-latex-settings()
    ;; set manually docset for zeal
    (setq zeal-at-point-docset "latex"))
  (add-hook 'LaTeX-mode-hook 'zeal-latex-settings)

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

  ;; zathura for emacs
  (setq zathura-procs ())
  (defun zathura-forward-search ()
    ;; Open the compiled pdf in Zathura with synctex. This is complicated since
    ;; 1) Zathura refuses to acknowledge Synctex directive if the pdf is not
    ;; already opened
    ;; 2) This means we have to bookkeep open Zathura processes ourselves: first
    ;; open a new pdf from the beginning, if it is not already open. Then call
    ;; Zathura again with the synctex directive.
    (interactive)
    (let* ((zathura-launch-buf (get-buffer-create "*Zathura Output*"))
           (pdfname (TeX-master-file "pdf"))
           (zatentry (assoc pdfname zathura-procs))
           (zatproc (if (and zatentry (process-live-p (cdr zatentry)))
                        (cdr zatentry)
                      (progn
                        (let ((proc (progn (message "Launching Zathura")
                                           (start-process "zathura-launch"
                                                          zathura-launch-buf "zathura"
                                                          "-x" "emacsclient +%{line} %{input}" pdfname))))
                          (when zatentry
                            (setq zathura-procs (delq zatentry zathura-procs)))
                          (add-to-list 'zathura-procs (cons pdfname proc))
                          (set-process-query-on-exit-flag proc nil)
                          proc))))
           (pid (process-id zatproc))
           (synctex (format "%s:0:%s"
                            (TeX-current-line)
                            (TeX-current-file-name-master-relative))))
      (start-process "zathura-synctex" zathura-launch-buf "zathura" "--synctex-forward" synctex pdfname)))
  ;; PDF syncing
  (setq TeX-view-program-list
        '(("Skim" "displayline -b -g %n %o %b")
          ("Evince" "evince --page-index=%(outpage) %o")
          ("Okular" "okular --unique %o#src:%n%b")
          ("Zathura" zathura-forward-search)
          ("PDF Tools" TeX-pdf-tools-sync-view)))
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")
                                     (output-dvi "xdvi")))
  (with-eval-after-load 'tex
    (add-to-list 'TeX-command-list '("View Evince" "evince %o" TeX-run-command nil t
                                     :help "Open document using evince"))
    (add-to-list 'TeX-command-list '("View Zathura" "zathura %o" TeX-run-command nil t
                                     :help "Open document using zathura")))

  ;; Doc-view settings
  (defun my-doc-view-settings ()
    ;; Automatic update for pdf
    (auto-revert-mode)
    ;; Emacs freezes with linum-mode
    (linum-mode -1))

  (add-hook 'doc-view-mode-hook 'my-doc-view-settings)
  (define-advice doc-view-display (:after (&rest _) fit-width)
    "Fit document width to window after displaying."
    (doc-view-fit-width-to-window))

  ;; Define the inner mode for the LaTeX comment environment
  (define-innermode poly-latex-comment-md-innermode
    :mode 'markdown-mode
    :head-mode 'host
    :tail-mode 'host
    :head-matcher "^[ \t]*\\\\begin{comment}.*$"
    :tail-matcher "^[ \t]*\\\\end{comment}.*$"
    :head-adjust-face nil
    :body-indent-offset 0
    :indent-offset 0)

  (define-polymode poly-latex-mode
    :hostmode 'poly-latex-hostmode
    :innermodes '(poly-latex-comment-md-innermode)
    (setq-local polymode-run-these-before-change-functions-in-other-buffers nil)
    (setq-local polymode-run-these-after-change-functions-in-other-buffers nil)))

(provide 'config-tex)
