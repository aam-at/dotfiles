;; This file configures tex for use.
(defun my-tex-setup ()
  ;; Latex/Tex settings
  (defun zeal-latex-settings()
    ;; set manually docset for zeal
    (setq zeal-at-point-docset "latex"))
  (add-hook 'LaTeX-mode-hook 'zeal-latex-settings)

  ;; Auctex settings
  (setq-default TeX-master nil) ; Query for master file.
  (setq TeX-auto-untabify t)
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
  (setq TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)
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
  (defadvice doc-view-display (after fit-width activate)
    (doc-view-fit-width-to-window)))

(provide 'config-tex)
