;; This file configures bibtex for use.

;;;###autoload
(defun my-bibtex-setup ()
  ;; bibtex settings
  (setq bibtex-autokey-name-year-separator ""
        bibtex-autokey-name-separator ""
        bibtex-autokey-name-case-convert 'downcase
        bibtex-autokey-year-length 4
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titleword-length nil
        bibtex-autokey-titlewords 1
        bibtex-autokey-titleword-separator ""
        bibtex-autokey-titlewords-stretch 1
        bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t)

  ;; helm-bibtex
  (spacemacs/set-leader-keys "hc" 'helm-bibtex)
  ;; configure bibtex-completion for org-ref
  (setq bibtex-completion-notes-path (aam/org-path "papers")
        bibtex-completion-bibliography aam/bibtex-files
        bibtex-completion-library-path (list (aam/bib-path "papers/")
                                             (aam/bib-path "review/")
                                             (aam/bib-path "books/"))
        bibtex-completion-find-additional-pdfs t
        bibtex-completion-additional-search-fields '(keywords tags)
        bibtex-completion-pdf-symbol "⌘"
        bibtex-completion-notes-symbol "✎")
  (setq bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}")))
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (LaTeX-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default)))
  (setq bibtex-completion-pdf-open-function 'find-file)

  ;; generate autokey
  (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode "g" 'aam/bibtex-generate-autokey)

  ;; orb-autokey
  (with-eval-after-load 'org-roam-bibtex
    (setq orb-autokey-format "%a%y%t"))

  ;; ebib settings
  (setq ebib-preload-bib-files aam/bibtex-files)
  (evil-set-initial-state 'ebib-index-mode 'emacs)
  (evil-set-initial-state 'ebib-entry-mode 'emacs)
  (evil-set-initial-state 'ebib-log-mode 'emacs))

(defun aam/bibtex-generate-autokey ()
  "Generate a BibTeX key for the current BibTeX entry."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry-start (point))
           (bibtex-key (bibtex-generate-autokey)))
      (goto-char entry-start)
      (search-forward "{")
      (delete-region (point) (line-end-position))
      (insert (concat bibtex-key ","))
      (message "Generated BibTeX key: %s" bibtex-key))))

(provide 'config-bibtex)
