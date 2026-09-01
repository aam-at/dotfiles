;; -*- lexical-binding: t; -*-
;; This file configures bibtex for use.

(require 'aam-core)

;;;###autoload
(defun aam/citar-setup ()
  "Configure the shared Doom-biblio-style Citar and Org Cite workflow."
  (require 'citar)
  (require 'citar-capf)
  (setq citar-bibliography aam/bibtex-files
        citar-library-paths (list (aam/bib-path "papers/")
                                  (aam/bib-path "review/")
                                  (aam/bib-path "books/"))
        citar-library-file-extensions '("pdf")
        citar-notes-paths (list (aam/org-path "papers"))
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar)

  ;; Keep native Org Cite exports predictable, matching Doom's biblio module.
  (with-eval-after-load 'oc
    (setq org-cite-global-bibliography aam/bibtex-files
          org-cite-export-processors '((latex biblatex) (t csl))
          org-support-shift-select t)
    (require 'oc-biblatex))
  ;; Load CSL only after the top-level Org feature to avoid an incremental-load
  ;; cycle between `oc-csl', Citeproc, and Org.
  (with-eval-after-load 'org
    (require 'oc-csl))

  ;; Helm otherwise takes over `org-cite-insert', bypassing Citar's rich
  ;; completion interface.
  (with-eval-after-load 'helm
    (when (boundp 'helm-completing-read-handlers-alist)
      (add-to-list 'helm-completing-read-handlers-alist '(org-cite-insert))))

  ;; Complete citekeys directly in Org and LaTeX buffers, while retaining
  ;; `org-cite-insert' for inserting correctly formatted citations.
  (dolist (hook '(org-mode-hook LaTeX-mode-hook latex-mode-hook))
    (add-hook hook #'citar-capf-setup)))

;;;###autoload
(defun aam/bibtex-setup ()
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
  (aam/citar-setup)

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
