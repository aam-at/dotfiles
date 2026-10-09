;;; aam-funcs-test.el --- Checks for the shared core, Org, LaTeX and C++ helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'aam-core)
(require 'aam-org)
(require 'aam-latex)
(require 'aam-cpp)

(defvar transient-mark-mode)

;;; Org: sort and filter entries by a numeric property.

(defconst aam/test-entries
  "* P
** A
:PROPERTIES:
:YEAR: 2020
:CITATION_COUNT: 5
:IMPACT_FACTOR: 1.5
:END:
** B
body
** C
:PROPERTIES:
:YEAR: 2010
:CITATION_COUNT: 50
:IMPACT_FACTOR: 9
:END:
** D
:PROPERTIES:
:YEAR: 2018
:END:
"
  "A parent P over four entries; B has only a body, D only a year.")

(defmacro aam/test-with-entries (&rest body)
  "Run BODY in an Org buffer of `aam/test-entries', point at the top."
  `(with-temp-buffer
     (org-mode)
     (insert aam/test-entries)
     (goto-char (point-min))
     ,@body))

(defun aam/test-headings ()
  "Headings of the current Org buffer, in order."
  (org-map-entries (lambda () (org-get-heading t t t t))))

(ert-deftest aam/org-sort-entries-put-the-largest-first-and-the-missing-last ()
  (dolist (case '((aam/org-sort-entries-by-year ("P" "A" "D" "C" "B"))
                  (aam/org-sort-entries-by-citations ("P" "C" "A" "B" "D"))
                  (aam/org-sort-entries-by-impact ("P" "C" "A" "B" "D"))))
    (aam/test-with-entries
     (funcall (car case))
     (should (equal (aam/test-headings) (cadr case))))))

(ert-deftest aam/org-sort-by-reads-the-property-with-the-getter-it-is-given ()
  (aam/test-with-entries
   (aam/org-sort-by (lambda (property) (and (equal property "N") "1")) "N")
   (should (equal (aam/test-headings) '("P" "A" "B" "C" "D")))))

(ert-deftest aam/org-filter-entries-highlight-the-large-and-fold-the-rest ()
  (dolist (case '((aam/org-filter-entries-by-year "2015" ("A" "D") ("B" "C"))
                  (aam/org-filter-entries-by-citations "10" ("C") ("A" "B" "D"))
                  (aam/org-filter-entries-by-year "-5" ("A" "B" "C" "D") ())))
    (aam/test-with-entries
     (funcall (car case) (cadr case))
     (let (lit folded)
       (org-map-entries
        (lambda ()
          (let ((name (org-get-heading t t t t)))
            (unless (equal name "P")
              (if (cl-some (lambda (o) (overlay-get o 'face))
                           (overlays-at (org-entry-beginning-position)))
                  (push name lit)
                (when (org-fold-folded-p (line-end-position)) (push name folded)))))))
       (should (equal (nreverse lit) (nth 2 case)))
       (should (equal (nreverse folded) (nth 3 case)))))))

(ert-deftest aam/org-filter-by-clears-the-previous-highlights ()
  (aam/test-with-entries
   (aam/org-filter-by "YEAR" 2015)
   (aam/org-filter-by "YEAR" 3000)
   (should-not (cl-some (lambda (o) (overlay-get o 'face))
                        (overlays-in (point-min) (point-max))))))

(ert-deftest aam/org-roam-get-entry-prop-prefers-the-entry-then-its-link ()
  (let ((target (generate-new-buffer " *target*")))
    (unwind-protect
        (progn
          (with-current-buffer target
            (org-mode)
            (insert "* Linked\n:PROPERTIES:\n:YEAR: 1999\n:END:\n"))
          (cl-letf (((symbol-function 'org-id-find)
                     (lambda (id &optional _marker)
                       (should (equal id "abc"))
                       (with-current-buffer target (copy-marker 1)))))
            (with-temp-buffer
              (org-mode)
              (insert "** Own [[id:abc][T]]\n:PROPERTIES:\n:YEAR: 2001\n:END:\n")
              (goto-char (point-min))
              (should (equal (aam/org-roam-get-entry-prop "YEAR") "2001")))
            (with-temp-buffer
              (org-mode)
              (insert "** Via link [[id:abc][T]]\n")
              (goto-char (point-min))
              (should (equal (aam/org-roam-get-entry-prop "YEAR") "1999"))
              (should-not (aam/org-roam-get-entry-prop "NOPE")))))
      (kill-buffer target))))

;;; Org: review template.

(ert-deftest aam/org-convert-json-to-review-inserts-the-template ()
  (with-temp-buffer
    (aam/org-convert-json-to-review
     "{\"TLDR\":\"t\",\"Summary\":\"s\",\"Related_work\":[{\"citation\":\"c1\",\"relationship\":\"r1\"},{\"citation\":\"c2\",\"relationship\":\"r2\"}],\"Contributions\":[\"a\",\"b\"],\"Approach\":\"ap\",\"Pros\":[\"p1\"],\"Cons\":[\"x1\",\"x2\"],\"Notes_and_questions\":[\"n\"],\"Future_work\":[\"f\"],\"Conclusion\":\"done\"}")
    (should (equal (buffer-string)
                   "+ *TLDR:*\n\n  <<tldr>>t\n+ *Summary:*\n\n  <<summary>>s\n+ *Related work:*\n  * c1 - r1\n  * c2 - r2\n+ *Contributions:*\n  1. a\n  2. b\n+ *Approach:*\n\n  ap\n+ *Pros:*\n  * p1\n+ *Cons:*\n  * x1\n  * x2\n+ *Notes and questions:*\n  * n\n+ *Future work:*\n  * f\n+ *Conclusion:*\n\n  done"))))

(ert-deftest aam/org-convert-json-to-review-tolerates-missing-fields ()
  (with-temp-buffer
    (aam/org-convert-json-to-review "{\"TLDR\":\"only\"}")
    (should (string-prefix-p "+ *TLDR:*\n\n  <<tldr>>only\n+ *Summary:*" (buffer-string)))
    (should (string-suffix-p "+ *Conclusion:*\n\n  " (buffer-string)))))

;;; Org: narrowing to a subtree in an indirect buffer.

(ert-deftest aam/org-narrow-to-subtree-remembers-the-file-and-widen-returns ()
  (let* ((file (make-temp-file "narrow" nil ".org" "* A\n** B\n*** C\n"))
         (base (find-file-noselect file)))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer base)
          (org-mode)
          (goto-char (point-min))
          (org-next-visible-heading 1)
          (forward-line 1)
          (aam/org-narrow-to-subtree)
          (let ((first (current-buffer)))
            (should-not (eq first base))
            (should (equal org-indirect-buffer-file-name file))
            (should (eq org-indirect-above-buffer base))
            (goto-char (point-min))
            (aam/org-narrow-to-subtree)
            (let ((second (current-buffer)))
              (should-not (eq second first))
              (should (equal org-indirect-buffer-file-name file))
              (should (eq org-indirect-above-buffer first))
              (aam/org-widen-from-subtree)
              (should (eq (current-buffer) first))
              (should-not (buffer-live-p second))))
          (aam/org-widen-from-subtree)
          (should (eq (current-buffer) base)))
      (kill-buffer base)
      (delete-file file))))

;;; Org: id link to file link.

(ert-deftest aam/org-convert-org-id-link-to-file-link-rewrites-an-id-link ()
  (cl-letf (((symbol-function 'org-id-find)
             (lambda (id) (should (equal id "abc")) '("/home/u/Dropbox/Org/a.org" . 1))))
    (with-temp-buffer
      (insert "see [[id:abc][Title]] now")
      (goto-char 8)
      (aam/org-convert-org-id-link-to-file-link)
      (should (equal (buffer-string) "see [[file:~/Dropbox/Org/a.org][Title]] now")))
    (with-temp-buffer
      (insert "[[https://example.com][Site]]")
      (goto-char 5)
      (aam/org-convert-org-id-link-to-file-link)
      (should (equal (buffer-string) "[[https://example.com][Site]]")))))

;;; Org: slugs, dated file names and notes with ids (stdlib only).

(ert-deftest aam/org-roam-title-to-slug-dashes-everything-but-letters-and-digits ()
  (should (equal (aam/org-roam--title-to-slug "Hello, World!") "hello--world-"))
  (should (equal (aam/org-roam--title-to-slug "snake__case_") "snake--case-"))
  (should (equal (aam/org-roam--title-to-slug "_x_ Y2") "-x--y2")))

(ert-deftest aam/org-roam-get-filepath-with-date-prefixes-the-slug ()
  (cl-letf (((symbol-function 'format-time-string) (lambda (&rest _) "20260101-1200")))
    (should (equal (aam/org-roam-get-filepath-with-date "my-note") "20260101-1200_my-note.org"))))

(ert-deftest aam/org-id-list-files-finds-org-files-that-hold-an-id ()
  (let ((root (file-name-as-directory (file-truename (make-temp-file "ids" t)))))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "sub" root))
          (dolist (spec '(("a.org" . ":ID: 1") ("b.org" . "none") ("sub/c.org" . ":ID: 2")
                          ("d.txt" . ":ID: 3")))
            (write-region (cdr spec) nil (expand-file-name (car spec) root) nil 'silent))
          (should (eq (aam/org-id--in-file (expand-file-name "a.org" root)) t))
          (should (eq (aam/org-id--in-file (expand-file-name "b.org" root)) nil))
          (should (equal (sort (mapcar (lambda (f) (file-relative-name f root))
                                       (aam/org-id--list-files root))
                               #'string<)
                         '("a.org" "sub/c.org"))))
      (delete-directory root t))))

;;; Core: PDF text extraction.

(defconst aam/test-pdf
  "%PDF-1.1
1 0 obj<</Type/Catalog/Pages 2 0 R>>endobj
2 0 obj<</Type/Pages/Kids[3 0 R]/Count 1>>endobj
3 0 obj<</Type/Page/Parent 2 0 R/MediaBox[0 0 200 200]/Contents 4 0 R/Resources<</Font<</F1 5 0 R>>>>>>endobj
4 0 obj<</Length 41>>stream
BT /F1 18 Tf 20 100 Td (Hello pdf) Tj ET
endstream endobj
5 0 obj<</Type/Font/Subtype/Type1/BaseFont/Helvetica>>endobj
trailer<</Root 1 0 R/Size 6>>
%%EOF
"
  "A one-page PDF that says \"Hello pdf\".")

(defun aam/test-fake-pdftotext (body)
  "Call BODY with `pdftotext' faked to print \"Hello pdf\"; return each run's args."
  (let (runs)
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "pdftotext"))
              ((symbol-function 'call-process)
               (lambda (program _in out _display &rest args)
                 (push (cons program (cons out args)) runs)
                 (insert "Hello pdf\n")
                 0)))
      (funcall body))
    (nreverse runs)))

(ert-deftest aam/pdf-extract-rejects-what-is-not-a-pdf-or-has-no-tool ()
  (let ((text (make-temp-file "notpdf" nil ".txt" "x")))
    (unwind-protect
        (progn
          (should-error (aam/extract-pdf-text-from-file "/no/such/file.pdf"))
          (should-error (aam/extract-pdf-text-from-file text))
          (should-error (aam/pdf--extract nil))
          (let ((pdf (make-temp-file "p" nil ".pdf" aam/test-pdf)))
            (unwind-protect
                (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
                  (should-error (aam/extract-pdf-text-from-file pdf)))
              (delete-file pdf))))
      (delete-file text))))

(ert-deftest aam/pdf-extract-pipes-pdftotext-into-a-fresh-buffer ()
  (let* ((dir (file-name-as-directory (make-temp-file "pdfdir" t)))
         (temporary-file-directory dir)
         (pdf (make-temp-file "p" nil ".pdf" aam/test-pdf)))
    (unwind-protect
        (save-window-excursion
          (let ((runs (aam/test-fake-pdftotext (lambda () (aam/extract-pdf-text-from-file pdf)))))
            (should (equal runs `(("pdftotext" t "-layout" "-nopgbrk" ,pdf "-"))))
            (should (equal (buffer-name) "*PDF Text*"))
            (should (equal (buffer-string) "Hello pdf\n"))
            (should (= (point) (point-min)))
            (should-not (buffer-modified-p))
            (kill-buffer))
          ;; Nothing is written to disk besides the PDF itself.
          (should (equal (directory-files dir nil directory-files-no-dot-files-regexp)
                         (list (file-name-nondirectory pdf)))))
      (delete-directory dir t))))

(ert-deftest aam/pdf-extract-from-the-current-buffer-needs-a-pdf-view-buffer ()
  (let ((pdf (make-temp-file "p" nil ".pdf" aam/test-pdf)))
    (unwind-protect
        (save-window-excursion
          (with-temp-buffer
            (setq buffer-file-name pdf)
            (should-error (aam/extract-pdf-text-from-current-buffer))
            (setq major-mode 'pdf-view-mode)
            (let ((runs (aam/test-fake-pdftotext #'aam/extract-pdf-text-from-current-buffer)))
              (should (equal (nth 4 (car runs)) pdf))
              (should (equal (buffer-string) "Hello pdf\n"))
              (kill-buffer))))
      (delete-file pdf))))

(ert-deftest aam/pdf-extract-reads-a-real-pdf-with-pdftotext ()
  (skip-unless (executable-find "pdftotext"))
  (let ((pdf (make-temp-file "p" nil ".pdf" aam/test-pdf)))
    (unwind-protect
        (save-window-excursion
          (aam/extract-pdf-text-from-file pdf)
          (should (string-match-p "Hello pdf" (buffer-string)))
          (kill-buffer))
      (delete-file pdf))))

;;; Core: bibliography files.

(ert-deftest aam/bibtex-files-follow-the-bibliography-root ()
  (let ((aam/bib-root "/lib/")
        (aam/bibtex-files nil))
    (should (equal (aam/bib--files)
                   (mapcar (lambda (n) (expand-file-name n "/lib/"))
                           '("refs.bib" "review.bib" "books.bib" "myrefs.bib"))))
    (let ((root (file-name-as-directory (file-truename (make-temp-file "bib" t)))))
      (unwind-protect
          (progn
            (aam/set-bibliography-root root)
            (should (equal aam/bib-root (file-truename root)))
            (should (equal aam/bibtex-files
                           (mapcar (lambda (n) (expand-file-name n root))
                                   '("refs.bib" "review.bib" "books.bib" "myrefs.bib")))))
        (delete-directory root t)))))

;;; LaTeX.

(defconst aam/test-prose "One sentence here.  Another sentence follows it.  Third one is last.\n")

(defconst aam/test-split-prose
  "One sentence here.\nAnother sentence follows it.\nThird one is last."
  "`aam/test-prose' split by sentence, without the blank lines the split leaves at the end.")

(ert-deftest aam/latex-split-puts-each-sentence-on-its-own-line ()
  (with-temp-buffer
    (insert aam/test-prose)
    (aam/latex-split-to-one-sentence-per-line)
    (should (equal (string-trim-right (buffer-string)) aam/test-split-prose))))

(ert-deftest aam/latex-split-leaves-text-outside-the-region-alone ()
  (with-temp-buffer
    (insert "First one.  Second one.  Third one.  Fourth one.\n")
    (set-mark 14)
    (goto-char 40)
    (transient-mark-mode 1)
    (activate-mark)
    (let ((outer (buffer-substring 1 14)))
      (aam/latex-split-to-one-sentence-per-line)
      (should (string-prefix-p outer (buffer-string)))
      (should (string-match-p "\n" (buffer-substring 14 (point-max)))))))

(ert-deftest aam/latex-cycle-text-wrapping-fills-then-splits-by-sentence ()
  (with-temp-buffer
    (insert aam/test-prose)
    (goto-char (point-min))
    (let ((splits 0))
      (cl-letf (((symbol-function 'aam/latex-split-to-one-sentence-per-line)
                 (lambda () (cl-incf splits))))
        (aam/latex-cycle-text-wrapping)
        (should (= splits 0))
        (aam/latex-cycle-text-wrapping)
        (should (= splits 1)))))
  (with-temp-buffer
    (insert aam/test-prose)
    (goto-char (point-min))
    (aam/latex-cycle-text-wrapping)
    (should (equal (buffer-string) aam/test-prose))
    (aam/latex-cycle-text-wrapping)
    (should (equal (string-trim-right (buffer-string)) aam/test-split-prose))))

(ert-deftest aam/latex-cycle-text-wrapping-restores-the-original ()
  (with-temp-buffer
    (insert aam/test-prose)
    (goto-char (point-min))
    (dotimes (_ 3) (aam/latex-cycle-text-wrapping))
    (should (equal (buffer-string) aam/test-prose))))

;; Known bug, present before the refactor: the restore step deletes up to the
;; end it measured before the split shrank the text, so it signals.
(ert-deftest aam/latex-cycle-text-wrapping-restore-fails-when-the-split-shrinks-the-text ()
  :expected-result :failed
  (with-temp-buffer
    (insert "One sentence here.  Another sentence follows it.\nThird one is last.\n")
    (goto-char (point-min))
    (dotimes (_ 3) (aam/latex-cycle-text-wrapping))))

;;; C++.

(ert-deftest aam/cpp-format-region-or-buffer-formats-the-region-when-active ()
  (let (calls)
    (cl-letf (((symbol-function 'clang-format-region)
               (lambda (beg end) (push (list 'region beg end) calls)))
              ((symbol-function 'clang-format-buffer) (lambda () (push 'buffer calls)))
              ((symbol-function 'whitespace-cleanup) (lambda () (push 'cleanup calls))))
      (with-temp-buffer
        (insert "int  x ;\nint y;\n")
        (let ((transient-mark-mode t))
          (aam/cpp-format-region-or-buffer)
          (should (equal (reverse calls) '(buffer cleanup)))
          (setq calls nil)
          (set-mark 1)
          (goto-char 9)
          (activate-mark)
          (aam/cpp-format-region-or-buffer)
          (should (equal (reverse calls) '((region 1 9) cleanup))))))))

;;; aam-funcs-test.el ends here
