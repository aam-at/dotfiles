;; This file configures org-mode for use.
(setq org-rating-guide (aam/org-path "templates/rating_guide.org"))
(setq org-gtd-trigger-list (aam/org-path "templates/trigger_list.org"))

(defvar org-template-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)  ;; Changed to quit-window
    map))

(define-derived-mode org-template-view-mode org-mode "Template View"
  "A special mode for displaying org templates."
  (use-local-map org-template-view-mode-map)
  (read-only-mode 1))

(defun org-show-template (template-path buffer-name)
  "Display a template file in a split window.
TEMPLATE-PATH is the path to the template file.
BUFFER-NAME is the name of the buffer to display it in."
  (if-let ((existing-buffer (get-buffer buffer-name)))
      ;; If the buffer exists, ensure it is displayed
      (display-buffer existing-buffer '(display-buffer-same-window))
    ;; Otherwise, create the buffer and display the template
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert-file-contents template-path)
      (org-template-view-mode)))
  ;; Always display the buffer in a visible window
  (display-buffer buffer-name '((display-buffer-pop-up-window
                                 display-buffer-same-window))))

;; Convenience functions for specific templates
(defun org-show-rating-guide ()
  "Display the rating guide template."
  (interactive)
  (org-show-template org-rating-guide "*Rating Guide*"))

(defun org-show-gtd-trigger-list ()
  "Display the GTD trigger list template."
  (interactive)
  (org-show-template org-gtd-trigger-list "*GTD Trigger List*"))

(defun org-daily-journal-find-location ()
  "Open today's daily journal file for use with `org-capture`.
This function ensures the journal entry is opened or created if it does not exist.
It also inhibits inserting the heading since `org-capture` will handle that.
If the journal is not daily, it narrows the buffer to the current subtree.

Finally, the cursor is placed at the end of the buffer, ready for editing."
  (interactive)
  ;; Open today's journal entry, suppressing the automatic heading insertion
  (org-journal-new-entry t)
  ;; If the journal type is not daily, narrow to the subtree for better focus
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  ;; Move the cursor to the end of the buffer
  (goto-char (point-max)))

(defun org-weekly-journal-file ()
  "Return path to current week's journal file (format: YYYYWNN.org).
Uses `aam/org-path` to locate the journal/ directory."
  (let* ((week-file-name (format-time-string "%GW%V.org")) ;; ISO week format
         (journal-path (aam/org-path "journal/")) ;; Base directory
         (full-file-path (expand-file-name week-file-name journal-path))) ;; Complete path
    full-file-path))

(defun org-weekly-journal-find-location ()
  "Open or create this week's journal file based on the current ISO week number.
The file is named using the format `<Year>W<Week>.org` (e.g., `2023W42.org`)
and stored in the `journal/` directory under the path returned by `aam/org-path`.

This function is intended for use with `org-capture` workflows."
  (interactive)
  (let* ((full-file-path (org-weekly-journal-file))) ;; Complete path
    ;; Create the file with a default structure if it does not exist
    (unless (file-exists-p full-file-path)
      (with-temp-file full-file-path
        (insert "#+TODO: TODO(t) NEXT(n) | DONE(d) FAILED(f)\n")
        (insert (format-time-string "#+TITLE: Week %V, %G\n\n"))))
    ;; Open the file and move to the end
    (find-file full-file-path)
    (goto-char (point-max))))

(defun org-monthly-journal-file ()
  "Return path to current month's journal file (format: YYYYMM.org).
Uses `aam/org-path` to locate the journal/ directory."
  (let* ((month-file-name (format-time-string "%GM%m.org")) ;; File name format
         (journal-path (aam/org-path "journal/")) ;; Base directory
         (full-file-path (expand-file-name month-file-name journal-path))) ;; Complete path
    full-file-path))

(defun org-monthly-journal-find-location ()
  "Open or create this month's journal file based on the current year and month.
The file is named using the format `<Year>M<Month>.org` (e.g., `2024M1.org`)
and stored in the `journal/` directory under the path returned by `aam/org-path`.

This function is intended for use with `org-capture` workflows."
  (interactive)
  (let* ((full-file-path (org-monthly-journal-file))) ;; Complete path
    ;; Create the file with a default structure if it does not exist
    (unless (file-exists-p full-file-path)
      (with-temp-file full-file-path
        (insert "#+TODO: TODO(t) NEXT(n) | DONE(d) FAILED(f)\n")
        (insert (format-time-string "#+TITLE: %B, %G\n\n"))))
    ;; Open the file and move to the end
    (find-file full-file-path)
    (goto-char (point-max))))

;;;###autoload
(defun my-org-setup ()
  ;; org settings
  (setq org-directory aam/org-root)
  (setq aam/org-inbox (aam/org-path "inbox.org"))
  (setq deft-directory org-directory
        deft-recursive t
        deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\|journal\\|data\\)$")
  ;; set org-roam directory before loading agenda
  (setq org-default-notes-file (aam/org-path "refile.org"))
  (add-hook 'org-mode-hook 'spacemacs/toggle-auto-fill-mode-on)
  ;; vulpea settings
  (setq vulpea-db-sync-directories (list org-directory))
  (vulpea-db-autosync-mode +1)
  ;; latex preview for org-mode
  (setq org-latex-create-formula-image-program 'dvipng
        org-preview-latex-image-directory "ltximg/")

  (setq org-deadline-warning-days 4
        org-edit-src-content-indentation 0
        org-enforce-todo-dependencies t
        org-hide-emphasis-markers t
        org-list-allow-alphabetical t
        org-startup-indented t
        org-use-speed-commands t
        org-clock-idle-time 30)

  ;; org safe-variables
  (put 'org-download-image-dir 'safe-local-variable (lambda (_) t))
  (put 'org-download-heading-lvl 'safe-local-variable (lambda (_) t))
  (put 'org-attach-id-dir 'safe-local-variable (lambda (_) t))
  (put 'org-use-property-inheritance 'safe-local-variable (lambda (_) t))
  (put 'org-archive-location 'safe-local-variable (lambda (_) t))
  (put 'org-current-tag-alist 'safe-local-variable (lambda (_) t))

  ;; sync buffers
  (add-hook 'org-mode-hook 'auto-revert-mode)

  ;; org appearance
  (setq org-startup-indented t)
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#ff6347" :weight bold))
          ("NEXT" . (:foreground "#4169e1" :weight bold))
          ("DONE" . (:foreground "#32cd32" :weight bold))
          ("FAILED" . (:foreground "#ff0000" :weight bold :strike-through t))))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.0))
  ;; org-modern
  (setq org-modern-table nil)
  (setq org-modern-priority
        (quote ((?A . "💥")
                (?B . "🌟")
                (?C . "💤"))))
  (setq org-modern-todo-faces
        '(("TODO" . (:background "#ff6347" :foreground "#ffffff" :weight bold))
          ("NEXT" . (:background "#4169e1" :foreground "#ffffff" :weight bold))
          ("DONE" . (:background "#32cd32" :foreground "#000000" :weight bold))
          ("FAILED" . (:background "#ff0000" :foreground "#ffffff" :weight bold :strike-through t))))
  ;; agenda settings
  (setq org-agenda-files (list (aam/org-path "inbox.org")
                               (aam/org-path "habits.org")
                               (aam/org-path "someday.org")
                               (aam/org-path "work.org")
                               (aam/org-path "personal.org")
                               (aam/org-path "ideas.org")
                               (aam/org-path "archived.org")))
  (setq aam/org-agenda-projects (org-extras/get-active-headline-files (aam/org-path "projects/index.org")))
  (setq org-agenda-files (append aam/org-agenda-projects org-agenda-files))
  (setq org-columns-default-format "%50ITEM(Title) %SCHEDULED(Date) %TAGS(Tags) %PRIORITY(P) %TODO(Todo)")

  (setq org-tag-alist '(("important" . ?i)
                        ("urgent"    . ?u)))
  (setq org-use-fast-tag-selection 'expert
        org-use-fast-todo-selection 'expert)
  (setq org-agenda-time-grid '((daily today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               "......" "----------------------")
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-include-deadlines t
        org-agenda-include-diary t
        org-agenda-include-diary t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-span 'day
        org-agenda-start-with-clockreport-mode t
        org-agenda-start-with-log-mode t
        org-agenda-sticky t)
  (setq org-agenda-custom-commands
        '(
          ;; Daily overview agenda
          ("d" "Daily overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :todo "TODAY"
                                  :scheduled today
                                  :order 0)
                           (:habit t)
                           (:name "Due Today"
                                  :deadline today
                                  :order 2)
                           (:name "Due Soon"
                                  :deadline future
                                  :order 8)
                           (:name "Overdue"
                                  :deadline past
                                  :order 7)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Inbox" :file-path "inbox" :order 0)
                            (:auto-property "PROJECT_ID" :order 9)))))))
          ;; Another daily overview agenda
          ("D" "Another daily overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :todo "TODAY"
                                  :scheduled today
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Next to do"
                                   :todo "NEXT"
                                   :order 1)
                            (:name "Due Today"
                                   :deadline today
                                   :order 2)
                            (:name "Due Soon"
                                   :deadline future
                                   :order 8)
                            (:name "Overdue"
                                   :deadline past
                                   :order 7)))))))
          ;; Projects overview agenda
          ("p" "Projects overview"
           ((alltodo ""
                     ((org-super-agenda-groups
                       '((:auto-property "PROJECT_ID")))))))
          ;; GTD agenda
          ("g" "Get Things Done (GTD)"
           ((agenda ""
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)))
            (todo "NEXT"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTasks\n")))
            (agenda nil
                    ((org-agenda-entry-types '(:deadline))
                     (org-agenda-format-date "")
                     (org-deadline-warning-days 7)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                     (org-agenda-overriding-header "\nDeadlines")))
            (tags-todo "inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nInbox\n")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n")))))
          ;; Eisenhower matrix agenda
          ("e" "Eisenhower matrix"
           ((todo ""
                  ((org-agenda-overriding-header "Eisenhower matrix")
                   (org-super-agenda-groups
                    '((:name "Do (urgent and important)"
                             :and (:tag "important" :tag "urgent"))
                      (:name "Schedule (important but not urgent)"
                             :and (:tag "important" :not (:tag "urgent")))
                      (:name "Delegate (urgent but not important)"
                             :and (:tag "urgent" :not (:tag "important")))
                      (:name "Declutter (not urgent and not important)"
                             :and (:not (:tag "important") :not (:tag "urgent")))))))))))

  (setq org-clock-history-length 23
        org-clock-in-resume t
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-when-done t
        org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; org capture settings
  (defun aam/org-capture-note-filepath (&optional with-date)
    "Return path to note with datetime prefix or without it"
    (funcall (-partial 'org-extras/roam-get-filepath-for-title
                       (aam/org-path "notes")
                       (when (eq with-date t)
                         'org-extras/roam-get-filepath-with-date))))
  (defun aam/org-capture-note-filepath-with-date ()
    (funcall (-partial 'aam/org-capture-note-filepath t)))
  (defun aam/org-capture-project-filepath ()
    "Return path to structured note"
    (funcall (-partial 'org-extras/roam-get-filepath-for-title
                       (aam/org-path "projects"))))
  (defun aam/org-capture-area-filepath ()
    "Return path to structured note"
    (funcall (-partial 'org-extras/roam-get-filepath-for-title
                       (aam/org-path "areas"))))
  (defun aam/org-capture-org-roam-link (file)
    (let ((node (with-current-buffer
                    (get-file-buffer file)
                  (org-roam-node-at-point))))
      (format "[[id:%s][%s]]"
              (org-roam-node-id node)
              (org-roam-node-title node))))
  ;; configure org-roam
  (setq org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-autosync-mode)

  ;; configure org-capture
  (add-hook 'org-capture-mode-hook #'org-align-tags)
  (add-hook 'org-capture-after-finalize-hook (lambda () (if (org-roam-file-p) (org-roam-db-sync))))

  (defun aam/org-roam-capture-finalize ()
    "Insert text at the beginning of the captured file."
    (save-excursion
      (goto-char (point-min))
      (insert ":PROPERTIES:\n")
      (insert (format ":ID: %s\n" (org-id-new)))
      (insert ":END:\n")))
  (setq org-capture-templates
        `(
          ("t" "Todo" entry (file ,aam/org-inbox)
           "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:\n")
          ("T" "Todo with context" entry (file ,aam/org-inbox)
           "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
%c
%i
Link: %a\n")
          ("r" "Read it later" entry (file, aam/org-inbox)
           "* TODO Read %(aam/org-capture-org-roam-link (org-capture-get :original-file)) :readlater:
:PROPERTIES:
:CREATED: %U
:END:\n")
          ("R" "Read it later" entry (file, aam/org-inbox)
           "* TODO Read %(aam/org-capture-org-roam-link (org-capture-get :original-file)) :readlater:important:
DEADLINE: %^{Deadline}t
:PROPERTIES:
:CREATED: %U
:END:\n")
          ("e" "Email" entry
           (file ,aam/org-inbox)
           "* TODO [#A] Reply: %a :@home:@work:"
           :immediate-finish t)
          ("j" "Journal entry" plain (function org-daily-journal-find-location)
           "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
           :jump-to-captured t :immediate-finish t)
          ("l" "Web link" entry (file ,aam/org-inbox)
           "* TODO %(org-cliplink-capture) :readlater:
:PROPERTIES:
:CREATED: %U
:END:\n" :immediate-finish t)
          ("f" "Elfeed link" entry (file ,aam/org-inbox)
           "* %?%:description :readlater:
:PROPERTIES:
:CREATED: %U
:END:
- %:link
%(when (< 0 (length \"%:elfeed-entry-link\")) (concat \"- web link: \" \"%:elfeed-entry-link\"))"
           :immidiate-finish t)
          ;; template for org-protocol-capture-html
          ("w" "Web site" entry (file "")
           "* %a :website:\n\n%U %?\n\n%:initial")
          ;; Snippets
          ("s" "Snippets")
          ;; Snippets for journaling
          ("sg" "Gratitude journal" plain
           (function org-daily-journal-find-location)
           (file ,(aam/org-path "templates/gratitude_pages.org"))
           :jump-to-captured t)
          ("sp" "Morning Pages Note" plain
           (function org-daily-journal-find-location)
           (file ,(aam/org-path "templates/morning_pages.org"))
           :jump-to-captured t)
          ;; Snippets for planning and reviewing
          ("sd" "Daily Review" plain
           (function org-daily-journal-find-location)
           (file ,(aam/org-path "templates/daily_review.org"))
           :jump-to-captured t)
          ("sw" "Weekly Plan" plain
           (function org-weekly-journal-find-location)
           (file ,(aam/org-path "templates/weekly_plan.org"))
           :jump-to-captured t)
          ("sW" "Weekly Review" plain
           (function org-weekly-journal-find-location)
           (file ,(aam/org-path "templates/weekly_review.org"))
           :jump-to-captured t)
          ("sm" "Monthly Plan" plain
           (function org-monthly-journal-find-location)
           (file ,(aam/org-path "templates/monthly_plan.org"))
           :jump-to-captured t)
          ("sM" "Monthly Review" plain
           (function org-monthly-journal-find-location)
           (file ,(aam/org-path "templates/monthly_review.org"))
           :jump-to-captured t)
          ;; Snippets for zettelkasten and PARA
          ("sn" "Simple (Atomic) Note" plain
           (file aam/org-capture-note-filepath-with-date)
           (file ,(aam/org-path "templates/note.org"))
           :hook aam/org-roam-capture-finalize
           :jump-to-captured t)
          ("sN" "Named (Structured) Note" plain
           (file aam/org-capture-note-filepath)
           (file ,(aam/org-path "templates/note.org"))
           :hook aam/org-roam-capture-finalize
           :jump-to-captured t)
          ;; PARA projects and areas
          ("sA" "Area" plain
           (file aam/org-capture-area-filepath)
           (file ,(aam/org-path "templates/area.org"))
           :hook aam/org-roam-capture-finalize
           :jump-to-captured t)
          ("sP" "Project" plain
           (file aam/org-capture-project-filepath)
           (file ,(aam/org-path "templates/project.org"))
           :hook aam/org-roam-capture-finalize
           :jump-to-captured t)))

  ;; org babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (shell .t)
     (sql . nil)
     (sqlite . t)))
  (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
        org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

  (setq org-log-into-drawer "LOGBOOK")

  ;; org-roam settings
  (setq org-roam-tag-sources '(prop last-directory))
  (setq org-roam-file-exclude-regexp '("data" "templates" "archived" "drafts"))
  (setq org-roam-graph-exclude-matcher '("journal" "inbox.org"))
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-capture-templates
        '(("r" "Notes for bibliography reference" plain
           "
* %^{year} - ${title}
:PROPERTIES:
:CREATED: %U
:Custom_ID: %^{citekey}
:AUTHOR: %^{author}
:JOURNAL: %^{journal}
:YEAR: %^{year}
:DOI: %^{doi}
:URL: %^{url}
:NOTER_DOCUMENT: %(aam-get-cite-pdf-filename \"%^{citekey}\")
:END:
[[file:%(aam-get-cite-pdf-filename \"%^{citekey}\")][pdf]]
%?"
           :if-new
           (file+head "papers/${citekey}.org" "#+TITLE: ${title}
#+STARTUP: latexpreview")
           :empty-lines 1
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?" :target
           (file+head "${slug}.org" "#+title: ${title}")
           :unnarrowed t)))

  ;; org-roam-bibtex settings
  (setq orb-preformat-keywords
        '("citekey" "date" "year" "type" "pdf?" "note?" "author" "editor"
          "journal" "url" "doi" "keywords"
          "author-abbrev" "editor-abbrev" "author-or-editor-abbrev"))
  (with-eval-after-load 'orb-note-actions
    (add-to-list 'orb-note-actions-user (cons "Open PDF file(s) externally" #'aam-open-pdf-external)))

  ;; configure org-journal
  (setq org-journal-dir (aam/org-path "journal/"))
  (setq org-journal-date-prefix "#+TODO: TODO(t) NEXT(n) | DONE(d) FAILED(f)\n* ")
  (setq org-journal-file-format "%Y%m%d.org"
        org-journal-enable-encryption nil
        org-journal-enable-cache t)
  (setq org-journal-agenda-days 7)
  (setq aam/journal-agenda-files (org-extras/org-journal-list-agenda-files org-journal-agenda-days))
  (setq org-agenda-files (append aam/journal-agenda-files org-agenda-files))

  ;; show images inline (only works in GUI)
  (when (window-system)
    (setq org-startup-with-inline-images t))
  ;; limit images width
  (setq org-image-actual-width '(800))
  ;; org-doing settings
  (setq org-doing-file (aam/org-path "todo.org"))

  ;; delve
  (if (eq org-enable-delve t)
      (setq delve-storage-paths (aam/org-path "delve")))

  ;; org-ref configuration
  (setq org-ref-bibliography-notes (aam/org-path "papers.org")
        org-ref-notes-directory (aam/org-path "papers")
        org-ref-default-bibliography aam/bibtex-files
        org-ref-pdf-directory (aam/bib-path "papers/"))
  ;; org-cite configuration
  (setq org-cite-global-bibliography
        (ensure-list
         (or (bound-and-true-p citar-bibliography)
             (bound-and-true-p bibtex-completion-bibliography))))

  ;; setup org modules
  (my-org-setup/modules)

  ;; status line
  (spacemacs|diminish org-roam-ui-mode)
  (spacemacs|diminish org-roam-ui-follow-mode)
  (spacemacs|diminish org-remark-mode "" "Or")
  (spacemacs|diminish org-remark-global-tracking-mode))

(defun my-org-setup/modules()
  ;; additional org-modules
  ;; anki like functionality
  (require 'org-learn)
  (setq org-learn-always-reschedule t)
  ;; tracking habits
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit t)
  (setq org-habit-graph-column 80)
  ;; pomodoro and timer
  (add-to-list 'org-modules 'org-timer t)
  ;; org-effectiveness
  (add-to-list 'org-modules 'org-effectiveness t)
  ;; org-crypt
  (require 'org-crypt)
  (setq org-crypt-key "alexander.matyasko@gmail.com")
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt"))))

(provide 'config-org)
