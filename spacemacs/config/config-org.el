;; This file configures email for use.

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
  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max)))

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
          ("j" "Journal entry" plain (function org-journal-find-location)
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
          ("sm" "Morning Pages Note" plain
           (function org-journal-find-location)
           (file ,(aam/org-path "templates/morning_pages.org"))
           :jump-to-captured t)
          ("sr" "Daily Review Note" plain
           (function org-journal-find-location)
           (file ,(aam/org-path "templates/daily_review.org"))
           :jump-to-captured t)
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
          ("sp" "Project" plain
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

  ;; cdlatex for org-mode
  (spacemacs|diminish org-cdlatex-mode)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)

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
           (file+head "papers/${citekey}.org" "#+TITLE: ${title}\n")
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
