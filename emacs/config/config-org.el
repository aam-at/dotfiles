;; -*- lexical-binding: t; -*-
;; This file configures org-mode for use.

(require 'aam-core)
(setq org-rating-guide (aam/org-path "templates/rating_guide.org"))
(setq org-gtd-trigger-list (aam/org-path "templates/trigger_list.org"))

(defconst aam/org-roam-bib-note-head "#+TITLE: ${title}\n#+STARTUP: latexpreview"
  "File header shared by the org-ref and Citar bibliography note templates.")

(defun aam/org-roam-bib-note-template (citekey)
  "Return the bibliography note body for the CITEKEY placeholder.
Fields come from Citar in both templates, so org-ref (\"r\") and
Citar (\"c\") notes are identical; missing fields expand to \"\"
instead of prompting."
  (let ((field (lambda (&rest fields)
                 (format "%%(citar-get-display-value '%S \"%s\")" fields citekey))))
    (concat "
* " (funcall field "year" "date") " - ${title}
:PROPERTIES:
:CREATED: %U
:Custom_ID: " citekey "
:AUTHOR: " (funcall field "author" "editor") "
:JOURNAL: " (funcall field "journaltitle" "journal" "booktitle") "
:YEAR: " (funcall field "year" "date") "
:DOI: " (funcall field "doi") "
:URL: " (funcall field "url") "
:MARKDOWN_DOCUMENT: %(aam/get-cite-markdown-filename \"" citekey "\")
:NOTER_DOCUMENT: %(aam/get-cite-pdf-filename \"" citekey "\")
:END:
[[file:%(aam/get-cite-pdf-filename \"" citekey "\")][pdf]] [[file:%(aam/get-cite-markdown-filename \"" citekey "\")][md]]
%?")))

(defun aam/org-ref-edit-note (keys)
  "Open the note for the first of KEYS, or create it via org-ref or Citar.
New notes show the capture template menu: \"r\" creates the note through
org-roam-bibtex, \"c\" through citar-org-roam."
  (let ((key (car keys)))
    (if (org-roam-node-from-ref (concat "@" key))
        (orb-edit-note key)
      (let* ((org-capture-templates org-roam-capture-templates)
             (template (org-capture-select-template)))
        (pcase (car-safe template)
          ("c" (citar-open-notes (list key)))
          ("r" (let ((org-roam-capture-templates (list template)))
                 (orb-edit-note key)))
          (_ (user-error "Abort")))))))

;; Read-only template views; `q' closes them (`view-mode').
(defun aam/org-show-rating-guide ()
  "Display the rating guide template."
  (interactive)
  (view-file-other-window org-rating-guide))

(defun aam/org-show-gtd-trigger-list ()
  "Display the GTD trigger list template."
  (interactive)
  (view-file-other-window org-gtd-trigger-list))

(defun aam/org-daily-journal-find-location ()
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

;; PERIOD -> (FILE-NAME-FORMAT TITLE-FORMAT), both for `format-time-string'.
(defconst aam/org-journal-periods
  '((weekly "%GW%V.org" "#+TITLE: Week %V, %G\n\n")
    (monthly "%GM%m.org" "#+TITLE: %B, %G\n\n")
    (yearly "%Y.org" "#+TITLE: %Y\n\n")))

;;;###autoload
(defun aam/org-journal-file (period)
  "Return the current PERIOD's journal file, e.g. journal/2026W37.org."
  (expand-file-name (format-time-string (car (alist-get period aam/org-journal-periods)))
                    (aam/org-path "journal/")))

;;;###autoload
(defun aam/org-journal-find-location (period)
  "Open the current PERIOD's journal file, creating it if needed, at its end.
PERIOD is a key of `aam/org-journal-periods'.  Used as an `org-capture' target."
  (let ((file (aam/org-journal-file period)))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert "#+TODO: TODO(t) NEXT(n) | DONE(d) FAILED(f)\n"
                (format-time-string (cadr (alist-get period aam/org-journal-periods))))))
    (find-file file)
    (goto-char (point-max))))

(defun aam/citar-dwim-at-bare-key (&rest _)
  "Run citar's default action on a bare @citekey at point.
Covers keys outside [cite:] syntax, e.g. in :ROAM_REFS:."
  (when (and (derived-mode-p 'org-mode)
             (thing-at-point-looking-at org-element-citation-key-re))
    (let ((key (match-string-no-properties 1)))
      (require 'citar)
      (when (citar-get-entry key)
        (citar-run-default-action (list key))
        t))))

;;;###autoload
(defun aam/org-setup ()
  ;; org settings
  ;; RET on a bare @citekey: Doom's RET is `+org/dwim-at-point', Spacemacs' is
  ;; `org-open-at-point'.
  (advice-add (if (fboundp '+org/dwim-at-point) '+org/dwim-at-point 'org-open-at-point)
              :before-until #'aam/citar-dwim-at-bare-key)
  (aam/configure-org-paths)
  ;; interactive dashboards for the five live PARA areas
  (add-to-list 'load-path (aam/org-path "scripts"))
  (require 'org-area-dashboard)
  (setq aam-org-area-dashboard-root org-directory)
  (aam/org-area-dashboard-setup)
  (setq aam/org-inbox (aam/org-path "inbox.org"))
  (setq deft-directory org-directory
        deft-recursive t
        deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\|journal\\|data\\)$")
  ;; set org-roam directory before loading agenda
  (setq org-default-notes-file (aam/org-path "refile.org"))
  (add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
  ;; vulpea settings
  (setq vulpea-db-sync-directories (list org-directory))
  (run-with-idle-timer 5 nil (lambda () (vulpea-db-autosync-mode +1)))
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
  (dolist (var '(org-download-image-dir org-download-heading-lvl org-attach-id-dir
					org-use-property-inheritance org-archive-location org-current-tag-alist))
    (put var 'safe-local-variable #'always))

  ;; sync buffers
  (add-hook 'org-mode-hook 'auto-revert-mode)

  ;; org appearance
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
  ;; Queues moved into the root index under `* Queues'; ideas moved to ideas/.
  (setq org-agenda-files (list (aam/org-path "inbox.org")
                               (aam/org-path "someday.org")
                               (aam/org-path "archived.org")
                               (aam/org-path "areas/work.org")
                               (aam/org-path "areas/personal.org")
                               (aam/org-path "areas/health.org")
                               (aam/org-path "areas/relationships.org")
                               (aam/org-path "areas/finance.org")))
  (setq aam/org-agenda-projects (aam/org-get-active-headline-files (aam/org-path "projects/index.org")))
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
    (aam/org-roam-get-filepath-for-title
     (aam/org-path "notes")
     (when (eq with-date t) 'aam/org-roam-get-filepath-with-date)))
  (defun aam/org-capture-note-filepath-with-date ()
    (aam/org-capture-note-filepath t))
  (defun aam/org-capture-project-filepath ()
    "Return path to structured note"
    (aam/org-roam-get-filepath-for-title (aam/org-path "projects")))
  (defun aam/org-capture-idea-filepath ()
    "Prompt for an idea title and return its `ideas/idea-<slug>.org' path."
    (setq aam/org-capture-title (read-string "Idea title: "))
    (expand-file-name
     (format "idea-%s.org"
             (aam/org-roam--title-to-slug aam/org-capture-title))
     (aam/org-path "ideas")))
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
    "Give the captured file a file-level ID property drawer."
    (save-excursion
      (goto-char (point-min))
      (org-id-get-create)))
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
          ("j" "Journal entry" plain (function aam/org-daily-journal-find-location)
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
           (function aam/org-daily-journal-find-location)
           (file ,(aam/org-path "templates/gratitude_pages.org"))
           :jump-to-captured t)
          ("sp" "Morning Pages Note" plain
           (function aam/org-daily-journal-find-location)
           (file ,(aam/org-path "templates/morning_pages.org"))
           :jump-to-captured t)
          ;; Snippets for planning and reviewing
          ("sd" "Daily Review" plain
           (function aam/org-daily-journal-find-location)
           (file ,(aam/org-path "templates/daily_review.org"))
           :jump-to-captured t)
          ("sw" "Weekly Plan" plain
           (function ,(apply-partially #'aam/org-journal-find-location 'weekly))
           (file ,(aam/org-path "templates/weekly_plan.org"))
           :jump-to-captured t)
          ("sW" "Weekly Review" plain
           (function ,(apply-partially #'aam/org-journal-find-location 'weekly))
           (file ,(aam/org-path "templates/weekly_review.org"))
           :jump-to-captured t)
          ("sm" "Monthly Plan" plain
           (function ,(apply-partially #'aam/org-journal-find-location 'monthly))
           (file ,(aam/org-path "templates/monthly_plan.org"))
           :jump-to-captured t)
          ("sM" "Monthly Review" plain
           (function ,(apply-partially #'aam/org-journal-find-location 'monthly))
           (file ,(aam/org-path "templates/monthly_review.org"))
           :jump-to-captured t)
          ("sy" "Yearly Plan" plain
           (function ,(apply-partially #'aam/org-journal-find-location 'yearly))
           (file ,(aam/org-path "templates/yearly_plan.org"))
           :jump-to-captured t)
          ("sY" "Yearly Review" plain
           (function ,(apply-partially #'aam/org-journal-find-location 'yearly))
           (file ,(aam/org-path "templates/yearly_review.org"))
           :jump-to-captured t)
          ;; Snippets for zettelkasten and PARA
          ("si" "Idea" plain
           (file aam/org-capture-idea-filepath)
           (file ,(aam/org-path "templates/idea.org"))
           :hook aam/org-roam-capture-finalize
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
          ;; PARA projects. Areas are a fixed set of five and are not captured:
          ;; templates/area.org stays for a deliberate restructure.
          ("sP" "Project" plain
           (file aam/org-capture-project-filepath)
           (file ,(aam/org-path "templates/project.org"))
           :hook aam/org-roam-capture-finalize
           :jump-to-captured t)))

  ;; defer babel language loading until ob is first used
  (with-eval-after-load 'ob
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
       (shell . t)
       (sql . nil)
       (sqlite . t)))
    (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
          org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"))

  (setq org-log-into-drawer "LOGBOOK")

  ;; org-roam settings
  (setq org-roam-file-exclude-regexp '("data" "templates" "archived" "drafts"))
  (setq org-roam-graph-exclude-matcher '("journal" "inbox.org"))
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-capture-templates
        `(("r" "Bibliography reference (org-ref)" plain
           ,(aam/org-roam-bib-note-template "%^{citekey}")
           :target (file+head "papers/${citekey}.org" ,aam/org-roam-bib-note-head)
           :empty-lines 1
           :unnarrowed t)
          ("c" "Bibliography reference (Citar)" plain
           ,(aam/org-roam-bib-note-template "${citar-citekey}")
           :target (file+head "papers/${citar-citekey}.org" ,aam/org-roam-bib-note-head)
           :empty-lines 1
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?" :target
           (file+head "${slug}.org" "#+title: ${title}")
           :unnarrowed t)))

  ;; org-roam-bibtex settings
  ;; Match citar-org-roam, which always records references as @citekey.
  (setq orb-roam-ref-format 'org-cite)
  (setq orb-preformat-keywords
        '("citekey" "date" "year" "type" "pdf?" "note?" "author" "editor"
          "journal" "url" "doi" "keywords"
          "author-abbrev" "editor-abbrev" "author-or-editor-abbrev"))
  (with-eval-after-load 'orb-note-actions
    (add-to-list 'orb-note-actions-user (cons "Open PDF file(s) externally" #'aam/open-pdf-external)))

  ;; configure org-journal
  (setq org-journal-dir (aam/org-path "journal/"))
  (setq org-journal-date-prefix "#+TODO: TODO(t) NEXT(n) | DONE(d) FAILED(f)\n* ")
  (setq org-journal-file-format "%Y%m%d.org"
        org-journal-enable-encryption nil
        org-journal-enable-cache t)
  (setq org-journal-agenda-days 7)
  (setq aam/journal-agenda-files (aam/org-journal-list-agenda-files org-journal-agenda-days))
  (setq org-agenda-files (append aam/journal-agenda-files org-agenda-files))

  ;; show images inline (only works in GUI)
  (when (window-system)
    (setq org-startup-with-inline-images t))
  ;; limit images width
  (setq org-image-actual-width '(800))
  ;; org-doing settings
  (setq org-doing-file (aam/org-path "todo.org"))

  ;; delve
  (setq delve-storage-paths (aam/org-path "delve"))

  ;; setup org modules
  (aam/org-setup-modules)

  ;; status line
  (when (fboundp 'diminish)
    (diminish 'org-roam-ui-mode)
    (diminish 'org-roam-ui-follow-mode)
    (diminish 'org-remark-mode "")
    (diminish 'org-remark-global-tracking-mode)))

(defun aam/org-setup-modules ()
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
