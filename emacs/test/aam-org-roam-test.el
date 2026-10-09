;;; aam-org-roam-test.el --- Checks for shared Org-roam helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'aam-org-roam)
;; Loaded up front: loading it mid-test would redefine a stubbed `compile'.
(require 'compile)

(defvar features)
(defvar org-roam-ui-app-build-dir)
(defvar org-roam-ui-mode)
(defvar org-roam-ui-port)


(ert-deftest aam/org-roam-find-forward-link-visits-an-outgoing-target ()
  (let ((source 'source)
        (target 'target)
        (unlinked 'unlinked)
        visited)
    (cl-letf (((symbol-function 'org-roam-node-at-point)
               (lambda (&optional assert)
                 (should assert)
                 source))
              ((symbol-function 'org-roam-node-id)
               (lambda (node) (symbol-name node)))
              ((symbol-function 'org-roam-db-query)
               (lambda (_query id)
                 (should (equal id "source"))
                 '(("target"))))
              ((symbol-function 'org-roam-node-read)
               (lambda (_initial filter _sort require-match prompt)
                 (should require-match)
                 (should (equal prompt "Forward link: "))
                 (should (funcall filter target))
                 (should-not (funcall filter unlinked))
                 target))
              ((symbol-function 'org-roam-node-visit)
               (lambda (node &rest _)
                 (setq visited node))))
      (aam/org-roam-find-forward-link)
      (should (eq visited target)))))

(ert-deftest aam/org-roam-ui-start-skips-an-occupied-port-pair ()
  (let ((org-roam-ui-mode nil)
        (org-roam-ui-port 35901)
        (aam/org-roam-ui-default-port 35901)
        (features (cons 'org-roam-ui features)) ; `require' then skips the load
        enabled-with)
    (cl-letf (((symbol-function 'aam/check-localhost-port)
               (lambda (port) (= port 35901)))
              ((symbol-function 'aam/org-roam-ui--enable-with-ports)
               (lambda (http-port websocket-port)
                 (setq enabled-with (list http-port websocket-port)
                       org-roam-ui-mode t))))
      (should (= 35902 (aam/org-roam-ui-start)))
      (should (equal enabled-with '(35902 35904))))))

(ert-deftest aam/zettel-search-labels-notes-once-across-searches ()
  (cl-letf (((symbol-function 'aam/zettel--call)
             (lambda (tool &rest args)
               (should (equal tool "search"))
               (should (eq (plist-get args :unlinked) t))
               `((results ((path . "notes/a.org") (title . "A, with a comma"))
                          ((path . ,(format "notes/%s.org" (plist-get args :by)))
                           (title . "B")))))))
    (should (equal (aam/zettel--search "q" '("name" "text") :unlinked t)
                   '(("A, with a comma  [notes/a.org]" . "notes/a.org")
                     ("B  [notes/name.org]" . "notes/name.org")
                     ("B  [notes/text.org]" . "notes/text.org"))))))

(ert-deftest aam/zettel-explore-keeps-unpicked-candidates-across-rounds ()
  (let ((label (lambda (p) (cons (format "%s  [%s]" p p) p)))
        searches offered)
    (cl-letf (((symbol-function 'aam/zettel--search)
               (lambda (query bys &rest _)
                 (push (cons query bys) searches)
                 (mapcar label (if (equal bys '("meaning")) '("hit1" "hit2") '("hit2" "hit3")))))
              ((symbol-function 'aam/zettel--neighbours)
               (lambda (_) (mapcar label '("nb1" "hit1" "hit3"))))
              ((symbol-function 'aam/zettel--pick)
               (lambda (_ cands)
                 (push (mapcar #'cdr cands) offered)
                 (and (null (cdr offered)) (list (funcall label "hit1")))))
              ((symbol-function 'find-file-noselect) (lambda (&rest _) (current-buffer)))
              ((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'org-mode) #'ignore)
              ((symbol-function 'gptel-mode) #'ignore)
              ((symbol-function 'gptel-context-remove-all) #'ignore)
              ((symbol-function 'gptel-add-file) #'ignore)
              ((symbol-function 'gptel-send) #'ignore))
      (save-window-excursion
        (aam/zettel-explore "how do a and b relate?" "a b"))
      (kill-buffer "*zettel-explore*")
      (should (equal (reverse searches)
                     '(("how do a and b relate?" "meaning") ("a b" "name" "text"))))
      (should (equal (reverse offered)
                     '(("hit1" "hit2" "hit3") ("nb1" "hit3" "hit2")))))))

(ert-deftest aam/zettel-findings-collect-both-paths-of-a-pair ()
  (cl-letf (((symbol-function 'aam/zettel--call)
             (lambda (&rest _)
               '((findings ((kind . "no-role") (path . "notes/a.org") (detail . "A"))
                           ((kind . "unlinked-similar") (path . "notes/a.org")
                            (other . "notes/b.org") (detail . "A <-> B")))))))
    (should (equal (aam/zettel--findings :kinds ["review"])
                   '(("no-role" ("notes/a.org") "A")
                     ("unlinked-similar" ("notes/a.org" "notes/b.org") "A <-> B"))))))

;;; Orphan nodes.

(ert-deftest aam/org-roam-insert-orphan-nodes-lists-each-node-as-a-link ()
  (let (sql)
    (cl-letf (((symbol-function 'org-roam-db-query)
               (lambda (query) (setq sql query) '(("id1" "One") ("id2" "Two")))))
      (with-temp-buffer
        (aam/org-roam-insert-orphan-nodes)
        (should (equal (buffer-string) "* [[id:id1][One]]\n* [[id:id2][Two]]\n"))
        (should (string-match-p "FROM nodes" sql))))))

;;; Org-roam UI.

(defun aam/test-slurp (file)
  "Contents of FILE."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest aam/org-roam-ui-web-build-rewrites-the-ports-once ()
  (let* ((root (file-name-as-directory (file-truename (make-temp-file "ui" t))))
         (source (expand-file-name "src/" root))
         (org-roam-ui-app-build-dir source)
         (aam/org-roam-ui-cache-directory (expand-file-name "cache/" root)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "sub" source) t)
          (write-region "ws localhost:35903 http localhost:35901 other localhost:35905"
                        nil (expand-file-name "index.html" source) nil 'silent)
          (write-region "fetch('localhost:35901')" nil (expand-file-name "sub/app.js" source)
                        nil 'silent)
          (write-region "localhost:35901" nil (expand-file-name "notes.txt" source) nil 'silent)
          (let* ((target (aam/org-roam-ui--web-build 4001 4003))
                 (page (expand-file-name "index.html" target)))
            (should (equal target (expand-file-name "org-roam-ui-4001-4003/"
                                                    aam/org-roam-ui-cache-directory)))
            (should (equal (aam/test-slurp page)
                           "ws localhost:4003 http localhost:4001 other localhost:35905"))
            (should (equal (aam/test-slurp (expand-file-name "sub/app.js" target))
                           "fetch('localhost:4001')"))
            (should (equal (aam/test-slurp (expand-file-name "notes.txt" target))
                           "localhost:35901"))
            (should (file-exists-p (expand-file-name ".aam-port-configured" target)))
            ;; A finished copy is reused...
            (write-region "edited" nil page nil 'silent)
            (should (equal (aam/org-roam-ui--web-build 4001 4003) target))
            (should (equal (aam/test-slurp page) "edited"))
            ;; ...until the source build is newer than it.
            (set-file-times source (time-add (current-time) 60))
            (aam/org-roam-ui--web-build 4001 4003)
            (should (string-match-p "localhost:4003" (aam/test-slurp page))))
          (let ((aam/org-roam-ui-cache-directory nil))
            (should-error (aam/org-roam-ui--web-build 1 3))))
      (delete-directory root t))))

(ert-deftest aam/org-roam-ui-enable-redirects-only-the-websocket-default ()
  (let ((org-roam-ui-port 1)
        (org-roam-ui-app-build-dir "orig/")
        ws-ports build-seen mode-calls)
    (cl-letf (((symbol-function 'websocket-server)
               (lambda (port &rest _) (push port ws-ports)))
              ((symbol-function 'aam/org-roam-ui--web-build)
               (lambda (http ws) (format "build-%d-%d/" http ws)))
              ((symbol-function 'org-roam-ui-mode)
               (lambda (arg)
                 (push arg mode-calls)
                 (when (eql arg 1)
                   (setq build-seen org-roam-ui-app-build-dir)
                   (websocket-server 35903 :host "x")
                   (websocket-server 9999)))))
      (should (aam/org-roam-ui--enable-with-ports 4001 4003))
      (should (= org-roam-ui-port 4001))
      (should (equal build-seen "build-4001-4003/"))
      (should (equal org-roam-ui-app-build-dir "orig/"))
      (should (equal (reverse ws-ports) '(4003 9999)))
      ;; The override is gone once startup returns.
      (websocket-server 35903)
      (should (equal (car ws-ports) 35903))
      (setq mode-calls nil)
      (cl-letf (((symbol-function 'org-roam-ui-mode)
                 (lambda (arg) (push arg mode-calls) (when (eql arg 1) (error "boom")))))
        (should-not (aam/org-roam-ui--enable-with-ports 4005 4007))
        (should (equal mode-calls '(-1 1)))))))

(ert-deftest aam/org-roam-ui-start-retries-reuses-and-gives-up ()
  (let ((org-roam-ui-mode nil)
        (org-roam-ui-port 35901)
        (aam/org-roam-ui-default-port nil)
        (features (cons 'org-roam-ui features))
        tried)
    (cl-letf (((symbol-function 'aam/check-localhost-port) #'ignore)
              ((symbol-function 'aam/org-roam-ui--enable-with-ports)
               (lambda (http _ws)
                 (push http tried)
                 (when (= http 35902) (setq org-roam-ui-mode t))))) ; fails on 35901
      (should (= 35902 (aam/org-roam-ui-start)))
      (should (equal (reverse tried) '(35901 35902)))
      (setq tried nil)
      (should (= 35901 (aam/org-roam-ui-start))) ; already running: its own port
      (should-not tried)
      (setq org-roam-ui-mode nil)
      (cl-letf (((symbol-function 'aam/org-roam-ui--enable-with-ports) #'ignore))
        (should-error (aam/org-roam-ui-start) :type 'user-error))
      (let ((aam/org-roam-ui-default-port 70000))
        (should-error (aam/org-roam-ui-start))))))

;;; Zettel chats: shared helpers, connect, ask, garden.

(defmacro aam/test-recording-gptel (&rest body)
  "Run BODY with gptel and window calls stubbed; they are logged on `calls'."
  `(let (calls)
     (cl-letf (((symbol-function 'gptel-context-remove-all) (lambda () (push '(clear) calls)))
               ((symbol-function 'gptel-add-file) (lambda (f) (push (list 'add f) calls)))
               ((symbol-function 'gptel-send) (lambda () (push (list 'send (buffer-name)) calls)))
               ((symbol-function 'gptel-mode) #'ignore)
               ((symbol-function 'org-mode) #'ignore)
               ((symbol-function 'find-file-noselect)
                (lambda (f &rest _) (push (list 'visit f) calls) (generate-new-buffer " *note*")))
               ((symbol-function 'display-buffer)
                (lambda (b &rest _) (push (list 'show (buffer-name b)) calls)))
               ((symbol-function 'balance-windows) #'ignore)
               ((symbol-function 'delete-other-windows) #'ignore)
               ((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b))))
       ,@body)))

(ert-deftest aam/zettel-attach-adds-to-gptel-then-shows-the-note ()
  (aam/test-recording-gptel
   (let* ((file (aam/org-path "n/a.org"))
          (buf (aam/zettel--attach "n/a.org")))
     (should (buffer-live-p buf))
     (should (equal (reverse calls)
                    `((add ,file) (visit ,file) (show ,(buffer-name buf)))))
     (setq calls nil)
     (aam/zettel--show "n/b.org")
     (should (equal (mapcar #'car (reverse calls)) '(visit show))))))

(ert-deftest aam/zettel-chat-seeds-a-gptel-buffer ()
  (aam/test-recording-gptel
   (let ((chat (aam/zettel--chat "*t-chat*" "SYS" "* Q\n")))
     (unwind-protect
         (progn
           (should (equal (buffer-local-value 'gptel-system-prompt chat) "SYS"))
           (should (equal (with-current-buffer chat (buffer-string)) "* Q\n")))
       (kill-buffer chat)))))

(ert-deftest aam/zettel-ask-attaches-the-files-and-sends-from-the-chat ()
  (aam/test-recording-gptel
   (aam/zettel--ask "*t-ask*" "SYS" '("a.org" "b.org") "PROMPT")
   (unwind-protect
       (progn
         (should (equal (mapcar #'car (reverse calls))
                        '(clear add visit show add visit show send)))
         (should (equal (cadr (assq 'send calls)) "*t-ask*"))
         (should (equal (with-current-buffer "*t-ask*" (buffer-string)) "PROMPT")))
     (kill-buffer "*t-ask*"))))

(ert-deftest aam/zettel-connect-asks-about-the-picked-notes-with-their-ids ()
  (let* ((aam/org-root (file-name-as-directory (file-truename (make-temp-file "org" t))))
         (file (expand-file-name "n/a.org" aam/org-root))
         searched)
    (make-directory (file-name-directory file))
    (unwind-protect
        (aam/test-recording-gptel
         (cl-letf (((symbol-function 'aam/zettel--search)
                    (lambda (&rest args) (setq searched args) '(("B  [n/b.org]" . "n/b.org"))))
                   ((symbol-function 'aam/zettel--pick) (lambda (_prompt cands) cands))
                   ((symbol-function 'org-id-get) (lambda (&rest _) "ID-B")))
           (with-temp-buffer
             (setq buffer-file-name file)
             (set-buffer-modified-p t)
             (aam/zettel-connect)
             (should (equal searched '("n/a.org" ("similar") :unlinked t :scope "notes")))
             (let ((added (cl-loop for c in (reverse calls)
                                   when (eq (car c) 'add) collect (cadr c))))
               (should (equal added (list file (aam/org-path "n/b.org")))))
             (should (equal (car calls) '(send "*zettel-connect*")))
             (let ((text (with-current-buffer "*zettel-connect*" (buffer-string))))
               (should (string-match-p "Candidates (all attached):\n- b: id:ID-B\n" text)))
             (kill-buffer "*zettel-connect*")
             (cl-letf (((symbol-function 'aam/zettel--pick) (lambda (&rest _) nil)))
               (should-error (aam/zettel-connect) :type 'user-error)))
           (with-temp-buffer
             (should-error (aam/zettel-connect) :type 'user-error))))
      (delete-directory aam/org-root t))))

(ert-deftest aam/zettel-garden-prompts-cover-every-finding-kind ()
  (should (= (length aam/zettel-garden-prompts) 8))
  (dolist (kind '("orphan" "no-backlinks" "no-links-out" "bare-links" "long-atomic"
                  "no-role" "unlinked-similar" "unlinked-across-topics"))
    (should (stringp (cdr (assoc kind aam/zettel-garden-prompts)))))
  (should (equal (cdr (assoc "orphan" aam/zettel-garden-prompts))
                 (cdr (assoc "no-backlinks" aam/zettel-garden-prompts)))))

(ert-deftest aam/zettel-garden-open-and-ask-use-the-finding-at-point ()
  (let ((item '("no-role" ("notes/a.org") "A"))
        shown asked)
    (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () item))
              ((symbol-function 'aam/zettel--show) (lambda (p) (push p shown)))
              ((symbol-function 'aam/zettel--ask) (lambda (&rest args) (setq asked args))))
      (aam/zettel-garden-open)
      (should (equal shown '("notes/a.org")))
      (aam/zettel-garden-ask)
      (should (equal (nth 2 asked) '("notes/a.org")))
      (should (string-match-p "\\`\\* no-role: notes/a.org\nA\n\nPropose one role tag"
                              (nth 3 asked)))
      (setq item '("unheard-of" ("x.org") "d"))
      (aam/zettel-garden-ask)
      (should (string-match-p "Explain this finding" (nth 3 asked)))
      (setq item nil)
      (should-error (aam/zettel-garden-open) :type 'user-error)
      (should-error (aam/zettel-garden-ask) :type 'user-error))))

(ert-deftest aam/zettel-garden-lists-the-findings-and-binds-its-keys ()
  (let (asked)
    (cl-letf (((symbol-function 'aam/zettel--findings)
               (lambda (&rest args)
                 (push args asked)
                 '(("no-role" ("n/a.org") "A")
                   ("unlinked-similar" ("n/a.org" "n/b.org") "A <-> B"))))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (aam/zettel-garden)
      (aam/zettel-garden t)
      (should (equal (reverse asked)
                     '((:kinds ["review"] :limit 40)
                       (:kinds ["errors" "review"] :limit 40))))
      (with-current-buffer "*zettel-garden*"
        (unwind-protect
            (progn
              (should (derived-mode-p 'aam/zettel-garden-mode))
              (should (equal (mapcar #'cadr tabulated-list-entries)
                             '(["no-role" "n/a.org" "A"]
                               ["unlinked-similar" "n/a.org" "A <-> B"])))
              (should (eq (lookup-key aam/zettel-garden-mode-map (kbd "RET"))
                          #'aam/zettel-garden-open))
              (should (eq (lookup-key aam/zettel-garden-mode-map "a")
                          #'aam/zettel-garden-ask)))
          (kill-buffer))))))

;;; Paper review. The skill's contract cases are the vault's
;;; skills/add-org-paper-with-review/evals/evals.json; its cases with picks are
;;; run here too, so one expected command checks both front-ends.

(defun aam/test-paper-review (target read)
  "Run `aam/org-paper-review' on TARGET in a scratch vault; return its command.
The choices offered are the real script's. READ answers each
`completing-read' from its prompt, candidates and default."
  (let* ((catalog (aam/org-paper-review--catalog))
         (aam/org-root (file-name-as-directory (file-truename (make-temp-file "org" t))))
         (file (expand-file-name target aam/org-root))
         command)
    (make-directory (file-name-directory file) t)
    (unwind-protect
        (cl-letf (((symbol-function 'aam/org-paper-review--catalog) (lambda () catalog))
                  ((symbol-function 'completing-read)
                   (lambda (prompt collection _pred _match _init _hist def)
                     (funcall read prompt collection def)))
                  ((symbol-function 'shell-quote-argument) #'identity)
                  ((symbol-function 'executable-find) #'identity)
                  ((symbol-function 'compile)
                   (lambda (cmd) (setq command cmd) (current-buffer))))
          (with-current-buffer (find-file-noselect file)
            (call-interactively #'aam/org-paper-review)
            (kill-buffer))
          command)
      (delete-directory aam/org-root t))))

(ert-deftest aam/org-paper-review-matches-the-skill-contract ()
  (let ((cases (aam/org-path "skills/add-org-paper-with-review/evals/evals.json"))
        (checked 0)
        harness model)
    (skip-unless (file-exists-p cases))
    (dolist (case (alist-get 'evals (with-temp-buffer
                                      (insert-file-contents cases)
                                      (json-parse-buffer :object-type 'alist :array-type 'list
                                                         :null-object nil :false-object nil))))
      (let-alist case
        (when .picks
          (cl-incf checked)
          (ert-info ((format "contract case %s" .name))
            (should
             (equal (aam/test-paper-review
                     .picks.target
                     (lambda (prompt collection def)
                       (let ((pick (pcase prompt
                                     ("Harness: " .picks.harness)
                                     ("Model: " .picks.model)
                                     ("Reasoning effort: " .picks.effort))))
                         ;; What the skill is told to pass, this command offers.
                         (when pick (should (member pick collection)))
                         (pcase prompt
                           ("Harness: " (setq harness (or pick def)))
                           ("Model: " (setq model (or pick def))))
                         (or pick def))))
                    ;; A value the case leaves open is the default here, as it
                    ;; is the running harness's default for the skill.
                    (string-replace
                     "{harness}" harness
                     (string-replace "{model}" model .expected.command))))))))
    (should (> checked 0))))

(ert-deftest aam/org-paper-review-saves-runs-and-reverts-the-note ()
  (let* ((aam/org-root (file-name-as-directory (file-truename (make-temp-file "org" t))))
         (file (expand-file-name "papers/key2026.org" aam/org-root))
         (out (generate-new-buffer " *review*"))
         command)
    (make-directory (file-name-directory file))
    (cl-letf (((symbol-function 'shell-quote-argument) #'identity)
              ((symbol-function 'executable-find) #'identity)
              ((symbol-function 'compile) (lambda (cmd) (setq command cmd) out)))
      (with-temp-buffer
        (should-error (aam/org-paper-review "codex" "m" "low") :type 'user-error))
      (with-current-buffer (find-file-noselect file)
        (insert "old")
        (aam/org-paper-review "codex" "m" "low")
        (should (equal command
                       (concat "python3 scripts/add_org_paper.py --review papers/key2026.org"
                               " --harness codex --model m --effort low")))
        (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string))
                       "old"))
        ;; The script rewrites the note on disk; the finish hook shows it.
        (write-region "new" nil file nil 'silent)
        (with-current-buffer out
          (run-hook-with-args 'compilation-finish-functions out "finished\n"))
        (should (equal (buffer-string) "new"))
        (kill-buffer)))
    (kill-buffer out)
    (delete-directory aam/org-root t)))

;;; aam-org-roam-test.el ends here
