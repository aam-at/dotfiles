;;; aam-org-roam.el --- Shared Org-roam helpers -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'aam-core)

(defun aam/org-roam-toggle-properties ()
  "Fold all property drawers, or unfold them when the first one is folded."
  (interactive)
  (if (save-excursion
        (goto-char (point-min))
        (and (re-search-forward org-property-start-re nil t)
             (org-fold-folded-p (line-end-position) 'drawer)))
      (org-fold-show-all '(drawers))
    (org-fold-hide-drawer-all)))

(defun aam/org-roam-orphan-nodes-by-id ()
  "Return a list of all orphan nodes in `org-roam`."
  (org-roam-db-query "SELECT
id, title
FROM nodes
WHERE id NOT IN (
                  SELECT DISTINCT n.id
                  FROM nodes n
                  LEFT OUTER JOIN links l ON n.id = l.source OR n.id = l.dest
                  WHERE l.type LIKE '%%id%%'
                  )"))

(defun aam/org-roam-insert-orphan-nodes ()
  "Insert all orphan nodes in `org-roam' in the current buffer."
  (interactive)
  (let* ((orphans (aam/org-roam-orphan-nodes-by-id)))
    (dolist (orphan orphans)
      (let ((id (car orphan))
            (title (cadr orphan)))
        (insert "* ")
        (insert (org-link-make-string (concat "id:" id) title))
        (insert "\n")))))

(defun aam/org-roam-find-forward-link ()
  "Select and visit a node linked from the Org-roam node at point."
  (interactive)
  (let* ((source (org-roam-node-at-point t))
         (ids (mapcar
               #'car
               (org-roam-db-query
                [:select :distinct [dest]
			 :from links
			 :where (= source $s1)
			 :and (= type "id")]
                (org-roam-node-id source)))))
    (unless ids
      (user-error "There are no forward links from the current note"))
    (org-roam-node-visit
     (org-roam-node-read
      nil
      (lambda (node)
        (member (org-roam-node-id node) ids))
      nil t "Forward link: "))))


(defcustom aam/org-roam-ui-port-search-limit 100
  "Number of HTTP ports to try when starting Org-roam UI."
  :type 'integer
  :group 'org-roam)

(defvar aam/org-roam-ui-cache-directory nil
  "Directory for port-specific Org-roam UI web builds.")

(defvar aam/org-roam-ui-default-port nil
  "Preferred HTTP port for the current Emacs profile.")

(defvar aam/org-roam-ui-websocket-port nil
  "WebSocket port selected for the current Org-roam UI session.")

(defvar aam/org-roam-ui-original-app-build-dir nil
  "Unmodified Org-roam UI web build used to make port-specific copies.")

(defun aam/org-roam-ui--web-build-for-ports (http-port websocket-port)
  "Return an Org-roam UI web build configured for HTTP-PORT and WEBSOCKET-PORT."
  (unless aam/org-roam-ui-cache-directory
    (error "`aam/org-roam-ui-cache-directory' is not configured"))
  (let* ((source (or aam/org-roam-ui-original-app-build-dir
                     (setq aam/org-roam-ui-original-app-build-dir
                           org-roam-ui-app-build-dir)))
         (target (expand-file-name
                  (format "org-roam-ui-%d-%d/" http-port websocket-port)
                  aam/org-roam-ui-cache-directory))
         (marker (expand-file-name ".aam-port-configured" target)))
    (when (or (not (file-exists-p marker))
              (file-newer-than-file-p source marker))
      ;; A marker is written only after the complete copy is usable.  An
      ;; interrupted rebuild is therefore retried on the next startup.
      (when (file-exists-p target)
        (delete-directory target t))
      (make-directory target t)
      (copy-directory source target nil t t)
      (dolist (file (directory-files-recursively
                     target "\\.\\(?:html\\|js\\)\\'"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (search-forward "localhost:35901" nil t)
            (replace-match (format "localhost:%d" http-port) t t))
          (goto-char (point-min))
          (while (search-forward "localhost:35903" nil t)
            (replace-match (format "localhost:%d" websocket-port) t t))
          (write-region (point-min) (point-max) file nil 'silent)))
      (write-region "" nil marker nil 'silent))
    target))

(defun aam/org-roam-ui--enable-with-ports (http-port websocket-port)
  "Enable Org-roam UI with HTTP-PORT and WEBSOCKET-PORT."
  (require 'cl-lib)
  (let ((websocket-server-function (symbol-function 'websocket-server)))
    (setq org-roam-ui-port http-port
          aam/org-roam-ui-websocket-port websocket-port
          org-roam-ui-app-build-dir
          (aam/org-roam-ui--web-build-for-ports http-port websocket-port))
    ;; Org-roam UI currently passes its hard-coded default to
    ;; `websocket-server'.  Keep the override local to mode startup rather than
    ;; advising every WebSocket server in the Emacs process.
    (cl-letf (((symbol-function 'websocket-server)
               (lambda (port &rest args)
                 (apply websocket-server-function
                        (if (= port 35903) websocket-port port)
                        args))))
      (org-roam-ui-mode 1))))

(defun aam/org-roam-ui-start ()
  "Start Org-roam UI on the first free localhost HTTP/WebSocket port pair.

Return the selected HTTP port.  When called interactively while the mode is
already active, open the existing UI instead."
  (interactive)
  (require 'org-roam-ui)
  (if org-roam-ui-mode
      (progn
        (when (called-interactively-p 'interactive)
          (org-roam-ui-open))
        org-roam-ui-port)
    (let ((initial-port (or aam/org-roam-ui-default-port org-roam-ui-port))
          (attempt 0)
          selected-port)
      (unless (and (integerp initial-port)
                   (<= 1 initial-port 65533))
        (error "Invalid Org-roam UI HTTP port: %S" initial-port))
      (while (and (not selected-port)
                  (< attempt aam/org-roam-ui-port-search-limit))
        (let* ((http-port (+ initial-port attempt))
               (websocket-port (+ http-port 2)))
          (when (> websocket-port 65535)
            (setq attempt aam/org-roam-ui-port-search-limit))
          (unless (or (> websocket-port 65535)
                      (aam/check-localhost-port http-port)
                      (aam/check-localhost-port websocket-port))
            (condition-case err
                (progn
                  (aam/org-roam-ui--enable-with-ports http-port websocket-port)
                  (setq selected-port http-port)
                  (message "Org-roam UI started on http://localhost:%d" http-port))
              (error
               ;; Also run cleanup after partial initialization, where the mode
               ;; variable may still be nil but a server process already exists.
               (ignore-errors (org-roam-ui-mode -1))
               (message "Org-roam UI port %d unavailable: %s"
                        http-port (error-message-string err))))))
        (setq attempt (1+ attempt)))
      (unless selected-port
        (user-error "Org-roam UI could not find a free port after %d attempts"
                    aam/org-roam-ui-port-search-limit))
      (when (called-interactively-p 'interactive)
        (org-roam-ui-open))
      selected-port)))


;;; Zettel explore: interactive version of the zettel-explore skill.

(defvar crm-separator)
(autoload 'gptel-context-remove-all "gptel-context")

(defvar aam/zettel--server nil "Long-lived `org-cli mcp' process; see `aam/zettel--call'.")
(defvar aam/zettel--id 0 "Id of the last request sent to `aam/zettel--server'.")

(defun aam/zettel--call (tool &rest args)
  "Call org-cli TOOL with keyword ARGS; return its JSON result as an alist.
One `org-cli mcp' process serves every call: a fresh org-cli parses the vault
and loads the embedding model each time (4-14s per call), a warm one answers in
0.3-2s. Kill the \" *org-cli*\" buffer to restart it."
  (unless (and (process-live-p aam/zettel--server)
               (buffer-live-p (process-buffer aam/zettel--server)))
    (setq aam/zettel--server
          (let ((default-directory aam/org-root))
            (make-process :name "org-cli" :buffer (get-buffer-create " *org-cli*")
                          :command '("org-cli" "--readonly" "mcp")
                          :stderr (get-buffer-create " *org-cli-stderr*")
                          :coding 'utf-8 :connection-type 'pipe :noquery t))))
  (let ((id (cl-incf aam/zettel--id)) reply)
    (with-current-buffer (process-buffer aam/zettel--server)
      (process-send-string
       aam/zettel--server
       (concat (json-serialize `(:jsonrpc "2.0" :id ,id :method "tools/call"
                                 :params (:name ,tool :arguments ,args)))
               "\n"))
      ;; One JSON reply per line; skip any left over from a call quit with C-g.
      (while (not reply)
        (goto-char (point-min))
        (if (search-forward "\n" nil t)
            (let ((msg (ignore-errors
                         (json-parse-string (delete-and-extract-region (point-min) (point))
                                            :object-type 'alist :array-type 'list
                                            :null-object nil :false-object nil))))
              (when (eql (alist-get 'id msg) id) (setq reply msg)))
          (unless (accept-process-output aam/zettel--server 120)
            (error "org-cli: no reply to %s" tool)))))
    (let-alist reply
      (let ((text (alist-get 'text (car .result.content))))
        (when (or .error .result.isError)
          (error "org-cli %s: %s" tool (or .error.message text)))
        (json-parse-string text :object-type 'alist :array-type 'list
                           :null-object nil :false-object nil)))))

(defun aam/zettel--label (note)
  "Completion candidate (LABEL . PATH) for NOTE, an alist with a path and a title."
  (let-alist note (cons (format "%s  [%s]" .title .path) .path)))

(defun aam/zettel--search (query bys &rest args)
  "Alist (LABEL . PATH) from org-cli searches for QUERY, one per BYS.
ARGS are more search arguments."
  (let (res)
    (dolist (by bys)
      (dolist (note (alist-get 'results (apply #'aam/zettel--call "search"
                                               :query query :by by :limit 10 args)))
        (cl-pushnew (aam/zettel--label note) res :test #'equal)))
    (nreverse res)))

(defun aam/zettel--neighbours (path)
  "Alist (LABEL . PATH) of notes PATH links to or is linked from."
  (let-alist (aam/zettel--call "links" :note path)
    (mapcar #'aam/zettel--label (append .backward .forward))))

(defun aam/zettel--pick (prompt cands)
  "Read several of CANDS, an alist keyed by label; return those entries.
Titles contain commas, so | separates the picks."
  (let ((crm-separator "[ \t]*|[ \t]*"))
    (delq nil (mapcar (lambda (label) (assoc label cands))
                      (completing-read-multiple (concat prompt ": ") cands)))))

;;;###autoload
(defun aam/zettel-explore (question terms)
  "Explore the vault for QUESTION: open notes, follow links, chat with the LLM.
QUESTION is searched by meaning; TERMS, a few key words, by name and text
\(text needs every word to appear, so a whole question finds nothing).
Each round pick notes (separated by |); the notes open for reading and
join the gptel context, and their neighbours join the candidates. Empty
pick ends the loop."
  (interactive "sQuestion: \nsKey terms for name and text search (empty to skip): ")
  (let* ((cands (or (delete-dups
                     (append (aam/zettel--search question '("meaning"))
                             (unless (string-empty-p terms)
                               (aam/zettel--search terms '("name" "text")))))
                    (user-error "No notes match %S" question)))
         (chat (generate-new-buffer "*zettel-explore*"))
         (seen nil))
    (with-current-buffer chat
      (org-mode) (gptel-mode 1)
      (setq-local gptel-system-prompt
                  "Answer from the attached notes only, citing each note by title and path. Shape: Answer (2-5 sentences); What the notes say (bullets, each citing a note); Connections; Inference (marked as yours); Gaps. Suggest which linked notes to open next.")
      (insert "* " question "\n"))
    (delete-other-windows)
    (switch-to-buffer chat)
    (gptel-context-remove-all)
    (while cands
      (let ((paths (mapcar #'cdr (aam/zettel--pick "Open notes, empty to stop" cands))))
        (if (null paths)
            (setq cands nil)
          (setq seen (append paths seen))
          (dolist (p paths)
            (let ((file (aam/org-path p)))
              (display-buffer (find-file-noselect file) '(display-buffer-pop-up-window))
              (gptel-add-file file)))
          (balance-windows)
          (setq cands (cl-remove-if (lambda (c) (member (cdr c) seen))
                                    (delete-dups (append (mapcan #'aam/zettel--neighbours paths)
                                                         cands)))))))
    (pop-to-buffer chat)
    (when seen (gptel-send))))

;;;###autoload
(defun aam/zettel-connect ()
  "Suggest linking sentences between the current note and unlinked similar notes.
Pick candidates; they open beside the note and the LLM drafts one sentence per
link, with the target's id. Copy accepted sentences into the note yourself."
  (interactive)
  (unless buffer-file-name (user-error "Not visiting a note"))
  (save-buffer)
  (let* ((note (current-buffer))
         (picks (or (aam/zettel--pick
                     "Link candidates"
                     (aam/zettel--search (file-relative-name buffer-file-name aam/org-root)
                                         '("similar") :unlinked t :scope "notes"))
                    (user-error "No link candidates chosen")))
         (chat (generate-new-buffer "*zettel-connect*"))
         (targets nil))
    (gptel-context-remove-all)
    (gptel-add-file buffer-file-name)
    (delete-other-windows)
    (dolist (pick picks)
      (let* ((file (aam/org-path (cdr pick)))
             (buf (find-file-noselect file)))
        (display-buffer buf '(display-buffer-pop-up-window))
        (gptel-add-file file)
        (push (format "- %s: id:%s" (file-name-base file)
                      (with-current-buffer buf (org-with-point-at 1 (org-id-get))))
              targets)))
    (balance-windows)
    (with-current-buffer chat
      (org-mode) (gptel-mode 1)
      (setq-local gptel-system-prompt
                  "A link is a claim about how two ideas relate. For each candidate, name the relationship from the first note to it (supports, contradicts, specializes, generalizes, explains, provides evidence for, operationalizes, mitigates, exposes a limitation of, motivates) or reject it with a reason. Links point to the note that owns the concept, at most five. For each accepted link write ONE sentence for the first note that states why, containing [[id:ID][words from the sentence]], using only the ids given.")
      (insert "* Connect " (buffer-name note) "\nCandidates (all attached):\n"
              (mapconcat #'identity (nreverse targets) "\n") "\n"))
    (pop-to-buffer chat)
    (gptel-send)))

(defun aam/zettel--ask (name system files prompt)
  "Open FILES (vault paths), attach them to gptel, and ask PROMPT in a new chat buffer."
  (gptel-context-remove-all)
  (dolist (f files)
    (display-buffer (find-file-noselect (aam/org-path f)) '(display-buffer-pop-up-window))
    (gptel-add-file (aam/org-path f)))
  (balance-windows)
  (let ((chat (generate-new-buffer name)))
    (with-current-buffer chat
      (org-mode) (gptel-mode 1)
      (setq-local gptel-system-prompt system)
      (insert prompt))
    (pop-to-buffer chat)
    (gptel-send)))

(defun aam/zettel--findings (&rest args)
  "List of (KIND PATHS DETAIL) from `org-cli check' over notes with keyword ARGS."
  (mapcar (lambda (f) (let-alist f (list .kind (delq nil (list .path .other)) .detail)))
          (alist-get 'findings (apply #'aam/zettel--call "check" :scope "notes" args))))

;;; Zettel garden: findings list from `org-cli check', judged one at a time.

(defconst aam/zettel-garden-prompts
  '(("orphan" . "Is this note worth linking from others? Say which kinds of notes should link to it and why, or recommend leaving it: an honest orphan beats a padded link.")
    ("no-backlinks" . "Is this note worth linking from others? Say which kinds of notes should link to it and why, or recommend leaving it: an honest orphan beats a padded link.")
    ("no-links-out" . "Is this a reference note (may link nowhere) or an atomic claim that should say what it builds on? If the latter, say what it builds on.")
    ("bare-links" . "For each link-only list item, write the clause saying why the link is there and show the sentence it belongs in, or recommend deleting it.")
    ("long-atomic" . "Is this one claim defended at length (leave it and say so) or several claims? If several, propose a split: one #L3 note per claim, the original becoming a short #L2 topic note that routes to them.")
    ("no-role" . "Propose one role tag with a reason: #L1 map, #L2 topic, #L3 atomic, #L4 leaf, reference.")
    ("unlinked-similar" . "Do the two notes make different claims (recommend a link and name the relationship) or the same claim (duplicates: propose merging the weaker into the stronger)?")
    ("unlinked-across-topics" . "This is raw material for ideas, not a defect. Name the claim the two notes make together, if any, and suggest M-x aam/zettel-ideate."))
  "Judging question per `org-cli check' finding kind.")

(defvar aam/zettel-garden-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'aam/zettel-garden-open)
    (define-key m (kbd "a") #'aam/zettel-garden-ask)
    m))

;; Evil's normal state shadows the mode map (a is `evil-append', RET `evil-ret').
(with-eval-after-load 'evil
  (evil-define-key* '(normal motion) aam/zettel-garden-mode-map
    (kbd "RET") #'aam/zettel-garden-open "a" #'aam/zettel-garden-ask))

(define-derived-mode aam/zettel-garden-mode tabulated-list-mode "Zettel-Garden"
  "Findings from `org-cli check'. RET opens the note(s), a asks the LLM to judge."
  (setq tabulated-list-format [("Kind" 22 t) ("Note" 60 t) ("Detail" 0 t)])
  (tabulated-list-init-header))

(defun aam/zettel-garden--item ()
  "Finding (KIND PATHS DETAIL) at point: its row's id, which survives sorting."
  (or (tabulated-list-get-id) (user-error "No finding at point")))

(defun aam/zettel-garden-open ()
  "Open the notes of the finding at point."
  (interactive)
  (dolist (p (nth 1 (aam/zettel-garden--item)))
    (display-buffer (find-file-noselect (aam/org-path p)) '(display-buffer-pop-up-window))))

(defun aam/zettel-garden-ask ()
  "Ask the LLM to judge the finding at point."
  (interactive)
  (pcase-let ((`(,kind ,paths ,detail) (aam/zettel-garden--item)))
    (aam/zettel--ask
     "*zettel-garden-chat*"
     "You advise on tidying an Org Zettelkasten. Findings are prompts to judge; the owner decides. Do not rewrite whole notes: give the proposed edits as short text."
     paths
     (format "* %s: %s\n%s\n\n%s\n" kind (string-join paths " + ") detail
             (or (cdr (assoc kind aam/zettel-garden-prompts))
                 "Explain this finding and suggest a fix.")))))

;;;###autoload
(defun aam/zettel-garden (&optional with-errors)
  "List `org-cli check' review findings for judging; with prefix, defects too."
  (interactive "P")
  (message "zettel-garden: checking...")
  (let ((items (aam/zettel--findings :kinds (if with-errors ["errors" "review"] ["review"])
                                     :limit 40)))
    (with-current-buffer (get-buffer-create "*zettel-garden*")
      (aam/zettel-garden-mode)
      (setq tabulated-list-entries
            (cl-loop for item in items
                     for (kind paths detail) = item
                     collect (list item (vector kind (car paths) detail))))
      (tabulated-list-print)
      (pop-to-buffer (current-buffer)))))

;;; Zettel ideate: ideas from pairs of notes that should be read together.

;;;###autoload
(defun aam/zettel-ideate (seed)
  "Draft research ideas from pairs of unlinked notes.
SEED is a vault note path, a topic, or empty for cross-topic pairs."
  (interactive
   (list (read-string "Seed note path or topic (empty = cross-topic pairs): "
                      (and buffer-file-name (file-in-directory-p buffer-file-name aam/org-root)
                           (file-relative-name buffer-file-name aam/org-root)))))
  (message "zettel-ideate: finding pairs...")
  (let* ((note (cond ((string-empty-p seed) nil)
                     ((string-suffix-p ".org" seed) seed)
                     ((cdar (aam/zettel--search seed '("meaning") :scope "notes")))
                     (t (user-error "No note matches %S" seed))))
         ;; Each pair is (LABEL PATH-A PATH-B).
         (pairs (if note
                    (mapcar (lambda (h) (list (car h) note (cdr h)))
                            (aam/zettel--search note '("similar") :unlinked t))
                  (cl-loop for (_ paths detail) in (aam/zettel--findings
                                                    :kinds ["unlinked-across-topics" "unlinked-similar"]
                                                    :limit 10)
                           when (cdr paths) collect (cons detail paths))))
         (chosen (if pairs
                     (aam/zettel--pick "Pairs (a few)" pairs)
                   (user-error "No candidate pairs"))))
    (when chosen
      (aam/zettel--ask
       "*zettel-ideate*"
       "An idea is a claim no single note makes: it follows from two notes read together. It names a specific claim and a test; a topic is not an idea. Drop an idea that restates one of its notes. Lenses: transfer (A's method solves B's problem), mechanism (A's mechanism explains B's result), contradiction (what evidence would settle it?), conjunction (if both hold, what else must be true?)."
       (seq-uniq (apply #'append (mapcar #'cdr chosen)))
       (concat "* Ideas from these pairs\n"
               (mapconcat (lambda (c) (format "- %s (%s)" (car c) (string-join (cdr c) " + "))) chosen "\n")
               "\n\nFor each pair, write the central claim of each note in one sentence, then present up to two ideas in this form, ranked by novelty then testability:\n"
               "### <one-sentence claim>\n- From: <Title (path)>: <its claim>; <Title (path)>: <its claim>\n- Lens: transfer | mechanism | contradiction | conjunction\n- Why it follows: <one or two sentences>\n- Cheapest test: <what to run or read>; refuted if <result>\n"
               "End with one line per dropped pair and why. Use the full paths given.\n")))))

(provide 'aam-org-roam)
;;; aam-org-roam.el ends here
