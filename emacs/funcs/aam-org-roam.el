;;; aam-org-roam.el --- Shared Org-roam helpers -*- lexical-binding: t; -*-

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

(provide 'aam-org-roam)
;;; aam-org-roam.el ends here
