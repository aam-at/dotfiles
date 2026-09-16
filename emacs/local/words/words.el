;;; words.el --- Contextual text actions and research search  -*- lexical-binding: t; -*-

;; Copyright (C) 2014, 2015 John Kitchin
;; Refactored for Emacs 31.1 in 2026.
;;
;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Version: 2.0.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: convenience, hypermedia, research, tools
;; URL: https://github.com/jkitchin/jmax

;; This shared library is loaded from the profile-independent `funcs' path.

;;; Commentary:

;; `words' is a small contextual action dispatcher.  It takes the active region,
;; the word/symbol at point, or an explicitly entered query and hands that text
;; to the appropriate subsystem.
;;
;; The package deliberately does not implement its own spelling engine,
;; bibliography manager, project search engine, grammar service, or scholarly
;; result aggregator.  It delegates those jobs to Emacs and focused packages.
;; Its niche is the single low-friction command surface, especially for
;; researcher-oriented searches.
;;
;; Main entry points:
;;
;;   M-x words              Transient action menu.
;;   M-x words-dispatch     Completion-based action menu.
;;   M-x words-search       Choose a web/research provider.
;;   M-x words-search-default
;;
;; Query precedence is region -> thing at point -> minibuffer.  All external
;; searches are explicit; this package never searches in the background.
;;
;; Emacs 31.1 ships Transient, so this rewrite uses it directly rather than
;; retaining the historical Hydra dependency.

;;; Code:

(require 'browse-url)
(require 'button)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'thingatpt)
(require 'transient)
(require 'url-util)

(defgroup words nil
  "Contextual actions for text at point or in the active region."
  :group 'convenience
  :prefix "words-")

(defcustom words-default-provider 'google-scholar
  "Provider used by `words-search-default'."
  :type 'symbol
  :safe #'symbolp
  :group 'words)

(defcustom words-thing-at-point 'word
  "Thing used when no region is active.

The common choices are `word' for prose and `symbol' for source code."
  :type '(choice (const :tag "Word" word)
                 (const :tag "Symbol" symbol))
  :group 'words)

(defcustom words-search-providers
  '((google
     :label "Google"
     :category web
     :url "https://www.google.com/search?q=%s"
     :home "https://www.google.com/")
    (x
     :label "X"
     :category web
     :url "https://x.com/search?q=%s&src=typed_query"
     :home "https://x.com/explore")
    (thesaurus
     :label "Thesaurus.com"
     :category language
     :url "https://www.thesaurus.com/browse/%s"
     :home "https://www.thesaurus.com/")
    (google-scholar
     :label "Google Scholar"
     :category research
     :url "https://scholar.google.com/scholar?q=%s"
     :home "https://scholar.google.com/")
    (crossref
     :label "Crossref"
     :category research
     :url "https://search.crossref.org/?q=%s"
     :home "https://search.crossref.org/")
    (pubmed
     :label "PubMed"
     :category research
     :url "https://pubmed.ncbi.nlm.nih.gov/?term=%s"
     :home "https://pubmed.ncbi.nlm.nih.gov/")
    (arxiv
     :label "arXiv"
     :category research
     :url "https://arxiv.org/search/?query=%s&searchtype=all&source=header"
     :home "https://arxiv.org/")
    (semantic-scholar
     :label "Semantic Scholar"
     :category research
     :url "https://www.semanticscholar.org/search?q=%s"
     :home "https://www.semanticscholar.org/")
    (web-of-science
     :label "Web of Science"
     :category research
     :home "https://www.webofscience.com/wos/woscc/basic-search"
     :manual-query t)
    (scopus
     :label "Scopus"
     :category research
     :home "https://www.scopus.com/search/form.uri?display=basic"
     :manual-query t))
  "Providers available to `words-search'.

Each entry has the form:

  (ID :label LABEL :category CATEGORY :url TEMPLATE :home URL ...)

TEMPLATE must contain one `%s', which receives a URL-encoded query.  A
provider with `:manual-query' non-nil has no stable public deep-search URL;
`words-search' copies the query to the kill ring and opens its `:home' URL.

This variable is intentionally data-only.  Do not make it file-local: a
repository-controlled provider table could redirect selected text to an
untrusted service."
  :type 'sexp
  :group 'words)

(defcustom words-translation-languages
  '((Arabic . "ar")
    (Chinese . "zh-CN")
    (French . "fr")
    (German . "de")
    (Italian . "it")
    (Japanese . "ja")
    (Korean . "ko")
    (Portuguese . "pt")
    (Russian . "ru")
    (Spanish . "es"))
  "Languages offered by `words-translate'.

Keys are display names and values are language codes accepted by the browser
translation service."
  :type '(alist :key-type symbol :value-type string)
  :group 'words)

(defcustom words-translation-url
  "https://translate.google.com/?sl=auto&tl=%s&text=%s&op=translate"
  "Browser translation URL.

The first `%s' receives the encoded target language code and the second the
encoded text.  Translation is browser-based on purpose: `words' does not own a
remote translation API, credentials, quotas, or response parser."
  :type 'string
  :group 'words)

(defcustom words-bibliography-files nil
  "Bibliography files searched by `words-bibliography-search'.

When nil, the command also recognizes `reftex-default-bibliography' and
`bibtex-completion-bibliography' when those variables are already available.
No bibliography package is loaded merely to discover files."
  :type '(repeat file)
  :group 'words)

(defcustom words-grammar-check-function nil
  "Command used by `words-grammar-check'.

This is nil by default because silently replacing the historical After the
Deadline service with another remote grammar service would change privacy and
network semantics.  Configure a local or explicitly chosen grammar command if
you want one in the dispatcher."
  :type '(choice (const :tag "Not configured" nil) function)
  :group 'words)

(defcustom words-speech-function nil
  "Optional function used by `words-speak'.

The function is called with two arguments, TEXT and RATE.  When nil, macOS uses
the `say' executable directly and other systems signal a `user-error'."
  :type '(choice (const :tag "Automatic" nil) function)
  :group 'words)

(defcustom words-speech-voice nil
  "Voice passed to the macOS `say' command, or nil for the system default."
  :type '(choice (const :tag "System default" nil) string)
  :group 'words)

(defcustom words-speech-rate 180
  "Default speech rate in words per minute."
  :type 'natnum
  :group 'words)

(defvar words--provider-history nil
  "Minibuffer history for provider selection.")
(defvar words--action-history nil
  "Minibuffer history for action selection.")
(defvar words--translation-history nil
  "Minibuffer history for translation languages.")

(defun words--query (&optional prompt)
  "Return plain contextual query text.

Use the active region first, then `words-thing-at-point', then PROMPT (or a
standard query prompt) in the minibuffer.  Text properties are always removed
and surrounding whitespace is trimmed."
  (let* ((raw (cond
               ((use-region-p)
                (buffer-substring-no-properties
                 (region-beginning) (region-end)))
               ((thing-at-point words-thing-at-point t))
               (t nil)))
         (query (and raw (string-trim raw))))
    (if (string-empty-p (or query ""))
        (string-trim (read-string (or prompt "Query: ")))
      query)))

(defun words--nonempty-query (&optional prompt)
  "Return a non-empty query, using PROMPT when input is needed."
  (let ((query (words--query prompt)))
    (when (string-empty-p query)
      (user-error "Query cannot be empty"))
    query))

(defun words--provider-spec (provider)
  "Return plist for PROVIDER or signal `user-error'."
  (if-let* ((entry (assq provider words-search-providers)))
      (cdr entry)
    (user-error "Unknown Words provider: %s" provider)))

(defun words--provider-label (provider)
  "Return display label for PROVIDER."
  (or (plist-get (words--provider-spec provider) :label)
      (symbol-name provider)))

(defun words--provider-candidates ()
  "Return completion alist mapping provider labels to IDs."
  (mapcar (lambda (entry)
            (cons (or (plist-get (cdr entry) :label)
                      (symbol-name (car entry)))
                  (car entry)))
          words-search-providers))

(defun words--provider-annotation (candidate)
  "Return category annotation for provider CANDIDATE label."
  (when-let* ((provider (cdr (assoc candidate (words--provider-candidates))))
              (category (plist-get (words--provider-spec provider) :category)))
    (format "  [%s]" category)))

(defun words--read-provider (&optional prompt default)
  "Read a provider ID with PROMPT and DEFAULT."
  (let* ((candidates (words--provider-candidates))
         (default-label
          (when-let* ((provider (or default words-default-provider)))
            (car (rassq provider candidates))))
         (completion-extra-properties
          '(:annotation-function words--provider-annotation))
         (choice (completing-read (or prompt "Provider: ") candidates nil t nil
                                  'words--provider-history default-label)))
    (or (cdr (assoc choice candidates))
        (user-error "Unknown Words provider: %s" choice))))

(defun words--search-url (provider query)
  "Return search URL for PROVIDER and QUERY, or nil for a manual provider."
  (let* ((spec (words--provider-spec provider))
         (template (plist-get spec :url)))
    (when template
      (format template (url-hexify-string query)))))

;;;###autoload
(defun words-search (provider &optional query)
  "Search PROVIDER for QUERY.

When QUERY is nil, derive it from the region, thing at point, or minibuffer.
For providers whose web applications do not expose a stable public deep-search
URL, copy QUERY to the kill ring and open the provider's search page instead."
  (interactive (list (words--read-provider) nil))
  (let* ((query (or query (words--nonempty-query "Search for: ")))
         (spec (words--provider-spec provider))
         (url (words--search-url provider query))
         (home (plist-get spec :home)))
    (cond
     (url
      (browse-url url))
     ((and (plist-get spec :manual-query) home)
      (kill-new query)
      (browse-url home)
      (message "Words: copied %S; paste it into %s"
               query (words--provider-label provider)))
     (home
      (browse-url home))
     (t
      (user-error "Provider %s has neither a search URL nor home page"
                  provider)))))

;;;###autoload
(defun words-search-default (&optional query)
  "Search `words-default-provider' for QUERY or contextual text."
  (interactive)
  (words-search words-default-provider query))

(defun words-open-provider (provider)
  "Open PROVIDER's home page without sending a query."
  (interactive (list (words--read-provider "Open provider: ")))
  (if-let* ((home (plist-get (words--provider-spec provider) :home)))
      (browse-url home)
    (user-error "Provider %s has no home page" provider)))

;;; Language actions

;;;###autoload
(defun words-dictionary (&optional query)
  "Look up QUERY using Emacs' built-in Dictionary client."
  (interactive)
  (require 'dictionary)
  (dictionary-search (or query (words--nonempty-query "Dictionary word: "))))

;;;###autoload
(defun words-thesaurus (&optional query)
  "Search the configured thesaurus provider for QUERY."
  (interactive)
  (words-search 'thesaurus query))

;;;###autoload
(defun words-spell ()
  "Correct spelling at point or in the active region.

Use `ispell-region' for an active region.  Otherwise prefer `jinx-correct' when
Jinx is loaded and active, then fall back to built-in `ispell-word'."
  (interactive)
  (cond
   ((use-region-p)
    (require 'ispell)
    (ispell-region (region-beginning) (region-end)))
   ((and (fboundp 'jinx-correct)
         (boundp 'jinx-mode)
         (symbol-value 'jinx-mode))
    (call-interactively 'jinx-correct))
   (t
    (require 'ispell)
    (call-interactively #'ispell-word))))

;;;###autoload
(defun words-grammar-check ()
  "Run the explicitly configured grammar-check command."
  (interactive)
  (unless (commandp words-grammar-check-function)
    (user-error "Set `words-grammar-check-function' to your grammar command"))
  (call-interactively words-grammar-check-function))

;;;###autoload
(defun words-translate (language &optional query)
  "Translate QUERY to LANGUAGE in a browser.

LANGUAGE is a display-name symbol from `words-translation-languages'.  This
command intentionally performs one explicit browser request rather than owning
a remote translation API."
  (interactive
   (list
    (intern
     (completing-read
      "Translate to: "
      (mapcar (lambda (entry) (symbol-name (car entry)))
              words-translation-languages)
      nil t nil 'words--translation-history))))
  (let ((code (alist-get language words-translation-languages)))
    (unless code
      (user-error "Unknown translation language: %s" language))
    (browse-url
     (format words-translation-url
             (url-hexify-string code)
             (url-hexify-string
              (or query (words--nonempty-query "Translate: ")))))))

;;;###autoload
(defun words-speak (&optional text rate)
  "Speak TEXT at RATE words per minute.

With `words-speech-function' non-nil, call it with TEXT and RATE.  Otherwise use
the macOS `say' executable without invoking a shell."
  (interactive)
  (let ((text (or text (words--nonempty-query "Speak: ")))
        (rate (or rate words-speech-rate)))
    (cond
     (words-speech-function
      (funcall words-speech-function text rate))
     ((not (eq system-type 'darwin))
      (user-error "Configure `words-speech-function' on non-macOS systems"))
     ((not (executable-find "say"))
      (user-error "macOS `say' executable was not found"))
     (t
      (make-process
       :name "words-say"
       :buffer nil
       :noquery t
       :connection-type 'pipe
       :command
       (append (list "say")
               (when (and words-speech-voice
                          (not (string-empty-p words-speech-voice)))
                 (list "-v" words-speech-voice))
               (list "-r" (number-to-string rate) text)))))))

;;; Browser providers and compatibility-friendly direct commands

;;;###autoload
(defun words-google (&optional query)
  "Search Google for QUERY or contextual text."
  (interactive)
  (words-search 'google query))

;;;###autoload
(defun words-x (&optional query)
  "Search X for QUERY or contextual text."
  (interactive)
  (words-search 'x query))

;;;###autoload
(defun words-google-scholar (&optional query)
  "Search Google Scholar for QUERY or contextual text."
  (interactive)
  (words-search 'google-scholar query))

;;;###autoload
(defun words-crossref (&optional query)
  "Search Crossref for QUERY or contextual text."
  (interactive)
  (words-search 'crossref query))

;;;###autoload
(defun words-pubmed (&optional query)
  "Search PubMed for QUERY or contextual text."
  (interactive)
  (words-search 'pubmed query))

;;;###autoload
(defun words-arxiv (&optional query)
  "Search arXiv for QUERY or contextual text."
  (interactive)
  (words-search 'arxiv query))

;;;###autoload
(defun words-semantic-scholar (&optional query)
  "Search Semantic Scholar for QUERY or contextual text."
  (interactive)
  (words-search 'semantic-scholar query))

;;;###autoload
(defun words-wos (&optional query)
  "Search Web of Science for QUERY or contextual text."
  (interactive)
  (words-search 'web-of-science query))

;;;###autoload
(defun words-scopus (&optional query)
  "Search Scopus for QUERY or contextual text."
  (interactive)
  (words-search 'scopus query))

;;;###autoload
(defun words-twitter (&optional query)
  "Compatibility command: search X for QUERY or contextual text."
  (interactive)
  (words-x query))

(defun words-open-wos ()
  "Open the Web of Science search page."
  (interactive)
  (words-open-provider 'web-of-science))

(defun words-open-pubmed ()
  "Open PubMed."
  (interactive)
  (words-open-provider 'pubmed))

(defun words-open-scopus ()
  "Open Scopus search."
  (interactive)
  (words-open-provider 'scopus))

(defun words-open-crossref ()
  "Open Crossref Metadata Search."
  (interactive)
  (words-open-provider 'crossref))

;;; Local actions

(defun words--bibliography-files ()
  "Return readable bibliography files from configured or known variables."
  (let* ((candidate
          (or words-bibliography-files
              (and (boundp 'reftex-default-bibliography)
                   (symbol-value 'reftex-default-bibliography))
              (and (boundp 'bibtex-completion-bibliography)
                   (symbol-value 'bibtex-completion-bibliography))))
         (files (cond ((stringp candidate) (list candidate))
                      ((listp candidate) candidate)
                      (t nil))))
    (seq-filter (lambda (file)
                  (and (stringp file) (file-readable-p file)))
                files)))

;;;###autoload
(defun words-bibliography-search (&optional query)
  "Search bibliography files for QUERY using `multi-occur'."
  (interactive)
  (let ((files (words--bibliography-files))
        (query (or query (words--nonempty-query "Bibliography search: "))))
    (unless files
      (user-error "No readable bibliography files; customize `words-bibliography-files'"))
    (multi-occur (mapcar #'find-file-noselect files) (regexp-quote query))))

(defun words-bibtex (&optional query)
  "Compatibility command for `words-bibliography-search'."
  (interactive)
  (words-bibliography-search query))

;;;###autoload
(defun words-project-search (&optional query)
  "Search the current project for literal QUERY using `project.el'."
  (interactive)
  (let ((query (or query (words--nonempty-query "Project search: "))))
    (project-find-regexp (regexp-quote query))))

(defun words--searchable-buffers ()
  "Return ordinary buffers suitable for a multi-buffer text search."
  (seq-filter
   (lambda (buffer)
     (let ((name (buffer-name buffer)))
       (and name
            (not (string-prefix-p " " name))
            (not (string-prefix-p "*" name))
            (with-current-buffer buffer
              (or buffer-file-name
                  (derived-mode-p 'text-mode 'prog-mode))))))
   (buffer-list)))

;;;###autoload
(defun words-buffer-search (&optional query)
  "Search ordinary open buffers for literal QUERY using `multi-occur'."
  (interactive)
  (let ((buffers (words--searchable-buffers))
        (query (or query (words--nonempty-query "Buffer search: "))))
    (unless buffers
      (user-error "No searchable ordinary buffers"))
    (multi-occur buffers (regexp-quote query))))

(define-derived-mode words-results-mode special-mode "Words-Results"
  "Major mode for local results produced by `words'.")

(keymap-set words-results-mode-map "RET" #'push-button)
(keymap-set words-results-mode-map "q" #'quit-window)

(defun words--insert-file-button (file)
  "Insert FILE as a clickable button."
  (insert-text-button
   file
   'follow-link t
   'help-echo "Visit file"
   'action (lambda (_button) (find-file file)))
  (insert "\n"))

(defun words--mdfind-sentinel (process _event)
  "Render Spotlight results when PROCESS exits."
  (when (memq (process-status process) '(exit signal))
    (let ((buffer (process-buffer process))
          (query (process-get process 'words-query))
          (status (process-exit-status process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t)
                (output (buffer-substring-no-properties (point-min) (point-max))))
            (erase-buffer)
            (words-results-mode)
            (insert (format "Spotlight results for: %s\n\n" query))
            (if (zerop status)
                (let ((files (split-string output "\n" t)))
                  (if files
                      (mapc #'words--insert-file-button files)
                    (insert "No matches.\n")))
              (insert (format "mdfind exited with status %d\n\n%s" status output))))
          (goto-char (point-min)))
        (display-buffer buffer)))))

;;;###autoload
(defun words-mdfind (&optional query)
  "Search macOS Spotlight for file names matching QUERY asynchronously."
  (interactive)
  (unless (eq system-type 'darwin)
    (user-error "`words-mdfind' is available only on macOS"))
  (let ((program (executable-find "mdfind"))
        (query (or query (words--nonempty-query "Spotlight file search: "))))
    (unless program
      (user-error "The macOS `mdfind' executable was not found"))
    (let ((buffer (get-buffer-create "*Words Spotlight*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Searching Spotlight for: %s\n" query))))
      (let ((process
             (make-process
              :name "words-mdfind"
              :buffer buffer
              :noquery t
              :connection-type 'pipe
              :command (list program "-name" query)
              :sentinel #'words--mdfind-sentinel)))
        (process-put process 'words-query query)
        (display-buffer buffer)
        process))))

;;; Dispatcher

(defconst words--actions
  '(("Search provider" . words-search)
    ("Dictionary" . words-dictionary)
    ("Thesaurus" . words-thesaurus)
    ("Spell" . words-spell)
    ("Grammar checker" . words-grammar-check)
    ("Translate" . words-translate)
    ("Speak" . words-speak)
    ("Bibliography search" . words-bibliography-search)
    ("Project search" . words-project-search)
    ("Open-buffer search" . words-buffer-search)
    ("macOS Spotlight" . words-mdfind))
  "Actions offered by `words-dispatch'.")

;;;###autoload
(defun words-dispatch ()
  "Select and invoke a contextual Words action with completion."
  (interactive)
  (let* ((choice (completing-read "Words action: " words--actions nil t nil
                                  'words--action-history))
         (command (cdr (assoc choice words--actions))))
    (unless (commandp command)
      (user-error "Unknown Words action: %s" choice))
    (call-interactively command)))

;;;###autoload (autoload 'words "words" "Contextual text actions and research search." t)
(transient-define-prefix words ()
			 "Contextual text actions and research search."
			 [["Language"
			   ("d" "Dictionary" words-dictionary)
			   ("t" "Thesaurus" words-thesaurus)
			   ("s" "Spell" words-spell)
			   ("r" "Translate" words-translate)
			   ("k" "Speak" words-speak)
			   ("!" "Grammar" words-grammar-check)]
			  ["Web"
			   ("g" "Google" words-google)
			   ("x" "X" words-x)
			   ("/" "Choose provider" words-search)
			   ("." "Default provider" words-search-default)]
			  ["Research"
			   ("G" "Google Scholar" words-google-scholar)
			   ("c" "Crossref" words-crossref)
			   ("p" "PubMed" words-pubmed)
			   ("a" "arXiv" words-arxiv)
			   ("o" "Semantic Scholar" words-semantic-scholar)
			   ("W" "Web of Science" words-wos)
			   ("S" "Scopus" words-scopus)]
			  ["Local"
			   ("b" "Bibliography" words-bibliography-search)
			   ("P" "Project" words-project-search)
			   ("B" "Open buffers" words-buffer-search)
			   ("M" "macOS Spotlight" words-mdfind)]])

;;; Compatibility with the historical 0.1.0 surface.

(define-obsolete-function-alias 'words-atd #'words-grammar-check "2.0.0")
(define-obsolete-function-alias 'words-swiper-all #'words-buffer-search "2.0.0")
(define-obsolete-function-alias 'words-finder #'words-mdfind "2.0.0")
(define-obsolete-function-alias 'words-hydra/body #'words "2.0.0")
(define-obsolete-function-alias 'words/body #'words "2.0.0")
(define-obsolete-function-alias 'wos #'words-open-wos "2.0.0")
(define-obsolete-function-alias 'pubmed #'words-open-pubmed "2.0.0")
(define-obsolete-function-alias 'scopus #'words-open-scopus "2.0.0")
(define-obsolete-function-alias 'crossref #'words-open-crossref "2.0.0")

(provide 'words)
;;; words.el ends here
