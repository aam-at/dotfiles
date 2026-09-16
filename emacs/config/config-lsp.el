;; -*- lexical-binding: t; -*-
;; This file selects the language-server client.

(defcustom aam/language-server-client 'eglot
  "Language-server client used by Emacs profiles.

Use `eglot' for Emacs's built-in client and its smaller runtime footprint, or
`lsp' for lsp-mode's richer integration.  Restart Emacs after changing it."
  :type '(choice (const :tag "Eglot" eglot)
                 (const :tag "lsp-mode" lsp))
  :group 'applications)

(defun aam/lsp-client-p ()
  "Return non-nil when lsp-mode is the selected language-server client."
  (eq aam/language-server-client 'lsp))

(defun aam/eglot-client-p ()
  "Return non-nil when Eglot is the selected language-server client."
  (eq aam/language-server-client 'eglot))

(provide 'config-lsp)
