;; -*- lexical-binding: t; -*-
;; This file configures language-server clients and lsp-booster.

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

;;;###autoload
(defun aam/lsp-setup ()
  "Setup LSP configurations including lsp-booster."
  (if (executable-find "emacs-lsp-booster")
      (progn
        (defun aam/lsp-booster--advice-json-parse (old-fn &rest args)
          "Try to parse bytecode instead of json."
          (or
           (when (equal (following-char) ?#)
             (let ((bytecode (read (current-buffer))))
               (when (byte-code-function-p bytecode)
                 (funcall bytecode))))
           (apply old-fn args)))
        (require 'json)
        (advice-add 'json-parse-buffer :around
                    #'aam/lsp-booster--advice-json-parse)

        (defun aam/lsp-booster--advice-final-command (old-fn cmd &optional test?)
          "Prepend emacs-lsp-booster command to lsp CMD."
          (let ((orig-result (funcall old-fn cmd test?)))
            (if (and (not test?)
                     (not (file-remote-p default-directory))
                     lsp-use-plists
                     (not (functionp 'json-rpc-connection))
                     (executable-find "emacs-lsp-booster"))
                (progn
                  (when-let* ((command-from-exec-path (executable-find (car orig-result))))
                    (setcar orig-result command-from-exec-path))
                  (message "Using emacs-lsp-booster for %s!" orig-result)
                  (append (list "emacs-lsp-booster" "--disable-bytecode" "--") orig-result))
              orig-result)))
        (advice-add 'lsp-resolve-final-command :around #'aam/lsp-booster--advice-final-command)
        (message "emacs-lsp-booster configuration loaded successfully."))
    (message "emacs-lsp-booster not found. Install: cargo install --git https://github.com/blahgeek/emacs-lsp-booster")))

(provide 'config-lsp)
