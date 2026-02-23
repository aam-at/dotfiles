;; This file configures LSP and lsp-booster.

;;;###autoload
(defun my-lsp-setup ()
  "Setup LSP configurations including lsp-booster."
  (if (executable-find "emacs-lsp-booster")
      (progn
        (defun lsp-booster--advice-json-parse (old-fn &rest args)
          "Try to parse bytecode instead of json."
          (or
           (when (equal (following-char) ?#)
             (let ((bytecode (read (current-buffer))))
               (when (byte-code-function-p bytecode)
                 (funcall bytecode))))
           (apply old-fn args)))
        (advice-add (if (progn (require 'json)
                               (fboundp 'json-parse-buffer))
                        'json-parse-buffer
                      'json-read)
                    :around
                    #'lsp-booster--advice-json-parse)

        (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
          "Prepend emacs-lsp-booster command to lsp CMD."
          (let ((orig-result (funcall old-fn cmd test?)))
            (if (and (not test?)
                     (not (file-remote-p default-directory))
                     lsp-use-plists
                     (not (functionp 'json-rpc-connection))
                     (executable-find "emacs-lsp-booster"))
                (progn
                  (when-let ((command-from-exec-path (executable-find (car orig-result))))
                    (setcar orig-result command-from-exec-path))
                  (message "Using emacs-lsp-booster for %s!" orig-result)
                  (append (list "emacs-lsp-booster" "--disable-bytecode" "--") orig-result))
              orig-result)))
        (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
        (message "emacs-lsp-booster configuration loaded successfully."))
    (message "emacs-lsp-booster not found. Install: cargo install --git https://github.com/blahgeek/emacs-lsp-booster")))

(provide 'config-lsp)
