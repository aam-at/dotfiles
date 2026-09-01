;; -*- lexical-binding: t; -*-
;; This file configures Python support.

(defconst aam/python-eglot-server-alternatives
  '(("ty" "server") ("pyrefly" "lsp"))
  "Python Eglot servers, ordered from highest to lowest priority.")

;;;###autoload
(defun aam/python-setup ()
  (when (aam/lsp-client-p)
    (setq lsp-pylsp-plugins-rope-autoimport-enabled t
          lsp-pylsp-plugins-ruff-enabled t
          lsp-pyright-langserver-command "basedpyright"
          lsp-pyright-multi-root nil
          lsp-disabled-clients '(semgrep-ls trunk-lsp ruff pyls pylsp pyright))

    (with-eval-after-load 'lsp-mode
      (lsp-register-client
       (make-lsp-client
        :new-connection (lsp-stdio-connection '("ty" "server"))
        :activation-fn (lsp-activate-on "python")
        :priority -5
        :server-id 'ty-ls))

      (lsp-register-client
       (make-lsp-client
        :new-connection (lsp-stdio-connection '("pyrefly" "lsp"))
        :activation-fn (lsp-activate-on "python")
        :priority -4
        :server-id 'pyrefly))))

  (when (aam/eglot-client-p)
    ;; Eglot has no numeric client priority. `eglot-alternatives' tries the
    ;; configured server commands in order, so Ty is preferred over Pyrefly.
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   (cons '(python-mode python-ts-mode)
                         (eglot-alternatives aam/python-eglot-server-alternatives)))))

  (when (require 'ruff-format nil t)
    (reformatter-define ruff-isort
			:program ruff-format-command
			:args (list "check" "--select" "I" "--fix" "--stdin-filename" (or (buffer-file-name) input-file))
			:lighter " RuffIsort"
			:group 'ruff-format))
  ;; NOTE: disable poetry tracking mode because it causes Emacs to crash
  ;; (setq poetry-tracking-strategy 'switch-buffer)
  ;; (add-hook 'python-mode-hook #'poetry-tracking-mode)
  )

(provide 'config-python)
