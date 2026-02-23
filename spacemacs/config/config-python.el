;; This file configures tex for use.

;;;###autoload
(defun my-python-setup ()
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
      :priority 1
      :server-id 'ty))

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("pyrefly" "lsp"))
      :activation-fn (lsp-activate-on "python")
      :priority 2
      :server-id 'pyrefly)))

  (reformatter-define ruff-isort
      :program ruff-format-command
      :args (list "check" "--select" "I" "--fix" "--stdin-filename" (or (buffer-file-name) input-file))
      :lighter " RuffIsort"
      :group 'ruff-format)
  ;; NOTE: disable poetry tracking mode because it causes Emacs to crash
  ;; (setq poetry-tracking-strategy 'switch-buffer)
  ;; (add-hook 'python-mode-hook #'poetry-tracking-mode)
  )

(provide 'config-python)
