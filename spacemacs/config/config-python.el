;; This file configures tex for use.

;;;###autoload
(defun my-python-setup ()
  (setq lsp-pylsp-plugins-rope-autoimport-enabled t
        lsp-pylsp-plugins-ruff-enabled t
        lsp-pyright-langserver-command "basedpyright"
        lsp-semgrep-languages '())

  (setq poetry-tracking-mode t
        poetry-tracking-strategy 'switch-buffer)
  (add-hook 'python-mode-hook #'poetry-tracking-mode))

(provide 'config-python)
