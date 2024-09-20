;; This file configures tex for use.

;;;###autoload
(defun my-python-setup ()
  (setq lsp-pylsp-plugins-rope-autoimport-enabled t
        lsp-pylsp-plugins-ruff-enabled t
        lsp-pyright-langserver-command "basedpyright"
        lsp-disabled-clients '(semgrep-ls trunk-lsp))

  ;; NOTE: disable poetry tracking mode because it causes Emacs to crash
  ;; (setq poetry-tracking-strategy 'switch-buffer)
  ;; (add-hook 'python-mode-hook #'poetry-tracking-mode)
  )

(provide 'config-python)
