;; This file configures common for use.

;;;###autoload
(defun my-common-setup ()
  (when (and (daemonp) (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PYTHONPATH"))

  ;; Emacs configuration
  (setq vc-follow-symlinks t)
  (unless (eq system-type 'gnu/linux)
    (unless (display-graphic-p)
      ;; in terminal when asking for gpg password
      (setq epa-pinentry-mode 'loopback)))

  (desktop-save-mode t)
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

  ;; startup
  (setq spacemacs-buffer-startup-lists-length nil)

  ;; layouts configuration
  (setq layouts-enable-autosave t)

  ;; epa encryption
  (setq epa-file-select-keys 0
        epa-file-cache-passphrase-for-symmetric-encryption t)
  (add-to-list 'load-suffixes ".el.gpg")

  ;; tramp configuration
  (setq tramp-default-method "rsync")

  ;; enable visual line navigation
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

  ;; basic programming settings
  (defun my-prog-settings()
    (spacemacs/toggle-fill-column-indicator-on)
    (spacemacs/toggle-relative-line-numbers-on))
  (add-hook 'prog-mode-hook 'my-prog-settings)

  ;; basic text settings
  (add-hook 'text-mode-hook 'visual-line-mode)
  (spacemacs/set-leader-keys "C-t l" 'visual-line-mode)

  ;; Chrome settings
  (add-hook 'edit-server-done-hook (lambda () (shell-command "wmctrl -a \"Google Chrome\"")))
  ;; Patch org-remark to work according to the following rules:
  ;; Rule 1: if the buffer is org file, keep notes in the same file. Otherwise,
  ;; create new file in the same directory. Rule 2: store the highlights in the
  ;; same file if the file is org buffer, otherwise put the highlights in the
  ;; separate file.
  (setq org-remark-notes-file-name #'org-extras/remark-notes-file)
  (advice-add 'org-remark-highlight-save :override #'org-extras/remark-highlight-save)

  ;; Magit settings
  (setq magit-org-todos-filename org-projectile-file)

  ;; password-store settings
  (setq password-store-password-length 18)

  ;; calendar settings
  (evil-set-initial-state 'calendar-mode 'emacs)
  (setq calendar-date-style "european")

  ;; status line
  (with-eval-after-load 'tree-sitter
    (spacemacs|diminish tree-sitter-mode " " " T"))
  (with-eval-after-load 'ts-fold
    (spacemacs|diminish ts-fold-mode))
  (with-eval-after-load 'lsp-ui
    (spacemacs|diminish lsp-mode " " " L")
    (spacemacs|diminish lsp-lens-mode))
  (with-eval-after-load 'helm-gtags
    (spacemacs|diminish helm-gtags-mode " ⓖ" " (g)")))

(provide 'config-common)
