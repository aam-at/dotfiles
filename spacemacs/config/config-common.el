;; This file configures common for use.

;;;###autoload
(defun my-common-setup ()
  (when (and (daemonp) (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  ;; set the minimum level of messages to be displayed
  (setq warning-minimum-level :error)
  ;; nicer looking faces for company
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

  ;; Emacs configuration
  (setq vc-follow-symlinks t)
  (unless (eq system-type 'gnu/linux)
    (unless (display-graphic-p)
      ;; in terminal when asking for gpg password
      (setq epg-pinentry-mode 'loopback)))

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

  ;; enable global activity watch
  (when (check-localhost-port 5600)
    (progn
      (message "Enable global activity watch")
      (global-activity-watch-mode)))

  ;; basic programming settings
  (defun my-prog-settings()
    (spacemacs/toggle-display-fill-column-indicator-on)
    (spacemacs/toggle-relative-line-numbers-on)
    (face-remap-add-relative 'default '(:family "JetBrains Mono")))
  (add-hook 'prog-mode-hook 'my-prog-settings)
  ;; speed up the lsp-mode using lsp-booster
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
            (if (and (not test?)                             ;; for check lsp-server-present?
                     (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                     lsp-use-plists
                     (not (functionp 'json-rpc-connection))  ;; native json-rpc
                     (executable-find "emacs-lsp-booster"))
                (progn
                  (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
                    (setcar orig-result command-from-exec-path))
                  (message "Using emacs-lsp-booster for %s!" orig-result)
                  (cons "emacs-lsp-booster" orig-result))
              orig-result)))
        (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
        (message "emacs-lsp-booster configuration loaded successfully."))
    (message "emacs-lsp-booster not found. To install, run: cargo install --git https://github.com/blahgeek/emacs-lsp-booster"))

  ;; basic text settings
  (defun my-text-settings()
    (visual-line-mode)
    (face-remap-add-relative 'default '(:family "iA Writer Mono S")))
  (add-hook 'text-mode-hook 'my-text-settings)
  (spacemacs/set-leader-keys "C-t l" 'visual-line-mode)

  ;; Chrome settings
  (add-hook 'edit-server-done-hook (lambda () (shell-command "wmctrl -a \"Google Chrome\"")))
  ;; Patch org-remark to work according to the following rules:
  ;; Rule 1: if the buffer is org file, keep notes in the same file. Otherwise,
  ;; create new file in the same directory. Rule 2: store the highlights in the
  ;; same file if the file is org buffer, otherwise put the highlights in the
  ;; separate file.
  (setq org-remark-notes-file-name #'org-extras/remark-notes-file)

  ;; Magit settings
  (setq magit-org-todos-filename org-projectile-file)

  ;; password-store settings
  (setq password-store-password-length 18)

  ;; calendar settings
  (evil-set-initial-state 'calendar-mode 'emacs)
  (setq calendar-date-style "european")

  ;; unfill paragraph
  (add-hook 'text-mode-hook
            (lambda ()
              (local-set-key (kbd "M-Q") 'unfill-paragraph)))

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
