;; -*- lexical-binding: t; -*-
;; This file configures common for use.

;;;###autoload
(defun aam/common-setup ()
  (require 'config-ui)
  (when (and (daemonp) (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  ;; set the minimum level of messages to be displayed
  (setq warning-minimum-level :error)
  ;; Emacs configuration
  (setq vc-follow-symlinks t)
  (unless (eq system-type 'gnu/linux)
    (unless (display-graphic-p)
      ;; in terminal when asking for gpg password
      (setq epg-pinentry-mode 'loopback)))

  (setq desktop-restore-eager 5
        desktop-files-not-to-save "^$"
        desktop-load-locked-desktop t)
  (desktop-save-mode t)

  ;; tune gcmh for writing sessions — larger threshold, auto idle delay
  (with-eval-after-load 'gcmh
    (setq gcmh-high-cons-threshold (* 256 1024 1024)
          gcmh-idle-delay 'auto
          gcmh-auto-idle-delay-factor 10))

  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

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

  ;; enable global activity watch (deferred to avoid blocking startup)
  (run-with-idle-timer 3 nil
		       (lambda ()
			 (when (aam-check-localhost-port 5600)
			   (message "Enable global activity watch")
			   (global-activity-watch-mode))))

  ;; basic programming settings
  (defun aam/prog-settings ()
    (display-fill-column-indicator-mode 1)
    (setq-local display-line-numbers 'relative)
    (display-line-numbers-mode 1)
    (face-remap-add-relative 'default `(:family ,aam/monospace-font-family)))
  (add-hook 'prog-mode-hook 'aam/prog-settings)
  ;; basic text settings
  (defun aam/text-settings ()
    (visual-line-mode)
    (face-remap-add-relative 'default `(:family ,aam/proportional-font-family)))
  (add-hook 'text-mode-hook 'aam/text-settings)

  ;; Chrome settings
  (add-hook 'edit-server-done-hook (lambda () (shell-command "wmctrl -a \"Google Chrome\"")))
  ;; Patch org-remark to work according to the following rules:
  ;; Rule 1: if the buffer is org file, keep notes in the same file. Otherwise,
  ;; create new file in the same directory. Rule 2: store the highlights in the
  ;; same file if the file is org buffer, otherwise put the highlights in the
  ;; separate file.
  (setq org-remark-notes-file-name #'aam/org-remark-notes-file)

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
  (with-eval-after-load 'shfmt
    (when (fboundp 'diminish)
      (diminish 'shfmt-on-save-mode " ")))
  (with-eval-after-load 'tree-sitter
    (when (fboundp 'diminish)
      (diminish 'tree-sitter-mode " ")))
  (with-eval-after-load 'ts-fold
    (when (fboundp 'diminish)
      (diminish 'ts-fold-mode)))
  (with-eval-after-load 'lsp-ui
    (when (fboundp 'diminish)
      (diminish 'lsp-mode " ")
      (diminish 'lsp-lens-mode)))
  (with-eval-after-load 'helm-gtags
    (when (fboundp 'diminish)
      (diminish 'helm-gtags-mode " ⓖ"))))

(provide 'config-common)
