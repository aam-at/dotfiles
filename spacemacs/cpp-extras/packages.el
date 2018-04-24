;;; packages.el --- cpp-extras layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Alexander Matyasko <alexander.matyasko@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `cpp-extras-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `cpp-extras/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `cpp-extras/pre-init-PACKAGE' and/or
;;   `cpp-extras/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst cpp-extras-packages
  '(
    company
    rtags helm-rtags
    company-rtags
    (doxymacs :location local)))

(defun cpp-extras/post-init-company ()
  (push 'company-rtags company-backends-c-mode-common))

(defun cpp-extras/init-rtags ()
  (use-package rtags))

(defun cpp-extras/init-helm-rtags ())

(defun cpp-extras/init-company-rtags ())

(defun cpp-extras/post-init-rtags ()
  (setq company-rtags-begin-after-member-access nil)
  (setq rtags-completions-enabled t)

  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)

  (defun use-rtags (&optional useFileManager)
    (and (rtags-executable-find "rc")
         (cond ((and (not (eq major-mode 'c++-mode))
                     (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
               (useFileManager (rtags-has-filemanager))
               (t (rtags-is-indexed)))))

  (defun tags-find-symbol-at-point (&optional prefix)
    (interactive "P")
    (if (and (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
        (helm-gtags-find-tag)))

  (defun tags-find-references-at-point (&optional prefix)
    (interactive "P")
    (if (and (not (rtags-find-references-at-point prefix)) rtags-last-request-not-indexed)
        (helm-gtags-find-rtag)))

  (defun tags-find-symbol ()
    (interactive)
    (call-interactively (if (use-rtags) 'rtags-find-symbol 'helm-gtags-find-symbol)))

  (defun tags-find-references ()
    (interactive)
    (call-interactively (if (use-rtags) 'rtags-find-references 'helm-gtags-find-rtag)))

  (defun tags-find-file ()
    (interactive)
    (call-interactively (if (use-rtags t) 'rtags-find-file 'helm-gtags-find-files)))

  (defun tags-imenu ()
    (interactive)
    (call-interactively (if (use-rtags t) 'rtags-imenu 'idomenu)))

  (dolist (mode '(c-mode c++-mode))
    (evil-leader/set-key-for-mode mode
      "g." 'rtags-find-symbol-at-point
      "g," 'rtags-find-references-at-point
      "gv" 'rtags-find-virtuals-at-point
      "gV" 'rtags-print-enum-value-at-point
      "g/" 'rtags-find-all-references-at-point
      "gY" 'rtags-cycle-overlays-on-screen
      "g>" 'rtags-find-symbol
      "g<" 'rtags-find-references
      "g[" 'rtags-location-stack-back
      "g]" 'rtags-location-stack-forward
      "gD" 'rtags-diagnostics
      "gG" 'rtags-guess-function-at-point
      "gp" 'rtags-set-current-project
      "gP" 'rtags-print-dependencies
      "ge" 'rtags-reparse-file
      "gE" 'rtags-preprocess-file
      "gR" 'rtags-rename-symbol
      "gm" 'rtags-symbol-type
      "gM" 'rtags-symbol-info
      "gS" 'rtags-display-summary
      "gO" 'rtags-goto-offset
      "g;" 'rtags-find-file
      "gF" 'rtags-fixit
      "gL" 'rtags-copy-and-print-current-location
      "gX" 'rtags-fix-fixit-at-point
      "gB" 'rtags-show-rtags-buffer
      "gI" 'rtags-imenu
      "gT" 'rtags-taglist
      "gh" 'rtags-print-class-hierarchy
      "ga" 'rtags-print-source-arguments))

  (rtags-enable-standard-keybindings)
  (define-key c-mode-base-map (kbd "M-.") (function tags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,") (function tags-find-references-at-point))
  (define-key c-mode-base-map (kbd "M-;") (function tags-find-file))
  (define-key c-mode-base-map (kbd "C-.") (function tags-find-symbol))
  (define-key c-mode-base-map (kbd "C-,") (function tags-find-references))
  (define-key c-mode-base-map (kbd "C-<") (function rtags-find-virtuals-at-point))
  (define-key c-mode-base-map (kbd "M-i") (function tags-imenu))

  (define-key global-map (kbd "M-.") (function tags-find-symbol-at-point))
  (define-key global-map (kbd "M-,") (function tags-find-references-at-point))
  (define-key global-map (kbd "M-;") (function tags-find-file))
  (define-key global-map (kbd "C-.") (function tags-find-symbol))
  (define-key global-map (kbd "C-,") (function tags-find-references))
  (define-key global-map (kbd "C-<") (function rtags-find-virtuals-at-point))
  (define-key global-map (kbd "M-i") (function tags-imenu)))

(defun cpp-extras/init-doxymacs ()
  (use-package doxymacs
    :commands doxymacs-mode
    :init
    (add-hook 'c++-mode-hook #'doxymacs-mode)
    :config
    (spacemacs|diminish doxymacs-mode)))
