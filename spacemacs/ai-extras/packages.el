;;; packages.el --- ai-extras layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Alexander Matyasko <amatyasko@amatyasko-PC>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defconst ai-extras-packages
  '(
    shell-maker
    (copilot :requires company
             :location (recipe
                        :fetcher github
                        :repo "zerolfx/copilot.el"
                        :files ("*.el" "dist")))
    (copilot-chat :requires company
                  :location (recipe
                             :fetcher github
                             :repo "chep/copilot-chat.el"
                             :files ("*.el")))
    (esi-dictate :location (recipe
                            :fetcher github
                            :repo "lepisma/emacs-speech-input"
                            :files ("*.el" "*.py")))
    elisa
    (khoj :location (recipe
                     :fetcher github
                     :repo "aam-at/khoj"
                     :files ("src/interface/emacs/*.el")))
    llm
    magit-gptcommit))

(defun ai-extras/init-shell-maker ()
  (use-package shell-maker
    :demand t))

(defun ai-extras/init-copilot ()
  (use-package copilot
    :defer t
    :init
    ;; accept completion from copilot and fallback to company
    (with-eval-after-load 'company
      ;; disable inline previews
      (delq 'company-preview-if-just-one-frontend company-frontends))
    (with-eval-after-load 'copilot
      (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
      (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
      (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
      (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word))
    (add-hook 'prog-mode-hook 'copilot-mode)
    (spacemacs|diminish copilot-mode "" "C")))

(defun ai-extras/init-copilot-chat ()
  (use-package copilot-chat
    :defer t
    :ensure t
    :init
    (defun ai-extras/bury-and-kill-buffer()
      (interactive)
      (bury-buffer)
      (delete-window))
    (spacemacs/declare-prefix "$c" "Copilot-Chat")
    ;; General Chat Management
    (spacemacs/set-leader-keys
      "$cc" 'copilot-chat-switch-to-buffer        ; Switch to chat buffer
      "$cr" 'copilot-chat-reset                   ; Reset chat state
      "$cd" 'copilot-chat-display                 ; Display chat buffer
      "$cM" 'ai-extras/select-copilot-chat-model) ; Set model for suggestions
    ;; Code Explanation
    (spacemacs/declare-prefix "$ce" "Code Explanation")
    (spacemacs/set-leader-keys
      "$cee" 'copilot-chat-explain                 ; Explain selection
      "$ceE" 'copilot-chat-explain-defun           ; Explain entire function
      "$ces" 'copilot-chat-explain-symbol-at-line) ; Explain symbol at line
    ;; Code Improvement
    (spacemacs/declare-prefix "$ci" "Code Improvement")
    (spacemacs/set-leader-keys
      "$cid" 'copilot-chat-doc                     ; Generate documentation
      "$cif" 'copilot-chat-fix                     ; Fix code
      "$cio" 'copilot-chat-optimize                ; Optimize code
      "$cit" 'copilot-chat-test                   ; Test suggestions
      "$cir" 'copilot-chat-review                  ; Review selection
      "$cib" 'copilot-chat-review-whole-buffer)    ; Review whole buffer
    ;; Buffer Management
    (spacemacs/declare-prefix "$cb" "Buffer Management")
    (spacemacs/set-leader-keys
      "$cba" 'copilot-chat-add-current-buffer      ; Add current buffer to chat
      "$cbx" 'copilot-chat-del-current-buffer      ; Delete buffer from chat
      "$cbl" 'copilot-chat-list)                   ; List managed buffers
    ;; Custom Prompts
    (spacemacs/declare-prefix "$cp" "Custom Prompts")
    (spacemacs/set-leader-keys
      "$cpp" 'copilot-chat-custom-prompt-selection ; Custom prompt selection
      "$cpf" 'copilot-chat-custom-prompt-function  ; Custom prompt function
      "$cpi" 'copilot-chat-ask-and-insert)         ; Insert prompt response
    ;; Commit Management
    (spacemacs/declare-prefix "$cm" "Commit Management")
    (spacemacs/set-leader-keys
      "$cmi" 'copilot-chat-insert-commit-message)   ; Generate commit message
    ;; History Navigation
    (spacemacs/declare-prefix "$ch" "History Navigation")
    (spacemacs/set-leader-keys
      "$chp" 'copilot-chat-prompt-history-previous ; Previous history
      "$chn" 'copilot-chat-prompt-history-next)    ; Next history
    (dolist (mode '(copilot-chat-mode copilot-chat-shell-shell-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "l" 'copilot-chat-prompt-split-and-list
        "n" 'copilot-chat-prompt-history-next
        "p" 'copilot-chat-prompt-history-previous
        "r" 'copilot-chat-review
        "d" 'copilot-chat-doc
        "f" 'copilot-chat-fix
        "o" 'copilot-chat-optimize
        "t" 'copilot-chat-test
        "q" 'bury-buffer))
    :config
    (evilified-state-evilify-map copilot-chat-mode-map
      :mode copilot-chat-mode
      :bindings
      "C-c q" 'bury-buffer)
    (evilified-state-evilify-map copilot-chat-list-mode-map
      :mode copilot-chat-list-mode
      :bindings
      "RET" 'copilot-chat-list-add-or-remove-buffer
      "C"   'copilot-chat-list-clear-buffers
      "g"   'copilot-chat-list-refresh
      "q"   'ai-extras/bury-and-kill-buffer)))

(defun ai-extras/init-esi-dictate ()
  (use-package esi-dictate
    :defer t
    :bind (:map esi-dictate-mode-map
                ("C-g" . esi-dictate-stop))
    :hook (esi-dictate-speech-final . esi-dictate-fix-context)
    :init
    (spacemacs/set-leader-keys
      "$d"' esi-dictate-start)
    :config
    (setq llm-warn-on-nonfree nil)))

(defun ai-extras/init-elisa ()
  (use-package elisa
    :demand t))

(defun ai-extras/init-khoj ()
  (use-package khoj
    :defer t
    :init
    (spacemacs/set-leader-keys
      "$k" 'khoj)))

(defun ai-extras/init-llm ())

(defun ai-extras/init-magit-gptcommit ()
  (use-package magit-gptcommit
    :defer t
    :after magit llm
    :init
    (if ai-extras-autostart-gptcommit-mode
        (progn
          (magit-gptcommit-mode -1)
          (magit-gptcommit-status-buffer-setup)))))
