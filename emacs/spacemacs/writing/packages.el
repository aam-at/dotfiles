;;; packages.el --- writing layer packages for Spacemacs -*- lexical-binding: t; -*-
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
(defconst writing-local-package-root
  (file-truename
   (expand-file-name "../../local"
                     (file-truename
                      (file-name-directory (or load-file-name buffer-file-name)))))
  "Root directory for locally maintained writing packages.")

(setq writing-packages
      `(;; general writing
        flycheck
        flycheck-vale
        writeroom-mode
        writegood-mode
        (write-or-die :location ,(expand-file-name "write-or-die" writing-local-package-root))
        ;; synonyms and thesaurus
        jinx
        powerthesaurus
        le-thesaurus
        (mw-thesaurus :location (recipe
                                 :fetcher github
                                 :repo "agzam/mw-thesaurus.el"))
        synosaurus
        (words :location ,(expand-file-name "words" writing-local-package-root))
        academic-phrases))

(defun writing/post-init-flycheck ()

  ;; proselint checker
  (flycheck-define-checker proselint
			   "A linter for prose."
			   :command ("proselint" source-inplace)
			   :error-patterns
			   ((warning line-start (file-name) ":" line ":" column ": "
				     (id (one-or-more (not (any " "))))
				     (message (one-or-more not-newline)
					      (zero-or-more "\n" (any " ") (one-or-more not-newline)))
				     line-end))
			   :modes (text-mode latex-mode LaTeX-mode org-mode markdown-mode gfm-mode))
  (add-to-list 'flycheck-checkers 'proselint)

  ;; textlint checker
  (flycheck-define-checker textlint
			   "A linter for textlint."
			   :command ("npx" "textlint"
				     "--config" (eval (expand-file-name "~/.textlintrc"))
				     "--format" "unix"
				     "--rule" "write-good"
				     "--rule" "no-start-duplicated-conjunction"
				     "--rule" "max-comma"
				     "--rule" "terminology"
				     "--rule" "period-in-list-item"
				     "--rule" "abbr-within-parentheses"
				     "--rule" "alex"
				     "--rule" "common-misspellings"
				     "--rule" "en-max-word-count"
				     "--rule" "diacritics"
				     "--rule" "stop-words"
				     "--plugin"
				     (eval
				      (if (derived-mode-p 'tex-mode)
					  "latex"
					"@textlint/text"))
				     source-inplace)
			   :error-patterns
			   ((warning line-start (file-name) ":" line ":" column ": "
				     (message (one-or-more not-newline)
					      (zero-or-more "\n" (any " ") (one-or-more not-newline)))
				     line-end))
			   :modes (text-mode latex-mode LaTeX-mode org-mode markdown-mode gfm-mode))
  (add-to-list 'flycheck-checkers 'textlint))

(defun writing/init-flycheck-vale ()
  "Initialize flycheck-vale"
  (with-eval-after-load  'flycheck
    (require 'flycheck-vale)
    (setq flycheck-vale-modes '(text-mode
                                markdown-mode
                                rst-mode
                                org-mode
                                latex-mode
                                LaTeX-mode))
    (flycheck-vale-setup)
    (dolist (mode flycheck-vale-modes)
      (flycheck-add-mode 'vale mode))))

(defun writing/post-init-writeroom-mode ()
  (setq writeroom-width 90)
  (spacemacs/set-leader-keys "xW" #'writeroom-mode))

(defun writing/init-writegood-mode ()
  "Initialize writegood-mode"
  (spacemacs/set-leader-keys "xG" #'writegood-mode))

(defun writing/init-write-or-die ()
  (use-package write-or-die
    :defer t
    :commands (write-or-die-mode write-or-die-toggle)
    :init
    (spacemacs/set-leader-keys "xD" #'write-or-die-toggle)
    (spacemacs|add-toggle write-or-die
                          :status (and (boundp 'write-or-die-state)
                                       (> write-or-die-state 0))
                          :on (write-or-die-go)
                          :off (write-or-die-mode -1)
                          :documentation "Activate `Write or Die!'"
                          :evil-leader "C-t d")))

(defun writing/init-jinx()
  (use-package jinx
    :defer t
    :diminish jinx-mode
    :init
    (with-eval-after-load 'ispell
      (global-set-key [remap ispell-word] #'jinx-correct))
    (with-eval-after-load 'evil-commands
      (global-set-key [remap evil-next-flyspell-error] #'jinx-next)
      (global-set-key [remap evil-prev-flyspell-error] #'jinx-previous))
    (global-jinx-mode)))

(defun writing/init-powerthesaurus()
  (spacemacs/declare-prefix "St" "Thesaurus")
  (spacemacs/set-leader-keys
   "Sts" 'powerthesaurus-lookup-synonyms-dwim
   "Sta" 'powerthesaurus-lookup-antonyms-dwim
   "Str" 'powerthesaurus-lookup-related-dwim
   "Std" 'powerthesaurus-lookup-definitions-dwim
   "Ste" 'powerthesaurus-lookup-sentences-dwim))

(defun writing/init-le-thesaurus()
  (spacemacs/set-leader-keys
   "Stl" 'le-thesaurus-get-synonyms
   "StL" 'le-thesaurus-get-antonyms))

(defun writing/init-mw-thesaurus()
  (add-hook 'variable-pitch-mode-hook #'mw-thesaurus-mode)
  (spacemacs/set-leader-keys
   "Stm" 'mw-thesaurus-lookup-dwim))

(defun writing/init-synosaurus()
  (use-package synosaurus
    :defer t
    :diminish synosaurus-mode
    :init
    (add-hook 'text-mode-hook 'synosaurus-mode)
    (add-hook 'markdown-mode-hook 'synosaurus-mode)
    (spacemacs/set-leader-keys "Stw" 'synosaurus-lookup)
    :config
    (setq synosaurus-choose-method 'default)))

(defun writing/init-words()
  (use-package words
    :defer t
    :commands (words words-hydra/body)
    :init
    (spacemacs/set-leader-keys
     "Sw" 'words-hydra/body)))

(defun writing/init-academic-phrases()
  (spacemacs/set-leader-keys
   "Spa" #'academic-phrases
   "SpA" #'academic-phrases-by-section))
