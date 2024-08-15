;;; packages.el --- writing Layer packages File for Spacemacs
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
(setq writing-packages
      '(
        cdlatex
        ;; general writing
        flycheck
        writeroom-mode
        writegood-mode
        flycheck-vale
        (write-or-die :location local)
        ;; synonyms and thesaurus
        jinx
        powerthesaurus
        le-thesaurus
        (mw-thesaurus :location (recipe)
                      :fetcher github
                      :repo "agzam/mw-thesaurus.el")
        synosaurus
        (words :location local)
        academic-phrases))

(defun writing/init-cdlatex()
  (use-package cdlatex
    :defer t
    :commands cdlatex-mode
    :diminish cdlatex-mode
    :init
    (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
    (add-hook 'latex-mode-hook 'turn-on-cdlatex)))

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
    :modes (text-mode latext-model org-mode markdown-mode gfm-mode))
  (add-to-list 'flycheck-checkers 'proselint)

  ;; textlint checker
  (flycheck-define-checker textlint
    "A linter for textlint."
    :command ("npx" "textlint"
              "--config" "/home/amatyasko/.textlintrc"
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
    :modes (text-mode latex-mode org-mode markdown-mode gfm-mode))
  (add-to-list 'flycheck-checkers 'textlint))

(defun writing/post-init-writeroom-mode ()
  (setq writeroom-width 90)
  (spacemacs/set-leader-keys "xW" #'writeroom-mode))

(defun writing/init-writegood-mode ()
  "Initialize writegood-mode"
  :defer t
  :init
  (spacemacs/set-leader-keys "xG" #'writegood-mode))

(defun writing/init-flycheck-vale ()
  "Initialize flycheck-vale"
  :defer t
  :confi
  (setq flycheck-vale-modes '(text-mode markdown-mode rst-mode org-mode latex-mode))
  (flycheck-vale-setup))

(defun writing/init-write-or-die ()
  (use-package write-or-die
    :defer t
    :commands write-or-die-mode
    :init
    (add-hook 'text-mode-hook #'write-or-die-mode)
    (spacemacs/set-leader-keys "xD" #'write-or-die-mode)
    (spacemacs|add-toggle write-or-die
      :status (eq write-or-die-state 1)
      :on (write-or-die-go)
      :off (write-or-die-stop)
      :documentation "Activate `Write or Die!'"
      :evil-leader "C-t d")))

(defun writing/init-jinx()
  :defer t)

(defun writing/init-powerthesaurus()
  :defer t
  :init
  (spacemacs/declare-prefix "St" "Thesaurus")
  (spacemacs/set-leader-keys
    "Sts" 'powerthesaurus-lookup-synonyms-dwim
    "Sta" 'powerthesaurus-lookup-antonyms-dwim
    "Str" 'powerthesaurus-lookup-related-dwim
    "Std" 'powerthesaurus-lookup-definitions-dwim
    "Ste" 'powerthesaurus-lookup-sentences-dwim))

(defun writing/init-le-thesaurus()
  :defer t
  :init
  (spacemacs/set-leader-keys
    "Stl" 'le-thesaurus-get-synonyms
    "StL" 'le-thesaurus-get-antonyms))

(defun writing/init-mw-thesaurus()
  :defer t
  :init
  (add-hook 'variable-pitch-mode 'mw-thesaurus-mode)
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
  :defer t)
