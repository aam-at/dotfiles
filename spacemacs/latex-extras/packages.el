;;; packages.el --- latex-extras layer packages file for Spacemacs.
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
(defconst latex-extras-packages
  '(
    adaptive-wrap
    auctex-cont-latexmk
    auctex-label-numbers
    cdlatex
    preview-auto
    (texpresso :location (recipe
                          :fetcher github
                          :repo "let-def/texpresso"
                          :files ("emacs/*.el")))
    (twauctex :location (recipe
                         :fetcher github
                         :repo "cgroza/twauctex"))))

(defun latex-extras/init-adaptive-wrap()
  (use-package adaptive-wrap
    :defer t
    :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
    :init (setq-default adaptive-wrap-extra-indent 0)))

(defun latex-extras/init-auctex-cont-latexmk()
  (use-package auctex-cont-latexmk
    :defer t
    :after latex
    :init
    (spacemacs/declare-prefix-for-mode 'latex-mode "T" "toggles")
    (spacemacs/set-leader-keys-for-major-mode 'latex-mode
      "Tc" 'auctex-cont-latexmk-toggle)))

(defun latex-extras/init-auctex-label-numbers()
  (use-package auctex-label-numbers
    :defer t
    :after latex
    :hook ((plain-TeX-mode . auctex-label-numbers-mode)
           (LaTeX-mode . auctex-label-numbers-mode))))

(defun latex-extras/init-cdlatex()
  (use-package cdlatex
    :defer t
    :commands cdlatex-mode
    :diminish cdlatex-mode
    :hook ((plain-TeX-mode . cdlatex-mode)
           (LaTeX-mode . cdlatex-mode))
    :init
    ;; Use \( ... \) instead of $ ... $.
    (setq cdlatex-use-dollar-to-ensure-math nil)
    ;; based on https://github.com/doomemacs/doomemacs/blob/master/modules/lang/latex/config.el
    (with-eval-after-load 'cdlatex
      ;; Smartparens takes care of inserting closing delimiters, and if you
      ;; don't use smartparens you probably don't want these either.
      (define-key cdlatex-mode-map (kbd "$") nil)
      (define-key cdlatex-mode-map (kbd "(") nil)
      (define-key cdlatex-mode-map (kbd "{") nil)
      (define-key cdlatex-mode-map (kbd "[") nil)
      (define-key cdlatex-mode-map (kbd "|") nil)
      (define-key cdlatex-mode-map (kbd "<") nil)
      ;; AUCTeX takes care of auto-inserting {} on _^ if you want, with
      ;; `TeX-electric-sub-and-superscript'.
      (define-key cdlatex-mode-map (kbd "^") nil)
      (define-key cdlatex-mode-map (kbd "_") nil))))

(defun latex-extras/init-preview-auto()
  (use-package preview-auto
    :defer t
    :after latex
    :hook ((plain-TeX-mode . preview-auto-conditionally-enable)
           (LaTeX-mode . preview-auto-conditionally-enable))
    :config
    (setq preview-locating-previews-message nil)
    (setq preview-protect-point t)
    (setq preview-leave-open-previews-visible t)))

(defun latex-extras/init-texpresso()
  (use-package texpresso
    :defer t
    :init
    (require 'texpresso)
    (spacemacs/set-leader-keys-for-major-mode 'latex-mode
      "t" 'texpresso)))

(defun latex-extras/init-twauctex()
  (use-package twauctex
    :defer t
    :after latex
    :diminish twauctex-mode
    :init
    ;; interferes with twauctex
    (remove-hook 'LaTeX-mode-hook #'latex/auto-fill-mode)
    (twauctex-global-mode)))
