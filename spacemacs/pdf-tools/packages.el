;;; packages.el --- pdf-tools Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq pdf-tools-packages
      '(
        pdf-tools
        org-pdfview
        ))

;; List of packages to exclude.
(setq pdf-tools-excluded-packages '())

(defun pdf-tools/init-pdf-tools()
  (use-package pdf-tools
    :defer t
    :init
    (progn
      (pdf-tools-install)
      (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))
    :config
    (evilified-state-evilify pdf-view-mode pdf-view-mode-map
      ;; TODO: make vim like search work
      ;; Use instead isearch - C-s and C-r
      ;; "/"  'isearch-forward
      ;; "?"  'isearch-backward
      ;; Navigation
      "h"         'pdf-view-previous-page
      "j"         'pdf-view-next-line-or-next-page
      "k"         'pdf-view-previous-line-or-previous-page
      "l"         'pdf-view-next-page
      "gg"        'pdf-view-first-page
      "G"         'pdf-view-last-page
      "gt"        'pdf-view-goto-page
      "gl"        'pdf-view-goto-label
      (kbd "C-d") 'pdf-view-scroll-up-or-next-page
      (kbd "C-u") 'pdf-view-scroll-down-or-previous-page
      ;; Jump to link
      "f"         'pdf-links-action-perform
      "F"         'pdf-links-isearch-link
      ;; History navigation
      "H"         'pdf-history-backward
      "L"         'pdf-history-forward
      ;; Image box slice
      "sm"        'pdf-view-set-slice-using-mouse
      "sb"        'pdf-view-set-slice-from-bounding-box
      "sr"        'pdf-view-reset-slice
      ;; Annotations
      "al"        'pdf-annot-list-annotations
      "aD"        'pdf-annot-delete
      "aa"        'pdf-annot-attachment-dired
      "at"        'pdf-annot-add-text-annotation
      ;; Misc
      "so"        'pdf-occur
      "o"         'pdf-outline
      "i"         'pdf-misc-display-metadata
      (kbd "C-p") 'pdf-misc-print-document
      "u"         'pdf-view-revert-buffer)

    (evil-define-key 'visual pdf-view-mode-map
      "am" 'pdf-annot-add-markup-annotation
      "ah" 'pdf-annot-add-highlight-markup-annotation
      "ao" 'pdf-annot-add-strikeout-markup-annotation
      "as" 'pdf-annot-add-squiggly-markup-annotation
      "au" 'pdf-annot-add-underline-markup-annotation
      "y"  'pdf-view-kill-ring-save)

    (evilified-state-evilify pdf-outline-buffer-mode pdf-outline-buffer-mode-map
        "-"                'negative-argument
        "j"                'next-line
        "k"                'previous-line
        "K"                'outline-backward-same-level
        "J"                'outline-forward-same-level
        "h"                'hide-subtree
        "l"                'show-subtree
        "L"                'show-all
        "d"                'hide-subtree
        "s"                'show-subtree
        "a"                'show-all
        (kbd "C-h")        'pdf-outline-up-heading
        "gg"               'beginning-of-buffer
        "G"                'pdf-outline-end-of-buffer
        "TAB"              'outline-toggle-children
        "RET"              'pdf-outline-follow-link
        "M-RET"            'pdf-outline-follow-link-and-quit
        "f"                'pdf-outline-display-link
        [mouse-1]          'pdf-outline-mouse-display-link
        "o"                'pdf-outline-select-pdf-window
        "``"               'pdf-outline-move-to-current-page
        "''"               'pdf-outline-move-to-current-page
        "Q"                'pdf-outline-quit-and-kill
        "q"                'quit-window
        "F"                'pdf-outline-follow-mode)

    (evilified-state-evilify pdf-occur-buffer-mode pdf-occur-buffer-mode-map
      "q"           'tablist-quit
      "r"           'pdf-occur-revert-buffer-with-args
      "*"           'spacemacs/enter-ahs-forward
      ;; "?" 'evil-search-backward
      "RET"         'pdf-occur-goto-occurrence
      (kbd "M-RET") 'pdf-occur-view-occurrence)

    (evilified-state-evilify pdf-annot-list-mode pdf-annot-list-mode-map
      "f" 'pdf-annot-list-display-annotation-from-id
      "d" 'tablist-flag-forward
      "x" 'tablist-do-flagged-delete
      "u" 'tablist-unmark-forward
      "q" 'tablist-quit
      )

    (spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode
      ;; Fit document to window
      "fw" 'pdf-view-fit-width-to-window
      "fh" 'pdf-view-fit-height-to-window
      "fp" 'pdf-view-fit-page-to-window)))

(defun pdf-tools/init-org-pdfview()
  :ensure t)
