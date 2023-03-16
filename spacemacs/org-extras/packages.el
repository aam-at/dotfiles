;;; packages.el --- org-extras Layer packages File for Spacemacs
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
(defconst org-extras-packages
  '(
    (calfw :toggle (spacemacs/system-is-linux))
    (calfw-org :toggle (spacemacs/system-is-linux))
    org-super-agenda
    (org-protocol-capture-html :location (recipe
                                          :fetcher github
                                          :repo "alphapapa/org-protocol-capture-html"))
    magit-org-todos
    org
    org-transclusion
    ob-async
    (org-gcal :location (recipe
                         :fetcher github
                         :repo "kidd/org-gcal.el"))
    org-ref
    org-fragtog
    ;; pdf and pdf annotation
    org-noter
    pdf-tools
    org-pdftools
    org-noter-pdftools))

;; List of packages to exclude.
(defconst org-extras-excluded-packages '())

;; For each package, define a function org-extras/init-<package-name>
;;
;; (defun org-extras/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
(defun org-extras/init-calfw ()
  :init
  (spacemacs/set-leader-keys "aC" 'cfw:open-org-calendar)
  :config
  (evil-set-initial-state 'cfw:calendar-mode 'emacs))

(defun org-extras/init-calfw-org ()
  :init
  (require 'calfw-org))

(defun org-extras/init-org-super-agenda()
  :config (org-super-agenda-mode))

(defun org-extras/init-org-protocol-capture-html ()
  (use-package org-protocol-capture-html
    :after org))

(defun org-extras/post-init-org-protocol-capture-html ()
  (require 'org-protocol-capture-html))

(defun org-extras/init-magit-org-todos()
  :defer t
  :config
  (magit-org-todos-autoinsert))

(defun org-extras/post-init-org ()
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images
            'append))

(defun org-extras/post-init-org-transclusion ()
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "uf" #'org-extras/org-convert-org-id-link-to-file-link))

(defun org-extras/init-ob-async ()
  :defer t
  :config
  (setq ob-async-no-async-languages-alist '("ipython")))


(defun org-extras/init-org-gcal ()
  :defer t
  :init
  (progn
    (require 'org-gcal)
    (spacemacs/declare-prefix "aog" "gcal")
    (spacemacs/set-leader-keys
      "aogs" 'org-gcal-sync
      "aogf" 'org-gcal-fetch
      "aogp" 'org-gcal-post-at-point
      "aogr" 'org-gcal-refresh-token)
    (spacemacs/declare-prefix-for-mode 'org-mode "mg" "gcal")
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "gs" 'org-gcal-sync
      "gf" 'org-gcal-fetch
      "gp" 'org-gcal-post-at-point
      "gr" 'org-gcal-refresh-token))
  :config
  (setq org-gcal-dir (concat spacemacs-cache-directory "org-gcal")))

(defun org-extras/post-init-org-ref ()
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "ir" 'org-ref-helm-insert-ref-link
    "iR" 'org-ref-helm-insert-label-link)
  (require 'org-ref)
  ;; optional but very useful libraries from org-ref
  (require 'doi-utils)
  (require 'org-ref-pdf)
  (require 'org-ref-url-utils)
  (require 'org-ref-bibtex)
  (require 'org-ref-arxiv)
  (require 'org-ref-pubmed)
  (require 'org-ref-isbn)
  (require 'org-ref-wos)
  (require 'org-ref-scopus)
  (require 'x2bib)
  (require 'org-ref-scifinder)
  (require 'org-ref-worldcat))

(defun org-extras/init-org-fragtog ()
  (use-package org-fragtog-mode
    :diminish org-fragtog-mode
    :after org
    :hook (org-mode . org-fragtog-mode)))

(defun org-extras/init-org-noter ()
  :defer t
  :commands (org-noter)
  :init
  (spacemacs/set-leader-keys "aon" 'org-noter)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "n" 'org-noter))

(defun org-extras/post-init-pdf-tools ()
  (spacemacs/set-leader-keys-for-major-mode 'pdf-view-mode "N" 'org-noter)
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2016-11/msg00169.html
  ;; Before adding, remove it (to avoid clogging)
  (delete '("\\.pdf\\'" . default) org-file-apps)
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2016-11/msg00176.html
  (add-to-list 'org-file-apps
               '("\\.pdf\\'" . (lambda (file link)
                                 (org-extras/org-pdfview-open link)))))

(defun org-extras/init-org-pdftools ()
  (use-package org-pdftools
    :after org
    :hook (org-load . org-pdftools-setup-link)))

(defun org-extras/init-org-noter-pdftools ()
  (use-package org-noter-pdftools
    :after org-noter
    :config
    (with-eval-after-load 'pdf-annot
      (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note))))
