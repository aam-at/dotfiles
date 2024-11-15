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
    org-super-agenda
    (org-protocol-capture-html :location (recipe
                                          :fetcher github
                                          :repo "alphapapa/org-protocol-capture-html"))
    magit-org-todos
    org
    org-transclusion
    vulpea
    ob-async
    (org-gcal :toggle org-enable-gcal
              :location (recipe
                         :fetcher github
                         :repo "kidd/org-gcal.el"))
    org-ref
    (delve :toggle org-enable-delve
           :location (recipe
                      :fetcher github
                      :repo "publicimageltd/delve"))
    (org-similarity :location (recipe
                               :fetcher github
                               :repo "aam-at/org-similarity"))
    org-fragtog
    ;; pdf and pdf annotation
    org-noter
    pdf-tools
    org-pdftools
    org-noter-pdftools))

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
  (spacemacs/declare-prefix-for-mode 'org-mode "S" "Scholar")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "Sr" 'org-extras/remove-all-overlays
    "Sy" 'org-extras/sort-entries-by-year
    "SY" 'org-extras/filter-entries-by-year
    "Sc" 'org-extras/sort-entries-by-citations
    "SC" 'org-extras/filter-entries-by-citations
    "Su" 'org-extras/citations-update-at-point)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images
            'append))

(defun org-extras/post-init-org-transclusion ()
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "uf" #'org-extras/convert-org-id-link-to-file-link))

(defun org-extras/init-vulpea()
  (use-package vulpea
    :after org-roam
    :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable))
    :init
    (spacemacs/set-leader-keys
      "aorf" 'vulpea-find
      "aorF" 'org-roam-node-find
      "aori" 'vulpea-insert
      "aorI" 'org-roam-node-insert
      "aorb" 'vulpea-find-backlink)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "rf" 'vulpea-find
      "rF" 'org-roam-node-find
      "ri" 'vulpea-insert
      "rI" 'org-roam-node-insert
      "rb" 'vulpea-find-backlink)))

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
  (require 'openalex)
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

(defun org-extras/init-delve ()
  (use-package delve
    :defer t
    :after org-roam
    :commands
    (delve delve-minor-mode-collect-actions delve-minor-mode-edit-actions delve-minor-mode-inspect-actions)
    :hook
    (delve-mode . delve-compact-view-mode) ; turn on compact view per default
    (delve-mode . hl-line-mode)   ; nicer to use with hl-line
    :init
    (require 'delve-minor-mode)
    (evilified-state-evilify-map delve-mode-map
      :mode delve-mode
      :bindings
      (kbd "RET") 'delve--key--toggle-preview
      (kbd "+") 'delve--key--add-tags
      (kbd "-") 'delve--key--remove-tags
      (kbd "T") 'delve--key--insert-node-by-tags
      (kbd "b") 'delve--key--backlinks
      (kbd "c") 'delve--key--collect-into-buffer
      (kbd "f") 'delve--key--fromlinks
      (kbd "g") 'delve--key--refresh
      (kbd "h") 'delve--key--insert-heading
      (kbd "i") 'delve--key--insert-query-or-pile
      (kbd "n") 'delve--node-transient-key
      (kbd "o") 'delve--key--open-zettel
      (kbd "p") 'delve--key--collect-into-pile
      (kbd "q") 'bury-buffer
      (kbd "s") 'delve--key--sort
      (kbd "t") 'delve--key--insert-tagged
      (kbd "v") 'delve-compact-view-mode
      (kbd "C-<left>") 'delve--key--backlinks
      (kbd "C-<return>") 'delve--key--open-zettel
      (kbd "C-<right>") 'delve--key--fromlinks
      (kbd "<delete>") 'delve--key--multi-delete)
    (spacemacs/set-leader-keys-for-major-mode 'delve-mode
      "+" 'delve--key--add-tags
      "-" 'delve--key--remove-tags
      "T" 'delve--key--insert-node-by-tags
      "b" 'delve--key--backlinks
      "c" 'delve--key--collect-into-buffer
      "f" 'delve--key--fromlinks
      "g" 'delve--key--refresh
      "h" 'delve--key--insert-heading
      "i" 'delve--key--insert-query-or-pile
      "n" 'delve--node-transient-key
      "o" 'delve--key--open-zettel
      "p" 'delve--key--collect-into-pile
      "q" 'bury-buffer
      "s" 'delve--key--sort
      "t" 'delve--key--insert-tagged
      "v" 'delve-compact-view-mode)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "Dd" 'delve
      "Dc" 'delve-minor-mode-collect-actions
      "De" 'delve-minor-mode-edit-actions
      "Di" 'delve-minor-mode-inspect-actions)
    :config
    (spacemacs|diminish delve-minor-mode " ⓓ" " D")
    (delve-global-minor-mode +1)))

(defun org-extras/init-org-similarity ()
  (use-package org-similarity
    :after org
    :commands (org-similarity-sidebuffer org-similarity-query)
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "Ss" 'org-similarity-sidebuffer
      "Sq" 'org-similarity-query)
    :config
    (setq org-similarity-directory org-directory
          org-similarity-file-extension-pattern "*.org"
          org-similarity-language "english"
          org-similarity-algorithm "tfidf"
          org-similarity-number-of-documents 10
          org-similarity-min-chars 0
          org-similarity-show-scores t
          org-similarity-threshold 0.05
          org-similarity-use-id-links t
          org-similarity-recursive-search t
          org-similarity-custom-python-interpreter nil
          org-similarity-remove-first t
          org-similarity-heading "** Related notes"
          org-similarity-prefix "- "
          org-similarity-ignore-frontmatter nil)))

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
                                 (org-extras/pdfview-open link)))))

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
