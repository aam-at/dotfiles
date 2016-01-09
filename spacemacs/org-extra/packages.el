;;; packages.el --- org-extra Layer packages File for Spacemacs
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
(setq org-extra-packages
    '(
      org-dashboard org-journal
      ;; provides synchronization with google calendar.
      (org-caldav :location (recipe
                             :fetcher github
                             :repo "dengste/org-caldav"))
      
      ;; ebib and key-chord dependecy for orgref
      (hydra :location (recipe
                        :fetcher github
                        :repo "abo-abo/hydra"))
      (ebib :location (recipe
                       :fetcher github
                       :repo "joostkremers/ebib"))
      ;; org mode reference management
      (org-ref :location (recipe
                          :fetcher github
                          :repo "jkitchin/org-ref"))
      ob-ipython
    ))

;; List of packages to exclude.
(setq org-extra-excluded-packages '())

;; For each package, define a function org-extra/init-<package-name>
;;
;; (defun org-extra/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
(defun org-extra/init-org-dashboard()
  (use-package org-dashboard
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "oD" 'org-dashboard-display))))

(defun org-extra/init-org-journal()
  (use-package org-journal
    :defer t
    :init
    (progn
      (setq org-journal-dir "~/Dropbox/Notes/journal")
      (spacemacs/set-leader-keys "oj" 'org-journal-new-entry)
      (spacemacs/set-leader-keys-for-major-mode 'org-journal-mode
        "j" 'org-journal-new-entry
        "n" 'org-journal-open-next-entry
        "p" 'org-journal-open-previous-entry))))

(defun org-extra/init-org-caldav()
  (use-package org-caldav
    :defer t
    :init
    (setq
     org-caldav-calendars '(:calendar-id "iu5alt927aue6hsjis25qhsark@group.calendar.google.com"
                                         :files "~/Dropbox/Notes/work.org"
                                         :inbox "~/Dropbox/Notes/fromwork.org"))))

(defun org-extra/init-hydra()
  (use-package hydra
    :defer t))

(defun org-extra/init-ebib()
  (use-package ebib
    :defer t))

(defun org-extra/init-org-ref()
  (use-package org-ref
    :defer t
    :init
    (progn
      (require 'org)
      (require 'hydra)
      (setq hydra-is-helpful t)

      (require 'key-chord)
      (key-chord-define-global
       "zz"
       (defhydra org-ref-hydra ()
         "org-ref"
         ("c" org-ref-helm-insert-cite-link "cite")
         ("r" org-ref-helm-insert-ref-link "ref")
         ("l" org-ref-helm-insert-label-link "label")
         ("R" org-ref "org-ref")))
      (spacemacs/set-leader-keys
        "oc" 'org-ref-cite-hydra/body
        "ob" 'org-ref-bibtex-hydra/body)
      (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode
        "n" 'org-ref-open-bibtex-notes)
      ;; optional but very useful libraries from org-ref
      (require 'org-ref-isbn)
      (require 'org-ref-pdf)
      (require 'org-ref-url-utils)
      (require 'doi-utils)
      (require 'org-ref-worldcat)
      (require 'org-ref-scifinder)
      (require 'org-ref-pubmed)
      (require 'org-ref-arxiv)
      (require 'org-ref-sci-id)
      (require 'org-ref-bibtex)
      (require 'org-ref-scopus)
      (require 'org-ref-wos))
    :config
    ;; Org-ref configuration
    (setq reftex-default-bibliography '("~/Dropbox/Research/Bibliography/references.bib"))
    (setq org-ref-bibliography-notes "~/Dropbox/Notes/papers.org"
          org-ref-default-bibliography '("~/Dropbox/Research/Bibliography/references.bib")
          org-ref-pdf-directory "~/Dropbox/Research/Bibliography/Papers")))

(defun org-extra/init-ob-ipython()
  (spacemacs|use-package-add-hook org
    (use-package ob-ipython)
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
    ))
