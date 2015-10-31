;;; packages.el --- aam Layer packages File for Spacemacs
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
(setq aam-packages
    '(
      ;; Twitter hackernews stackexchange
      twittering-mode hackernews sx
      ;; for viewsing log files
      syslog-mode
      ;; Additional package for org-mode
      ;; TODO: configure keybindings for quick access
      org-dashboard org-journal
      ;; provides synchronization with google calendar.
      (org-caldav :location (recipe
                               :fetcher github
                               :repo "dengste/org-caldav"))
      ;; integration for orgmode and habitrpg.
      (habitrpg :location (recipe
                          :fetcher github
                          :repo "ryjm/habitrpg.el"))
      )
    )

;; List of packages to exclude.
(setq aam-excluded-packages '())

;; For each package, define a function aam/init-<package-name>
;;
;; (defun aam/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
(defun aam/init-org-dashboard()
  (use-package org-dashboard
    :defer t))

(defun aam/init-org-journal()
  (use-package org-journal
    :defer t
    :config
    (setq org-journal-dir "~/Dropbox/Notes/journal")))

(defun aam/init-org-caldav()
  (use-package org-caldav
    :defer t))

(defun aam/init-habitrpg()
  (use-package habitrpg
    :defer t))

(defun kostajh/init-twittering-mode()
  (use-package twittering-mode
    :defer t))

(defun aam/init-hackernews()
  (use-package hackernews
    :defer t))

(defun aam/init-sx()
  (use-package sx
    :defer t))

(defun aam/init-syslog-mode()
  (use-package syslog-mode
    :defer t))
