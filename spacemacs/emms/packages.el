;;; packages.el --- emms layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
;; added to `emms-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `emms/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `emms/pre-init-PACKAGE' and/or
;;   `emms/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst emms-packages
  '(
    emms
    helm-emms))

(defun emms/init-emms()
  :init
  (progn
    (setq emms-directory (concat spacemacs-cache-directory "emms"))

    (require 'emms-setup)
    (require 'emms-history)
    (require 'emms-playlist-sort)
    (require 'emms-mode-line)
    (require 'emms-playing-time)
    (require 'emms-cue)
    (emms-all)
    (emms-default-players)

    (spacemacs/declare-prefix "am" "music")
    (spacemacs/set-leader-keys
      "amb" 'emms-browser
      "amB" 'emms-smart-browse
      "amp" 'emms-playlist-mode-go
      "ams" 'emms-streams
      "amo" 'emms-show
      "amO" 'emms-show-all
      "am ." 'emms-next
      "am ," 'emms-previous
      "am SPC" 'emms-pause
      "am r" 'emms-random
      "am RET" 'emms-smart-browse
      "am+" 'emms-volume-mode-plus
      "am-" 'emms-volume-mode-minus
      )
    (add-hook 'emms-browser-show-display-hook 'evil-initialize)
    (add-hook 'emms-stream-hook 'evil-initialize)
    )
  :config
  (progn
    (setq emms-source-file-default-directory "~/Music")
    (setq emms-player-mpd-music-directory "~/Music")
    (evil-set-initial-state 'emms-playlist-mode 'emacs)
    (define-key emms-playlist-mode-map
      (kbd dotspacemacs-leader-key) spacemacs-default-map)
    (evil-set-initial-state 'emms-show-all-mode 'emacs)
    (define-key emms-show-all-mode-map
      (kbd dotspacemacs-leader-key) spacemacs-default-map)
    (evil-set-initial-state 'emms-browser-mode 'emacs)
    (define-key emms-browser-mode-map
      (kbd dotspacemacs-leader-key) spacemacs-default-map)
    (evil-set-initial-state 'emms-stream-mode 'emacs)
    (define-key emms-stream-mode-map
      (kbd dotspacemacs-leader-key) spacemacs-default-map)
    ))

(defun emms/init-helm-emms()
  :defer t)
