;;; packages.el --- speed-reading Layer packages File for Spacemacs
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

(setq speed-reading-packages
    '(
      spray
      ))

;; List of packages to exclude.
(setq speed-reading-excluded-packages '())

(defun spacemacs/init-spray ()
  (use-package spray
    :commands spray-mode
    :init
    (progn
      (defun spacemacs/start-spray ()
        "Start spray speed reading on current buffer at current point."
        (interactive)
        (evil-insert-state)
        (spray-mode t)
        (internal-show-cursor (selected-window) nil))
      (spacemacs/set-leader-keys "asr" 'spacemacs/start-spray)

      (defadvice spray-quit (after spacemacs//quit-spray activate)
        "Correctly quit spray."
        (internal-show-cursor (selected-window) t)
        (evil-normal-state)))
    :config
    (progn
      (define-key spray-mode-map (kbd "h") 'spray-backward-word)
      (define-key spray-mode-map (kbd "l") 'spray-forward-word)
      (define-key spray-mode-map (kbd "q") 'spray-quit))))