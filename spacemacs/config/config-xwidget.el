;; This file configures xwidgets for use.

;;;###autoload
(defun my-xwidget-setup ()
  (evil-set-initial-state 'xwidget-webkit-mode 'emacs)
  (with-eval-after-load 'xwidget
    (define-key xwidget-webkit-mode-map [mouse-4] 'xwidget-webkit-scroll-down)
    (define-key xwidget-webkit-mode-map [mouse-5] 'xwidget-webkit-scroll-up)
    (define-key xwidget-webkit-mode-map (kbd "-") 'xwidget-webkit-zoom-out)
    (define-key xwidget-webkit-mode-map (kbd "+") 'xwidget-webkit-zoom-in)
    (define-key xwidget-webkit-mode-map (kbd "k") 'xwidget-webkit-scroll-down)
    (define-key xwidget-webkit-mode-map (kbd "j") 'xwidget-webkit-scroll-up)
    (define-key xwidget-webkit-mode-map (kbd "l") 'xwidget-webkit-scroll-forward)
    (define-key xwidget-webkit-mode-map (kbd "h") 'xwidget-webkit-scroll-backward)
    (define-key xwidget-webkit-mode-map (kbd "W") 'xwidget-webkit-fit-width)))

(provide 'config-xwidget)
