;;; config-wayland.el --- Wayland display fixes for pgtk -*- lexical-binding: t; -*-

;; emacsclient falls back to $DISPLAY when $WAYLAND_DISPLAY is absent from its
;; environment and asks the daemon for a frame on that X display.  Opening an
;; X11 display from a pgtk build is unsupported (warning dialog, input and
;; clipboard bugs), so route such requests to the Wayland display.

(defun aam/frame-on-wayland (args)
  (let ((display (car args)))
    (if (and (stringp display)
             (string-prefix-p "wayland" display))
        args
      (cons (getenv "WAYLAND_DISPLAY") (cdr args)))))

(when (and (featurep 'pgtk) (getenv "WAYLAND_DISPLAY"))
  (advice-add 'make-frame-on-display :filter-args #'aam/frame-on-wayland))

(provide 'config-wayland)
;;; config-wayland.el ends here
