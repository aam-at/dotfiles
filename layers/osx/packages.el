(setq osx-packages
      '(
        exec-path-from-shell
        osx-trash
        pbcopy
        launchctl
        reveal-in-osx-finder
        ))

(when (spacemacs/system-is-mac)
  ;; Enable built-in trash support via finder API if available (only on Emacs
  ;; Mac Port)
  (when (boundp 'mac-system-move-file-to-trash-use-finder)
    (setq mac-system-move-file-to-trash-use-finder t)))

(defun osx/post-init-exec-path-from-shell ()
  ;; Use GNU ls as `gls' from `coreutils' if available.  Add `(setq
  ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning when
  ;; not using GNU ls.  We must look for `gls' after `exec-path-from-shell' was
  ;; initialized to make sure that `gls' is in `exec-path'
  (when (spacemacs/system-is-mac)
    (let ((gls (executable-find "gls")))
      (when gls
        (setq insert-directory-program gls
              dired-listing-switches "-aBhl --group-directories-first")))))

(defun osx/init-osx-trash ()
  (use-package osx-trash
    :if (and (spacemacs/system-is-mac)
             (not (boundp 'mac-system-move-file-to-trash-use-finder)))
    :init (osx-trash-setup)))

(defun osx/init-pbcopy ()
  (use-package pbcopy
    :if (and (spacemacs/system-is-mac) (not (display-graphic-p)))
    :init (turn-on-pbcopy)))

(defun osx/init-launchctl ()
  (use-package launchctl
    :if (spacemacs/system-is-mac)
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.plist$" . nxml-mode))
      (evil-leader/set-key "al" 'launchctl))
    :config
    (progn
      (evilify launchctl-mode launchctl-mode-map
               (kbd "q") 'quit-window
               (kbd "s") 'tabulated-list-sort
               (kbd "g") 'launchctl-refresh
               (kbd "n") 'launchctl-new
               (kbd "e") 'launchctl-edit
               (kbd "v") 'launchctl-view
               (kbd "l") 'launchctl-load
               (kbd "u") 'launchctl-unload
               (kbd "r") 'launchctl-reload
               (kbd "S") 'launchctl-start
               (kbd "K") 'launchctl-stop
               (kbd "R") 'launchctl-restart
               (kbd "D") 'launchctl-remove
               (kbd "d") 'launchctl-disable
               (kbd "E") 'launchctl-enable
               (kbd "i") 'launchctl-info
               (kbd "f") 'launchctl-filter
               (kbd "=") 'launchctl-setenv
               (kbd "#") 'launchctl-unsetenv
               (kbd "h") 'launchctl-help))))

(defun osx/init-reveal-in-osx-finder ()
  (use-package reveal-in-osx-finder
    :if (spacemacs/system-is-mac)
    :commands reveal-in-osx-finder))