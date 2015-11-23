;; configuration for org and google calendar synchronisation
(setq org-caldav-calendars
      '((:calendar-id "iu5alt927aue6hsjis25qhsark@group.calendar.google.com" :files ("~/Dropbox/Notes/work.org")
                      :inbox "~/Dropbox/Notes/fromwork.org")))


;; Configure ropemacs
;; ropemacs rope pymacs needs to be installed manually
(defun load-ropemacs()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-"))
(add-hook 'python-mode 'load-ropemacs)

(setq ropemacs-confirm-saving 'nil)
(evil-leader/set-key-for-mode 'python-mode
  ;; Rope project shortcuts
  "mpo" 'rope-open-project
  "mpk" 'rope-close-project
  "mpf" 'rope-find-file
  "mp4f" 'rope-find-file-other-window
  "mpu" 'rope-undo
  "mpr" 'rope-redo
  "mpc" 'rope-project-config
  "mpnm" 'rope-create-module
  "mpnp" 'rope-create-package
  "mpnf" 'rope-create-file
  "mpnd" 'rope-create-directory
  ;; Refactorings
  "mrr" 'rope-rename
  "mrl" 'rope-extract-variable
  "mrm" 'rope-extract-method
  "mrv" 'rope-move
  "mrx" 'rope-restructure
  "mru" 'rope-use-function
  "mrf" 'rope-introduce-factory
  "mrs" 'rope-change-signature
  "mr1r" 'rope-rename-current-module
  "mr1v" 'rope-move-current-module
  "mr1p" 'rope-module-to-package
  "mro" 'rope-organize-imports
  ;; Generate (variable|function|class|module|package)
  "mrnv" 'rope-generate-variable
  "mrnf" 'rope-generate-function
  "mrnc" 'rope-generate-class
  "mrnm" 'rope-generate-module
  "mrnp" 'rope-generate-package
  ;; Assist
  "mra/" 'rope-code-assist
  "mrag" 'rope-goto-definition
  "mrad" 'rope-show-doc
  "mraf" 'rope-find-occurrences
  "mrai" 'rope-find-implementations
  "mra?" 'rope-lucky-assist
  "mraj" 'rope-jump-to-global
  "mrac" 'rope-show-calltip
  "mraa" 'rope-analyze-module)
