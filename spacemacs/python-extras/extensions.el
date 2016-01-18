(setq python-extras-pre-extensions
      '(
        ;; pre extension names go here
        ))

(setq python-extras-post-extensions
      '(
        pymacs
        ))

(defun python-extras/init-pymacs()
  (use-package pymacs
    :defer t
    :commands pymacs-load
    :config
    ;; ropemacs rope pymacs needs to be installed manually for python
    (defvar is-ropemacs-loaded nil "True if ropemacs already loaded")
    (defun load-ropemacs()
      "Load pymacs and ropemacs"
      (interactive)
      (unless is-ropemacs-loaded
        (pymacs-load "ropemacs" "rope-")
        (setq is-ropemacs-loaded t)))
    (add-hook 'python-mode-hook 'load-ropemacs)

    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      ;; Rope project shortcuts
      "po" 'rope-open-project
      "pk" 'rope-close-project
      "pf" 'rope-find-file
      "p4f" 'rope-find-file-other-window
      "pu" 'rope-undo
      "pr" 'rope-redo
      "pc" 'rope-project-config
      "pnm" 'rope-create-module
      "pnp" 'rope-create-package
      "pnf" 'rope-create-file
      "pnd" 'rope-create-directory
      ;; Refactorings
      "rr" 'rope-rename
      "rl" 'rope-extract-variable
      "rm" 'rope-extract-method
      "rv" 'rope-move
      "rx" 'rope-restructure
      "ru" 'rope-use-function
      "rf" 'rope-introduce-factory
      "rs" 'rope-change-signature
      "r1r" 'rope-rename-current-module
      "r1v" 'rope-move-current-module
      "r1p" 'rope-module-to-package
      "ro" 'rope-organize-imports
      ;; Generate (variable|function|class|module|package)
      "rnv" 'rope-generate-variable
      "rnf" 'rope-generate-function
      "rnc" 'rope-generate-class
      "rnm" 'rope-generate-module
      "rnp" 'rope-generate-package
      ;; Assist
      "ra/" 'rope-code-assist
      "rag" 'rope-goto-definition
      "rad" 'rope-show-doc
      "raf" 'rope-find-occurrences
      "rai" 'rope-find-implementations
      "ra?" 'rope-lucky-assist
      "raj" 'rope-jump-to-global
      "rac" 'rope-show-calltip
      "raa" 'rope-analyze-module)))
