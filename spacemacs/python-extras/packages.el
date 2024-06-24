(defconst python-extras-packages
  '(ruff-format))

(defun python-extras/init-ruff-format ()
  (use-package ruff-format
    :defer t
    :init
    :config
    (reformatter-define ruff-isort
      :program ruff-format-command
      :args (list "check" "--select" "I" "--fix" "--stdin-filename" (or (buffer-file-name) input-file))
      :lighter " RuffIsort"
      :group 'ruff-format)))
