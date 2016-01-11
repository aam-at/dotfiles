(setq python-extras-pre-extensions
      '(
        pymacs
        ))

(setq python-extras-post-extensions
      '(
        ;; post extension names go here
        ))

(defun python-extras/init-pymacs()
  (use-package pymacs
    :defer t
    :load-path "private/python-extras"))
