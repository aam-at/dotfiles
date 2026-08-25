;;; funcs.el --- Spacemacs layer shim for shared LaTeX helpers -*- lexical-binding: t; -*-

(load (expand-file-name "../../funcs/aam-latex.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil 'nomessage)
