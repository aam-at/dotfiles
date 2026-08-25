;;; funcs.el --- Spacemacs layer shim for shared aam helpers -*- lexical-binding: t; -*-

(load (expand-file-name "../../funcs/aam-core.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil 'nomessage)
