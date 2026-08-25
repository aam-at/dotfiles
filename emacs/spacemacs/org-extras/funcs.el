;;; funcs.el --- Spacemacs layer shim for shared Org helpers -*- lexical-binding: t; -*-

(load (expand-file-name "../../funcs/aam-org.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil 'nomessage)
