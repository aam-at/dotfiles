;;; funcs.el --- Spacemacs layer shim for shared Org-roam helpers -*- lexical-binding: t; -*-

(load (expand-file-name "../../funcs/aam-org-roam.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil 'nomessage)
