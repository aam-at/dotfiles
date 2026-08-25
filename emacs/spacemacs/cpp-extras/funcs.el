;;; funcs.el --- Spacemacs layer shim for shared C++ helpers -*- lexical-binding: t; -*-

(load (expand-file-name "../../funcs/aam-cpp.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil 'nomessage)
