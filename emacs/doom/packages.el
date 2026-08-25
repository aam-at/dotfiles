;; -*- no-byte-compile: t; -*-
;;; packages.el

;; Keep the Doom package-manager entrypoint intentionally small.  Each
;; manifest maps one shared feature domain to Doom package declarations.
(load! "packages/core")
(load! "packages/aam")
(load! "packages/org")
(load! "packages/writing")
(load! "packages/ai")
(load! "packages/extras")
