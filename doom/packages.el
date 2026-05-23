;; -*- no-byte-compile: t; -*-
;;; packages.el

;; Additional packages and private-layer dependencies translated from Spacemacs.

(package! exec-path-from-shell)
(package! gcmh)
(package! keychain-environment)
(package! magit-annex)
(package! unfill)

;; aam layer
(package! activity-watch-mode)
(package! casual)
(package! cloc)
(package! explain-pause-mode
  :recipe (:host github :repo "lastquestion/explain-pause-mode"))
(package! ewmctrl)
(package! fish-completion)
(package! gscholar-bibtex)
(package! key-chord)
(package! key-seq)
(package! memoize)
(package! popper)
(package! prescient)
(package! company-prescient)
(package! unicode-math-input
  :recipe (:host github :repo "astoff/unicode-math-input.el"))
(package! pretty-hydra)
(package! ultra-scroll
  :recipe (:host github :repo "jdtsmith/ultra-scroll"))

;; Org extras and roam extras
(package! cdlatex)
(package! org-super-agenda)
(package! org-protocol-capture-html
  :recipe (:host github :repo "alphapapa/org-protocol-capture-html"))
(package! magit-org-todos)
(package! org-doing)
(package! org-doing-notifier
  :recipe (:local-repo "../org-extras/local/org-doing-notifier"))
(package! org-mru-clock)
(package! org-transclusion)
(package! org-gcal
  :recipe (:host github :repo "kidd/org-gcal.el"))
(package! org-ref)
(package! org-roam-bibtex)
(package! org-roam-ui)
(package! vulpea)
(package! websocket)
(package! delve
  :recipe (:host github :repo "publicimageltd/delve"))
(package! org-similarity
  :recipe (:host github :repo "aam-at/org-similarity"))
(package! org-fragtog)
(package! org-pdftools)
(package! org-noter-pdftools)

;; Writing layer
(package! flycheck-vale)
(package! writeroom-mode)
(package! jinx)
(package! powerthesaurus)
(package! le-thesaurus)
(package! mw-thesaurus
  :recipe (:host github :repo "agzam/mw-thesaurus.el"))
(package! synosaurus)
(package! academic-phrases)
(package! write-or-die
  :recipe (:local-repo "../writing/local/write-or-die"))
(package! words
  :recipe (:local-repo "../writing/local/words"))

;; AI layer
(package! shell-maker)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! copilot-chat
  :recipe (:host github :repo "chep/copilot-chat.el" :files ("*.el")))
(package! esi-dictate
  :recipe (:host github :repo "lepisma/emacs-speech-input" :files ("*.el" "*.py")))
(package! khoj
  :recipe (:host github :repo "aam-at/khoj" :files ("src/interface/emacs/*.el")))
(package! llm)
(package! ellama)
(package! magit-gptcommit)
(package! whisper)

;; Misc Spacemacs layer equivalents not covered by Doom modules.
(package! ebib)
(package! elfeed-goodies)
(package! org-contacts)
(package! org-remark)
(package! org-appear)
(package! org-download)
(package! org-mime)
(package! org-cliplink)
(package! pandoc-mode)
(package! reformatter)
(package! ruff-format)
(package! polymode)
(package! poly-markdown)
