;;; ai.el --- AI-assisted editing dependencies -*- no-byte-compile: t; -*-

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
