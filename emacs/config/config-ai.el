;; -*- lexical-binding: t; -*-
;; This file configures llms for use.

(require 'aam-core)

;;;###autoload
(defun aam/ai-setup ()
  (require 'llm-openai)
  (require 'llm-ollama)
  ;; esi dictate settings
  (setq esi-dictate-dg-api-key deepgram-api-key)
  (setq esi-dictate-llm-provider (make-llm-openai
                                  :key openai-api-key
                                  :chat-model "gpt-4o-mini"))
  ;; ellama settings
  (setopt ellama-language "Russian"
          ellama-naming-scheme 'ellama-generate-name-by-llm
          ellama-sessions-directory (aam/org-path "drafts"))
  (setopt ellama-provider
          (make-llm-ollama
           ;; this model should be pulled to use it
           ;; value should be the same as you print in terminal during pull
           :chat-model "llama3.1:8b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 131072))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
          (make-llm-ollama
           :chat-model "llama3.1:8b-instruct-q8_0"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("stop" . ("\n")))))

  ;; Translation llm provider
  (setopt ellama-translation-provider (make-llm-ollama
                                       :chat-model "phi3:14b-medium-128k-instruct-q6_K"
                                       :embedding-model "nomic-embed-text"))

  ;; gptel settings
  (setq gptel-model "llama3.1:8b-instruct"
        gptel-backend (gptel-make-ollama "Ollama"
					 :host "localhost:11434"
					 :stream t
					 :models '("llama3.1:8b-instruct-q8_0"))
        gptel-log-level 'nil
        gptel-default-mode 'org-mode
        gptel-expert-commands t
        gptel-org-branching-context t)
  ;; llm settings
  (setq magit-gptcommit-llm-provider
        (make-llm-openai-compatible :default-chat-temperature 1.0
                                    :key deepseek-api-key
                                    :chat-model "deepseek-coder"
                                    :url "https://api.deepseek.com/v1"))

  ;; whisper settings
  (setq whisper-install-directory "~/local/tools/"
        whisper-model
        (cond
         ((>= (aam/gpu-memory-gb) 6) "large-v3")
         ((>= (aam/gpu-memory-gb) 4) "medium")
         (t "small"))
        whisper-language "en"
        whisper-translate nil
        whisper-use-threads (/ (num-processors) 2))

  ;; copilot-chat settings
  (setq copilot-chat-frontend 'shell-maker
        copilot-chat-model "claude-3.5-sonnet"
        copilot-chat-prompt "You are helpful AI assistant living in Emacs.")

  ;; khoj settings
  (setq khoj-server-is-local t
        khoj-auto-index nil
        khoj-server-url "http://localhost:42111"
        khoj-index-files-batch 1
        khoj-default-content-type "org"
        khoj-index-files (directory-files-recursively aam/org-root (rx ".org" eos))
        khoj-index-directories nil))

(provide 'config-ai)
