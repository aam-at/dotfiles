;; This file configures llms for use.

;;;###autoload
(defun my-ai-setup ()
  (require 'llm-openai)
  (require 'llm-ollama)
  ;; ellama settings
  (setopt ellama-language "Russian"
          ellama-naming-scheme 'ellama-generate-name-by-llm
          ellama-sessions-directory (aam/org-path "drafts")
          ellama-keymap-prefix "C-c e")
  (setopt ellama-provider
          (make-llm-ollama
           ;; this model should be pulled to use it
           ;; value should be the same as you print in terminal during pull
           :chat-model "llama3:8b-instruct-q8_0"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-providers
          '(
            ;; deepseek models
            ("deepseek-chat" . (make-llm-openai-compatible
                                :key deepseek-api-key
                                :chat-model "deepseek-chat"
                                :url "https://api.deepseek.com/v1"))
            ("deepseek-coder" . (make-llm-openai-compatible
                                 :key deepseek-api-key
                                 :chat-model "deepseek-coder"
                                 :url "https://api.deepseek.com/v1"))
            ;; ollama models
            ("llama3:8b" . (make-llm-ollama
                            :chat-model "llama3:8b"
                            :embedding-model "nomic-embed-text"))
            ("llama3:8b-instruct" . (make-llm-ollama
                                     :chat-model "llama3:8b-instruct-q8_0"
                                     :embedding-model "nomic-embed-text"))
            ("llama3:70b" . (make-llm-ollama
                             :chat-model "llama3:70b"
                             :embedding-model "nomic-embed-text"))
            ("phi3:3.8b" . (make-llm-ollama
                            :chat-model "phi3:3.8b"
                            :embedding-model "phi3:3.8b"))
            ("phi3:14b" . (make-llm-ollama
                           :chat-model "phi3:14b"
                           :embedding-model "phi3:14b"))
            ("wizardlm2:7b" . (make-llm-ollama
                               :chat-model "wizardlm2:7b"
                               :embedding-model "wizardlm2:7b"))
            ("wizardlm2:8x22b" . (make-llm-ollama
                                  :chat-model "wizardlm2:8x22b"
                                  :embedding-model "wizardlm2:8x22b"))
            ("command-r" . (make-llm-ollama
                            :chat-model "command-r"
                            :embedding-model "command-r"))
            ("command-r-plus" . (make-llm-ollama
                                 :chat-model "command-r-plus"
                                 :embedding-model "command-r-plus"))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
          (make-llm-ollama
           :chat-model "llama3:8b-instruct-q8_0"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("stop" . ("\n")))))
  ;; Translation llm provider
  (setopt ellama-translation-provider (make-llm-ollama
                                       :chat-model "phi3:14b-medium-128k-instruct-q6_K"
                                       :embedding-model "nomic-embed-text"))

  ;; gptel settings
  (setq gptel-model "llama3:8b"
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '("llama3:8b"))
        gptel-default-mode 'org-mode
        gptel-expert-commands t
        gptel-org-branching-context t)
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '("llama3:8b"
              "llama3:70b"
              "phi3:3.8b"
              "phi3:14b"
              "wizardlm2:7b"
              "wizardlm2:8x22b"
              "command-r"
              "command-r-plus"))

  (gptel-make-openai "TogetherAI"
    :host "api.together.xyz"
    :key together-ai-api-key
    :stream t
    :models '("meta-llama/Llama-3-8b-chat-hf"
              "meta-llama/Llama-3-70b-chat-hf"
              "microsoft/WizardLM-2-8x22B"))

  (gptel-make-openai "Deepseek"
    :host "api.deepseek.com"
    :key deepseek-api-key
    :stream t
    :models '("deepseek-chat"
              "deepseek-coder"))
  ;; llm settings
  ;; (require 'llm-ollama)
  ;; (setq magit-gptcommit-llm-provider
  ;;       (make-llm-ollama :embedding-model "nomic-embed-text:latest"
  ;;                        :chat-model "llama3:latest"
  ;;                        :default-chat-temperature 0.1))
  (setq magit-gptcommit-llm-provider
        (make-llm-openai-compatible :default-chat-temperature 1.0
                                    :key deepseek-api-key
                                    :chat-model "deepseek-coder"
                                    :url "https://api.deepseek.com/v1"))

  ;; whisper settings
  (setq whisper-install-directory "~/local/tools/"
        whisper-model
        (cond
         ((>= (gpu-memory-gb) 6) "large-v3")
         ((>= (gpu-memory-gb) 4) "medium")
         (t "small"))
        whisper-language "en"
        whisper-translate nil
        whisper-use-threads (/ (num-processors) 2)))

(provide 'config-ai)
