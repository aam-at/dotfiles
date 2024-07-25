;; This file configures llms for use.

;;;###autoload
(defun my-ai-setup ()
  (require 'llm-openai)
  (require 'llm-gemini)
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
           :chat-model "llama3.1:8b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 131072))))
  (setq ellama-providers
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
          ;; gemini models
          ("gemini-1.0-pro" . (make-llm-gemini
                               :key gemini-api-key
                               :chat-model "gemini-1.0-pro"))
          ("gemini-1.5-pro" . (make-llm-gemini
                               :key gemini-api-key
                               :chat-model "gemini-1.5-pro"))
          ("gemini-1.5-flash" . (make-llm-gemini
                                 :key gemini-api-key
                                 :chat-model "gemini-1.5-flash"))
          ;; ollama models
          ("gemma2:9b" . (make-llm-ollama
                          :chat-model "gemma2:9b"
                          :embedding-model "nomic-embed-text"))
          ("gemma2:9b-instruct" . (make-llm-ollama
                                   :chat-model "gemma2:9b-instruct-q6_K"
                                   :embedding-model "nomic-embed-text"))
          ("glm4:9b" . (make-llm-ollama
                        :chat-model "glm4:9b"
                        :embedding-model "nomic-embed-text"))
          ("llama3.1:8b" . (make-llm-ollama
                            :chat-model "llama3.1:8b"
                            :embedding-model "nomic-embed-text"))
          ("llama3.1:8b-instruct" . (make-llm-ollama
                                     :chat-model "llama3.1:8b-instruct-q8_0"
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
           :chat-model "llama3.1:8b-instruct-q8_0"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("stop" . ("\n")))))

  ;; Elisa config
  (setopt elisa-limit 5
          elisa-web-search-function 'elisa-search-searxng ;; searxng works better than duckduckgo in my tests
          elisa-prompt-rewriting-enabled t ;; prompt rewriting may increase quality of answers
          elisa-reranker-enabled t) ;; reranker increases answer quality
  (setopt elisa-chat-provider
          (make-llm-ollama
           :chat-model "llama3.1:8b-instruct-q8_0"
           :embedding-model "chatfire/bge-m3:q8_0"
           :default-chat-non-standard-params '(("num_ctx" . 131072))))
  (setopt elisa-embeddings-provider (make-llm-ollama :embedding-model "chatfire/bge-m3:q8_0"))
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
        gptel-default-mode 'org-mode
        gptel-expert-commands t
        gptel-org-branching-context t)
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '("gemma2:9b"
              "gemma2:9b-instruct-q6_K"
              "glm4:9b"
              "llama3.1:8b"
              "llama3.1:8b-instruct-q8_0"
              "phi3:3.8b"
              "phi3:14b"
              "wizardlm2:7b"
              "wizardlm2:8x22b"
              "command-r"
              "command-r-plus"))
  (gptel-make-gemini "Gemini"
    :key gemini-api-key
    :stream t
    :models '("gemini-1.0-pro"
              "gemini-1.5-pro"
              "gemini-1.5-flash"))

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
  ;; (setq magit-gptcommit-llm-provider
  ;;       (make-llm-ollama :embedding-model "nomic-embed-text:latest"
  ;;                        :chat-model "llama3.1:latest"
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
        whisper-use-threads (/ (num-processors) 2))

  ;; khoj setings
  (setq khoj-server-is-local t
        khoj-server-url "http://localhost:42110"
        khoj-index-files-batch 2
        khoj-index-directories (list aam/org-root)))

(provide 'config-ai)
