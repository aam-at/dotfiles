;; This file configures llms for use.

;;;###autoload
(defun my-ai-setup ()
  (require 'llm-openai)
  (require 'llm-gemini)
  (require 'llm-ollama)
  ;; esi dictate settings
  (setq esi-dictate-dg-api-key deepgram-api-key)
  (setq esi-dictate-llm-provider (make-llm-openai
                                  :key openai-api-key
                                  :chat-model "gpt-4o-mini"))
  ;; (setq esi-dictate-llm-provider (make-llm-openai-compatible
  ;;                                 :key github-api-key
  ;;                                 :chat-model "gpt-4o-mini"
  ;;                                 :url  "models.inference.ai.azure.com/chat/completions"))

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
          ;; deepinfra models
          ("deepinfra-llama3.1:8b" . (make-llm-openai-compatible
                                      :key deepinfra-api-key
                                      :chat-model "meta-llama/Meta-Llama-3.1-8B-Instruct"
                                      :url "https://api.deepinfra.com/v1/openai"))
          ("deepinfra-llama3.1:70b" . (make-llm-openai-compatible
                                       :key deepinfra-api-key
                                       :chat-model "meta-llama/Meta-Llama-3.1-70B-Instruct"
                                       :url "https://api.deepinfra.com/v1/openai"))
          ("deepinfra-llama3.1:405b" . (make-llm-openai-compatible
                                        :key deepinfra-api-key
                                        :chat-model "meta-llama/Meta-Llama-3.1-405B-Instruct"
                                        :url "https://api.deepinfra.com/v1/openai"))
          ;; github models
          ("github-gpt4o" . (make-llm-openai-compatible
                             :key github-api-key
                             :chat-model "gpt-4o"
                             :url "models.inference.ai.azure.com/chat/completions"))
          ("github-gpt4o-mini" . (make-llm-openai-compatible
                                  :key github-api-key
                                  :chat-model "gpt-4o-mini"
                                  :url "models.inference.ai.azure.com/chat/completions"))
          ;; groq models
          ("groq-llama3.1:8b" . (make-llm-openai-compatible
                                 :key groq-api-key
                                 :chat-model "llama-3.1-8b-instant"
                                 :url "https://api.groq.com/openai/v1/"))
          ("groq-llama3.1:70b" . (make-llm-openai-compatible
                                  :key groq-api-key
                                  :chat-model "llama-3.1-70b-versatile"
                                  :url "https://api.groq.com/openai/v1"))
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
          elisa-prompt-rewriting-enabled t ;; prompt rewriting may increase quality of answers
          elisa-reranker-enabled t ;; reranker increases answer quality
          elisa-web-search-function 'elisa-search-searxng) ;; searxng works better than duckduckgo in my tests
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
        gptel-log-level 'nil
        gptel-default-mode 'org-mode
        gptel-expert-commands t
        gptel-org-branching-context t)
  (gptel-make-openai "OpenAI"
    :stream t
    :key openai-api-key
    :models '("gpt-4o-mini"
              "gpt-4o"
              "gpt-4o-2024-08-06"
              "o1-preview"
              "o1-mini"))
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
  (gptel-make-openai "Github Models"
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions"
    :stream t
    :key github-api-key
    :models '("DeepSeek-R1"
              "gpt-4o"
              "gpt-4o-mini"
              "Llama-3.3-70B-Instruct"
              "o1-mini"
              "o1-preview"))
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key openrouter-api-key
    :models '("anthropic/claude-3.5-sonnet"
              "deepseek/deepseek-r1:free"
              "deepseek/deepseek-r1"
              "google/gemini-pro-1.5-exp"
              "openai/gpt-4o-mini"
              "openai/gpt-4o"
              "openai/gpt-4o-2024-08-06"
              "openai/o1-mini"
              "openai/o1-preview"
              "perplexity/llama-3.1-sonar-huge-128k-online"
              "perplexity/llama-3.1-sonar-large-128k-online"
              "perplexity/llama-3.1-sonar-small-128k-online"))

  (gptel-make-openai "TogetherAI"
    :host "api.together.xyz"
    :key together-ai-api-key
    :stream t
    :models '("meta-llama/Llama-3-8b-chat-hf"
              "meta-llama/Llama-3-70b-chat-hf"
              "microsoft/WizardLM-2-8x22B"))

  (gptel-make-openai "Groq"
    :host "api.groq.com"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key groq-api-key
    :models '("deepseek-r1-distill-llama-70b"
              "llama-3.3-70b-versatile"
              "llama-3.1-8b-instant"))

  (gptel-make-openai "Deepinfra"
    :host "api.deepinfra.com"
    :endpoint "/v1/openai/chat/completions"
    :stream t
    :key deepinfra-api-key
    :models '("meta-llama/Meta-Llama-3.1-405B-Instruct"
              "meta-llama/Meta-Llama-3.1-70B-Instruct"
              "meta-llama/Meta-Llama-3.1-8B-Instruct"))

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

  ;; copilot-chat settings
  (setq copilot-chat-frontend 'shell-maker
        copilot-chat-prompt (concat "You are a world-class coding tutor. Your code explanations perfectly balance high-level concepts and granular details. Your approach ensures that students not only understand how to write code, but also grasp the underlying principles that guide effective programming.\n"
                                    "When asked for your name, you must respond with \"GitHub Copilot\".\n"
                                    "Follow the user's requirements carefully & to the letter.\n"
                                    "Your expertise is strictly limited to software development topics.\n"
                                    "Follow Microsoft content policies.\n"
                                    "Avoid content that violates copyrights.\n"
                                    "For questions not related to software development, simply give a reminder that you are an AI programming assistant.\n"
                                    "Keep your answers short and impersonal.\n"
                                    "Use Markdown formatting in your answers.\n"
                                    "Make sure to include the programming language name at the start of the Markdown code blocks.\n"
                                    "Avoid wrapping the whole response in triple backticks.\n"
                                    "The user works in an IDE called Emacs which has a concept for editors with open files, integrated unit test support, an output pane that shows the output of running the code as well as an integrated terminal.\n"
                                    "The active document is the source code the user is looking at right now.\n"
                                    "You can only give one reply for each conversation turn.\n\n"
                                    "Additional Rules\n"
                                    "Think step by step:\n"
                                    "1. Examine the provided code selection and any other context like user question, related errors, project details, class definitions, etc.\n"
                                    "2. If you are unsure about the code, concepts, or the user's question, ask clarifying questions.\n"
                                    "3. If the user provided a specific question or error, answer it based on the selected code and additional provided context. Otherwise focus on explaining the selected code.\n"
                                    "4. Provide suggestions if you see opportunities to improve code readability, performance, etc.\n\n"
                                    "Focus on being clear, helpful, and thorough without assuming extensive prior knowledge.\n"
                                    "Use developer-friendly terms and analogies in your explanations.\n"
                                    "Identify 'gotchas' or less obvious parts of the code that might trip up someone new.\n"
                                    "Provide clear and relevant examples aligned with any provided context.\n")
        copilot-chat-model "claude-3.5-sonnet")

  ;; khoj settings
  (setq khoj-server-is-local t
        khoj-auto-index nil
        khoj-server-url "http://localhost:42110"
        khoj-index-files-batch 1
        khoj-default-content-type "org"
        khoj-index-files (directory-files-recursively aam/org-root (rx ".org" eos))
        khoj-index-directories nil))

(provide 'config-ai)
