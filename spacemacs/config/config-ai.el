;; This file configures llms for use.
(defun my-ai-setup ()
  ;; gptel settings
  (setq gptel-model "llama3:8b"
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '("llama3:8b"))
        gptel-default-mode 'org-mode
        gptel-expert-commands t)
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
              "deepseek-coder")))

(provide 'config-ai)
