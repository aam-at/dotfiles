;; This file configures stub for secure part of the config for use in case
;; decryption fails.

;;;###autoload
(defun my-secure-stub-setup()
  (setq deepinfra-api-key "deepinfra-api-key")
  (setq deepseek-api-key "deepseek-api-key")
  (setq gemini-api-key "gemini-api-key")
  (setq groq-api-key "groq-api-key")
  (setq jina-api-key "jina-api-key")
  (setq khoj-api-key "khoj-api-key")
  (setq nvidia-api-key "nvidia-api-key")
  (setq openai-api-key "openai-api-key")
  (setq openrouter-api-key "openrouter-api-key")
  (setq together-ai-api-key "together-ai-api-key"))

(provide 'config-secure-stub)
