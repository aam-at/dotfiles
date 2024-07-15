function aichat
    set -l DEEPSEEK_API_KEY (copy_password.sh deepseek)
    set -l GEMINI_API_KEY (copy_password.sh gemini)
    env DEEPSEEK_API_KEY=$DEEPSEEK_API_KEY GEMINI_API_KEY=$GEMINI_API_KEY aichat $argv
end
