function aichat --description 'Launch aichat with secrets pulled from the password store'
    if not type -q copy_password.sh
        echo 'aichat: copy_password.sh is required but not available.' >&2
        return 127
    end

    if not type -q aichat
        echo 'aichat: the aichat CLI is not installed.' >&2
        return 127
    end

    set -l deepseek (copy_password.sh deepseek 2>/dev/null)
    set -l gemini (copy_password.sh gemini 2>/dev/null)

    env DEEPSEEK_API_KEY=$deepseek GEMINI_API_KEY=$gemini command aichat $argv
end
