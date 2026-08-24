function fish_greeting
    set -l text_color (set_color --bold brwhite)
    set -l reset_color (set_color normal)

    if type -q fastfetch
        command fastfetch
    else
        printf '%sFastfetch not found › enjoy your shell.%s\n' $text_color $reset_color
    end

    set_color normal
end
