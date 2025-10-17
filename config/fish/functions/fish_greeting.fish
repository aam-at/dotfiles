function fish_greeting
    set -l banner_color (set_color --bold brcyan)
    set -l text_color (set_color --bold brwhite)
    set -l reset_color (set_color normal)

    set -l banner \
        '     ______           __          __  _       ' \
        '    / ____/___ ____  / /__  _____/ /_(_)___ _ ' \
        '   / /   / __ `/ _ \/ / _ \/ ___/ __/ / __ `/ ' \
        '  / /___/ /_/ /  __/ /  __(__  ) /_/ / /_/ /  ' \
        '  \____/\__,_/\___/_/\___/____/\__/_/\__,_/   '

    for line in $banner
        printf '%s%s%s\n' $banner_color $line $reset_color
    end

    if type -q fastfetch
        command fastfetch --key-padding-left 5
    else
        printf '%sFastfetch not found › enjoy your shell.%s\n' $text_color $reset_color
    end

    set_color normal
end
