function __fzf_help --description 'Display FZF keybinding help'
    printf '%s\n' \
        'Keybindings Help:' \
        '  ?           : Toggle the preview window.' \
        '  alt + j     : Move the preview window down.' \
        '  alt + k     : Move the preview window up.' \
        '  ctrl + d    : Scroll the preview window page down.' \
        '  ctrl + u    : Scroll the preview window page up.' \
        '  ctrl + a    : Select all items.' \
        '  ctrl + e    : Open the selected file in Emacs (silently).' \
        '  ctrl + o    : Open the selected file or directory with the default application.' \
        '  ctrl + s    : Toggle sorting of the results.' \
        '  ctrl + v    : Open the selected file in NeoVim.' \
        '  ctrl + y    : Copy the selected file path to the clipboard.' \
        '  shift + tab : Move the selection up.' \
        '  tab         : Move the selection down.' \
        '  ctrl + h    : Show this help message.' \
        | less -R
end
