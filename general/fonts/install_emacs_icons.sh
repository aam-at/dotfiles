#!/bin/bash

# Set source and target directories
emacs_fonts_dir=$( cd emacs-icons/fonts && pwd )

find_command="find \"$emacs_fonts_dir\" \( -name '*.[o,t]tf' -or -name '*.pcf.gz' \) -type f -print0"

if [[ `uname` == 'Darwin' ]]; then
    # MacOS
    font_dir="$HOME/Library/Fonts"
else
    # Linux
    font_dir="$HOME/.local/share/fonts"
    mkdir -p $font_dir
fi

# Copy all fonts to user fonts directory
echo "Copying fonts..."
eval $find_command | xargs -0 -I % cp "%" "$font_dir/"

# Reset font cache on Linux
if command -v fc-cache @>/dev/null ; then
    echo "Resetting font cache, this may take a moment..."
    fc-cache -f $font_dir
fi
echo $emacs_fonts_dir

echo "All Emacs-icons fonts installed to $font_dir"
