#!/bin/bash

# Set source directories
fonts_dir="$(pwd)"

# Set target directories
if test "$(uname)" = "Darwin" ; then
    # MacOS
    font_dir="$HOME/Library/Fonts"
else
    # Linux
    font_dir="$HOME/.local/share/fonts"
    mkdir -p $font_dir
fi

# Copy all fonts to user fonts directory
echo "Copying fonts..."
find "$fonts_dir" \( -name "$prefix*.[ot]tf" -or -name "$prefix*.pcf.gz" \) -type f -print0 | xargs -0 -n1 -I % cp "%" "$font_dir/"

# Reset font cache on Linux
if which fc-cache >/dev/null 2>&1 ; then
    echo "Resetting font cache, this may take a moment..."
    fc-cache -f "$font_dir"
fi

echo "Fonts are installed to $font_dir"
