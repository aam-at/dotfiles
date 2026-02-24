#!/usr/bin/env bash
# setup/lib.sh — shared helpers for distro install scripts
# Source this file; do not execute directly.

# ── Font helpers ─────────────────────────────────────────────

get_font_dir() {
    local subdirectory="$1"
    local base_font_dir
    if [ "$(uname)" = "Darwin" ]; then
        base_font_dir="$HOME/Library/Fonts"
    else
        base_font_dir="$HOME/.local/share/fonts"
    fi
    if [ -n "$subdirectory" ]; then
        echo "$base_font_dir/$subdirectory"
    else
        echo "$base_font_dir"
    fi
}

install_fonts() {
    local source_dir="$1"
    local prefix="$2"
    local subdirectory="$3"

    if [ -z "$source_dir" ]; then
        echo "Error: Please provide the source directory as an argument." >&2
        return 1
    fi

    local font_dir
    font_dir=$(get_font_dir "$subdirectory")
    mkdir -p "$font_dir"

    echo "Copying fonts from $source_dir to $font_dir..."
    if [ -n "$prefix" ]; then
        find "$source_dir" \( -name "$prefix*.[ot]tf" -or -name "$prefix*.pcf.gz" \) -type f -print0 | xargs -0 -n1 -I % cp "%" "$font_dir/"
    else
        find "$source_dir" \( -name "*.[ot]tf" -or -name "*.pcf.gz" \) -type f -print0 | xargs -0 -n1 -I % cp "%" "$font_dir/"
    fi

    if command -v fc-cache >/dev/null 2>&1; then
        echo "Resetting font cache, this may take a moment..."
        fc-cache -f "$font_dir"
    fi
    echo "Fonts installed to $font_dir"
}

install_font_packages() {
    local tools_dir="$1"

    if [ -z "$tools_dir" ]; then
        echo "Error: tools_dir argument is required." >&2
        return 1
    fi
    mkdir -p "$tools_dir"

    local font_packages=(
        "https://github.com/JetBrains/JetBrainsMono/|jetbrains-fonts|JetBrainsFonts"
        "https://github.com/adobe-fonts/source-code-pro|adobe-source-code-pro-fonts|AdobeFonts"
        "https://github.com/domtronn/all-the-icons.el|all-icons-fonts|AllIconsFonts"
        "https://github.com/iaolo/iA-Fonts|iawriter-fonts|iAWriterFonts"
        "https://github.com/powerline/fonts|powerline-fonts|PowerlineFonts"
        "https://github.com/ryanoasis/nerd-fonts|nerd-fonts|NerdFonts"
        "https://github.com/sebastiencs/icons-in-terminal|icons-fonts|IconsFonts"
    )

    for package in "${font_packages[@]}"; do
        IFS='|' read -r repo_url dir_name font_subdir <<<"$package"
        if [ ! -d "$tools_dir/$dir_name" ]; then
            echo "Cloning $dir_name..."
            git clone --depth=1 "$repo_url" "$tools_dir/$dir_name"
        fi
        local font_dir
        font_dir=$(get_font_dir "$font_subdir")
        if [ ! -d "$font_dir" ]; then
            install_fonts "$tools_dir/$dir_name" "" "$font_subdir"
        else
            echo "Skipping $font_subdir installation as it is already installed"
        fi
    done

    # Cochineal font
    if [ ! -d "$tools_dir/cochineal-fonts" ]; then
        wget https://mirrors.ctan.org/fonts/cochineal.zip -O /tmp/cochineal.zip
        unzip -o /tmp/cochineal.zip -d "$tools_dir/cochineal-fonts"
        rm -f /tmp/cochineal.zip
    fi
    local cochineal_dir
    cochineal_dir=$(get_font_dir "CochinealFonts")
    if [ ! -d "$cochineal_dir" ]; then
        install_fonts "$tools_dir/cochineal-fonts" "" "CochinealFonts"
    else
        echo "Skipping CochinealFonts installation as it is already installed"
    fi
}
