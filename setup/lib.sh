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

# ── Ollama ───────────────────────────────────────────────────

install_ollama() {
    if ! command -v ollama &>/dev/null; then
        echo "Installing ollama..."
        curl -fsSL https://ollama.com/install.sh | sh
    fi
    echo "Downloading ollama models..."
    local ollama_models=(
        # coding
        "qwen2.5-coder:3b" "qwen2.5-coder:7b"
        # llm
        "gemma3:4b" "gemma3:12b" "phi4:mini"
        # embedding
        "granite-embedding:278m" "mxbai-embed-large:latest" "nomic-embed-text:latest"
    )
    for model in "${ollama_models[@]}"; do
        ollama pull "$model" || echo "Warning: failed to pull $model" >&2
    done
}

# ── Spacemacs / Intellimacs ──────────────────────────────────

install_spacemacs() {
    if [ ! -d "$HOME/.emacs.d" ]; then
        echo "Installing Spacemacs..."
        git clone https://github.com/aam-at/spacemacs ~/.emacs.d
    else
        echo "Spacemacs already installed."
    fi
}

install_intellimacs() {
    if [ ! -d "$HOME/.intellimacs" ]; then
        echo "Installing Intellimacs..."
        git clone https://github.com/MarcoIeni/intellimacs ~/.intellimacs
    else
        echo "Intellimacs already installed."
    fi
}

# ── fzf ──────────────────────────────────────────────────────

install_fzf() {
    if [ ! -d "$HOME/.fzf" ]; then
        echo "Installing fzf..."
        git clone --depth 1 https://github.com/junegunn/fzf.git "$HOME/.fzf"
        "$HOME/.fzf/install" --all
    else
        echo "fzf already installed."
    fi
}

# ── oh-my-fish ───────────────────────────────────────────────

install_omf() {
    if [ ! -d "$HOME/.config/omf" ]; then
        echo "Installing oh-my-fish (omf)..."
        curl -fsSL https://raw.githubusercontent.com/oh-my-fish/oh-my-fish/master/bin/install | fish || true
    else
        echo "oh-my-fish already installed."
    fi
}

# ── Argument parsing ─────────────────────────────────────────

# Sets INSTALL_* and GUI variables. Call after defining defaults.
# Usage: parse_common_args "$@"
parse_common_args() {
    while [ $# -gt 0 ]; do
        case $1 in
        --gui)
            GUI=true
            shift
            ;;
        --ollama)
            INSTALL_OLLAMA=true
            shift
            ;;
        --no-python)
            INSTALL_PYTHON=false
            shift
            ;;
        --no-rust)
            INSTALL_RUST=false
            shift
            ;;
        --no-go)
            INSTALL_GO=false
            shift
            ;;
        --no-node)
            INSTALL_NODE=false
            shift
            ;;
        --no-lua)
            INSTALL_LUA=false
            shift
            ;;
        --no-emacs)
            INSTALL_EMACS=false
            shift
            ;;
        --no-fonts)
            INSTALL_FONTS=false
            shift
            ;;
        --help | -h)
            echo "Usage: $0 [OPTIONS]"
            echo "  --gui          Install GUI packages"
            echo "  --ollama       Install Ollama + models"
            echo "  --no-python    Skip Python toolchain"
            echo "  --no-rust      Skip Rust toolchain"
            echo "  --no-go        Skip Go toolchain"
            echo "  --no-node      Skip Node.js toolchain"
            echo "  --no-lua       Skip Lua toolchain"
            echo "  --no-emacs     Skip Emacs/Spacemacs"
            echo "  --no-fonts     Skip font installation"
            exit 0
            ;;
        *)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
        esac
    done
}
