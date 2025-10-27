#!/usr/bin/env bash

# AI module installation script
# Installs AI tools, models, and related packages

# Module configuration
MODULE_NAME="ai"
MODULE_DESCRIPTION="AI tools, models, and development packages"

# Install Ollama
install_ollama() {
    log_info "Installing Ollama"
    
    if ! command -v ollama &> /dev/null; then
        log_info "Downloading and installing Ollama"
        curl -fsSL https://ollama.com/install.sh | sh
        
        # Add ollama to PATH
        if [[ -f "$HOME/.bashrc" ]]; then
            echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.bashrc"
        fi
        
        # Source the updated PATH
        export PATH="$HOME/.local/bin:$PATH"
    else
        log_info "Ollama is already installed"
    fi
}

# Install Ollama models
install_ollama_models() {
    log_info "Installing Ollama models"
    
    if ! command -v ollama &> /dev/null; then
        log_error "Ollama is not installed"
        return 1
    fi
    
    # Coding models
    local coding_models=("qwen2.5-coder:3b" "qwen2.5-coder:7b")
    for model in "${coding_models[@]}"; do
        log_info "Pulling coding model: $model"
        ollama pull "$model"
    done
    
    # General models
    local general_models=("gemma3:4b" "gemma3:12b" "phi4:mini")
    for model in "${general_models[@]}"; do
        log_info "Pulling general model: $model"
        ollama pull "$model"
    done
    
    # Embedding models
    local embedding_models=("granite-embedding:278m" "mxbai-embed-large:latest" "nomic-embed-text:latest")
    for model in "${embedding_models[@]}"; do
        log_info "Pulling embedding model: $model"
        ollama pull "$model"
    done
}

# Install Python AI packages
install_python_ai_packages() {
    log_info "Installing Python AI packages"
    
    # Install pip if not available
    if ! command -v pip &> /dev/null && ! command -v pip3 &> /dev/null; then
        log_info "Installing pip"
        if command -v apt &> /dev/null; then
            apt_install python3-pip
        elif command -v pacman &> /dev/null; then
            pacman_install python-pip
        fi
    fi
    
    # Use pip3 if pip is not available
    local pip_cmd="pip"
    if ! command -v pip &> /dev/null && command -v pip3 &> /dev/null; then
        pip_cmd="pip3"
    fi
    
    # Install Python packages
    local python_packages=(
        "aider-chat"
        "gpustat"
        "nvitop"
        "openai"
        "anthropic"
        "google-generativeai"
    )
    
    for package in "${python_packages[@]}"; do
        log_info "Installing Python package: $package"
        $pip_cmd install "$package"
    done
}

# Install Node.js AI packages
install_node_ai_packages() {
    log_info "Installing Node.js AI packages"
    
    # Install Node.js if not available
    if ! command -v node &> /dev/null; then
        log_info "Installing Node.js"
        if command -v apt &> /dev/null; then
            apt_install nodejs npm
        elif command -v pacman &> /dev/null; then
            pacman_install nodejs npm
        fi
    fi
    
    # Install npm packages
    local npm_packages=(
        "@anthropic-ai/claude-code@latest"
        "@github/copilot@latest"
        "@google/gemini-cli@latest"
        "@openai/codex@latest"
        "opencode-ai@latest"
    )
    
    for package in "${npm_packages[@]}"; do
        log_info "Installing npm package: $package"
        sudo npm install -g "$package"
    done
}

# Install Rust AI packages
install_rust_ai_packages() {
    log_info "Installing Rust AI packages"
    
    # Install Rust if not available
    if ! command -v cargo &> /dev/null; then
        log_info "Installing Rust"
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
        source "$HOME/.cargo/env"
    fi
    
    # Install Rust packages
    local rust_packages=("aichat" "cargo-binstall")
    
    for package in "${rust_packages[@]}"; do
        log_info "Installing Rust package: $package"
        cargo install "$package"
    done
}

# Install GitHub Copilot CLI
install_github_copilot() {
    log_info "Installing GitHub Copilot CLI"
    
    if command -v gh &> /dev/null; then
        gh extension install github/gh-copilot
    else
        log_warn "GitHub CLI not found, skipping Copilot CLI installation"
    fi
}

# Main installation function
install_module_packages() {
    log_section "Installing AI Tools and Packages"
    
    # Install system packages
    if command -v apt &> /dev/null; then
        apt_install python3-pip python3-venv nodejs npm curl
    elif command -v pacman &> /dev/null; then
        pacman_install python-pip nodejs npm curl
    fi
    
    # Install AI tools
    install_python_ai_packages
    install_node_ai_packages
    install_rust_ai_packages
    install_ollama
    install_ollama_models
    install_github_copilot
    
    log_info "AI module installed successfully"
}