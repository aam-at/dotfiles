#!/usr/bin/env bash

# Core module installation script
# Installs essential system packages

# Module configuration
MODULE_NAME="core"
MODULE_DESCRIPTION="Core system packages and utilities"

# Load module configuration
load_module_config() {
    local config_file="$INSTALLER_DIR/modules/$MODULE_NAME/packages.yaml"
    
    if [[ ! -f "$config_file" ]]; then
        log_error "Configuration file not found: $config_file"
        return 1
    fi
    
    log_debug "Loading configuration for module: $MODULE_NAME"
    source "$config_file"
}

# Install core packages
install_core_packages() {
    log_section "Installing Core Packages"
    
    # Detect package manager
    local package_manager=""
    if command -v apt &> /dev/null; then
        package_manager="apt"
    elif command -v pacman &> /dev/null; then
        package_manager="pacman"
    else
        log_error "No supported package manager found"
        return 1
    fi
    
    log_info "Using package manager: $package_manager"
    
    # Install system packages
    case "$package_manager" in
        "apt")
            apt_install curl wget git vim nano htop tree unzip zip tar gzip \
                       build-essential software-properties-common \
                       apt-transport-https ca-certificates gnupg lsb-release \
                       net-tools dnsutils iputils-ping traceroute nmap \
                       openssh-client openssh-server
            ;;
        "pacman")
            pacman_install curl wget git vim nano htop tree unzip zip tar gzip \
                          base-devel ca-certificates gnupg lsb-release \
                          net-tools dnsutils iputils traceroute nmap openssh
            ;;
    esac
    
    # Enable SSH service
    if command -v systemctl &> /dev/null; then
        log_info "Enabling SSH service"
        sudo systemctl enable ssh 2>/dev/null || true
    fi
    
    log_info "Core packages installed successfully"
}

# Main installation function
install_module_packages() {
    log_info "Installing module: $MODULE_NAME"
    log_info "Description: $MODULE_DESCRIPTION"
    
    # Load configuration
    load_module_config
    
    # Install packages
    install_core_packages
    
    log_info "Module $MODULE_NAME installed successfully"
}