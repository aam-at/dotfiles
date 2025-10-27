#!/usr/bin/env bash

# Modern Linux Installation System - Base Package Manager
# Abstract base class for all package managers

# Package manager interface
declare -A PACKAGE_MANAGERS

# Base package manager class
class PackageManager {
    local name=""
    local available=false
    local update_command=""
    local install_command=""
    local remove_command=""
    local search_command=""
    local list_command=""
    
    # Constructor
    PackageManager() {
        local manager_name="$1"
        name="$manager_name"
        log_debug "Initializing package manager: $name"
    }
    
    # Check if package manager is available
    is_available() {
        log_debug "Checking availability of package manager: $name"
        return $available
    }
    
    # Update package database
    update() {
        log_info "Updating package database for $name"
        if [[ -n "$update_command" ]]; then
            log_command "$update_command"
        else
            log_warn "No update command defined for $name"
        fi
    }
    
    # Install packages
    install() {
        local packages=("$@")
        log_info "Installing packages with $name: ${packages[*]}"
        
        if [[ -n "$install_command" ]]; then
            local cmd="$install_command ${packages[*]}"
            log_command "$cmd"
        else
            log_error "No install command defined for $name"
            return 1
        fi
    }
    
    # Remove packages
    remove() {
        local packages=("$@")
        log_info "Removing packages with $name: ${packages[*]}"
        
        if [[ -n "$remove_command" ]]; then
            local cmd="$remove_command ${packages[*]}"
            log_command "$cmd"
        else
            log_error "No remove command defined for $name"
            return 1
        fi
    }
    
    # Search for packages
    search() {
        local query="$1"
        log_debug "Searching for packages with $name: $query"
        
        if [[ -n "$search_command" ]]; then
            local cmd="$search_command $query"
            log_command "$cmd"
        else
            log_warn "No search command defined for $name"
        fi
    }
    
    # List installed packages
    list() {
        log_debug "Listing installed packages with $name"
        
        if [[ -n "$list_command" ]]; then
            log_command "$list_command"
        else
            log_warn "No list command defined for $name"
        fi
    }
    
    # Check if package is installed
    is_installed() {
        local package="$1"
        log_debug "Checking if package is installed: $package"
        
        # This should be implemented by specific package managers
        return 1
    }
    
    # Get package information
    info() {
        local package="$1"
        log_debug "Getting package information: $package"
        
        # This should be implemented by specific package managers
        return 1
    }
}

# Register package manager
register_package_manager() {
    local name="$1"
    local manager="$2"
    
    PACKAGE_MANAGERS["$name"]="$manager"
    log_debug "Registered package manager: $name"
}

# Get package manager
get_package_manager() {
    local name="$1"
    
    if [[ -n "${PACKAGE_MANAGERS[$name]:-}" ]]; then
        echo "${PACKAGE_MANAGERS[$name]}"
    else
        log_error "Package manager not found: $name"
        return 1
    fi
}

# List available package managers
list_package_managers() {
    log_info "Available package managers:"
    for manager in "${!PACKAGE_MANAGERS[@]}"; do
        log_info "  - $manager"
    done
}

# Detect available package managers
detect_package_managers() {
    log_info "Detecting available package managers..."
    
    # Check for APT
    if command -v apt &> /dev/null; then
        register_package_manager "apt" "apt"
        log_info "Found APT package manager"
    fi
    
    # Check for Pacman
    if command -v pacman &> /dev/null; then
        register_package_manager "pacman" "pacman"
        log_info "Found Pacman package manager"
    fi
    
    # Check for YUM/DNF
    if command -v dnf &> /dev/null; then
        register_package_manager "dnf" "dnf"
        log_info "Found DNF package manager"
    elif command -v yum &> /dev/null; then
        register_package_manager "yum" "yum"
        log_info "Found YUM package manager"
    fi
    
    # Check for Zypper
    if command -v zypper &> /dev/null; then
        register_package_manager "zypper" "zypper"
        log_info "Found Zypper package manager"
    fi
    
    # Check for Homebrew (macOS)
    if command -v brew &> /dev/null; then
        register_package_manager "brew" "brew"
        log_info "Found Homebrew package manager"
    fi
    
    # Check for Pip
    if command -v pip &> /dev/null || command -v pip3 &> /dev/null; then
        register_package_manager "pip" "pip"
        log_info "Found Pip package manager"
    fi
    
    # Check for Cargo
    if command -v cargo &> /dev/null; then
        register_package_manager "cargo" "cargo"
        log_info "Found Cargo package manager"
    fi
    
    # Check for NPM
    if command -v npm &> /dev/null; then
        register_package_manager "npm" "npm"
        log_info "Found NPM package manager"
    fi
    
    # Check for Go
    if command -v go &> /dev/null; then
        register_package_manager "go" "go"
        log_info "Found Go package manager"
    fi
}

# Install packages using the best available manager
install_packages() {
    local packages=("$@")
    local manager=""
    
    # Try to find the best package manager for the current system
    if [[ "$DETECTED_OS" == "linux" ]]; then
        case "$DETECTED_DISTRO" in
            "ubuntu"|"debian")
                manager="apt"
                ;;
            "arch"|"manjaro")
                manager="pacman"
                ;;
            "fedora"|"rhel"|"centos")
                if command -v dnf &> /dev/null; then
                    manager="dnf"
                else
                    manager="yum"
                fi
                ;;
            "opensuse")
                manager="zypper"
                ;;
        esac
    elif [[ "$DETECTED_OS" == "macos" ]]; then
        manager="brew"
    fi
    
    if [[ -n "$manager" ]] && [[ -n "${PACKAGE_MANAGERS[$manager]:-}" ]]; then
        log_info "Using package manager: $manager"
        # Call the specific package manager's install function
        case "$manager" in
            "apt") source "$INSTALLER_DIR/managers/apt.sh" && apt_install "${packages[@]}" ;;
            "pacman") source "$INSTALLER_DIR/managers/pacman.sh" && pacman_install "${packages[@]}" ;;
            "dnf") source "$INSTALLER_DIR/managers/dnf.sh" && dnf_install "${packages[@]}" ;;
            "yum") source "$INSTALLER_DIR/managers/yum.sh" && yum_install "${packages[@]}" ;;
            "zypper") source "$INSTALLER_DIR/managers/zypper.sh" && zypper_install "${packages[@]}" ;;
            "brew") source "$INSTALLER_DIR/managers/brew.sh" && brew_install "${packages[@]}" ;;
        esac
    else
        log_error "No suitable package manager found for current system"
        return 1
    fi
}