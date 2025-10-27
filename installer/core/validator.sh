#!/usr/bin/env bash

# Modern Linux Installation System - Validation Module
# Provides system validation and requirement checking

# System information
DETECTED_OS=""
DETECTED_ARCH=""
DETECTED_DISTRO=""
DETECTED_VERSION=""

# Validate system requirements
validate_system_requirements() {
    log_section "System Validation"
    
    detect_system_info
    validate_os_support
    validate_architecture
    validate_dependencies
    validate_disk_space
    validate_network_connectivity
    
    log_info "System validation completed successfully"
}

# Detect system information
detect_system_info() {
    log_info "Detecting system information..."
    
    # Detect OS
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        DETECTED_OS="linux"
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        DETECTED_OS="macos"
    else
        log_error "Unsupported operating system: $OSTYPE"
        exit 1
    fi
    
    # Detect architecture
    DETECTED_ARCH=$(uname -m)
    
    # Detect Linux distribution
    if [[ "$DETECTED_OS" == "linux" ]]; then
        if [[ -f /etc/os-release ]]; then
            source /etc/os-release
            DETECTED_DISTRO="$ID"
            DETECTED_VERSION="$VERSION_ID"
        elif [[ -f /etc/redhat-release ]]; then
            DETECTED_DISTRO="rhel"
            DETECTED_VERSION=$(cat /etc/redhat-release | grep -oE '[0-9]+' | head -1)
        else
            log_error "Unable to detect Linux distribution"
            exit 1
        fi
    fi
    
    log_info "OS: $DETECTED_OS"
    log_info "Architecture: $DETECTED_ARCH"
    log_info "Distribution: $DETECTED_DISTRO"
    log_info "Version: $DETECTED_VERSION"
}

# Validate OS support
validate_os_support() {
    log_info "Validating OS support..."
    
    case "$DETECTED_OS" in
        "linux")
            case "$DETECTED_DISTRO" in
                "ubuntu"|"debian"|"arch"|"manjaro"|"fedora"|"rhel"|"centos"|"opensuse")
                    log_info "Supported distribution: $DETECTED_DISTRO"
                    ;;
                *)
                    log_warn "Untested distribution: $DETECTED_DISTRO"
                    log_warn "Installation may not work correctly"
                    ;;
            esac
            ;;
        "macos")
            log_info "macOS detected - limited support"
            ;;
        *)
            log_error "Unsupported operating system: $DETECTED_OS"
            exit 1
            ;;
    esac
}

# Validate architecture
validate_architecture() {
    log_info "Validating architecture..."
    
    case "$DETECTED_ARCH" in
        "x86_64"|"amd64")
            log_info "Supported architecture: $DETECTED_ARCH"
            ;;
        "aarch64"|"arm64")
            log_warn "ARM architecture detected - some packages may not be available"
            ;;
        "i386"|"i686")
            log_warn "32-bit architecture detected - limited support"
            ;;
        *)
            log_error "Unsupported architecture: $DETECTED_ARCH"
            exit 1
            ;;
    esac
}

# Validate dependencies
validate_dependencies() {
    log_info "Validating system dependencies..."
    
    local required_commands=("curl" "wget" "git" "tar" "gzip")
    local missing_commands=()
    
    for cmd in "${required_commands[@]}"; do
        if ! command -v "$cmd" &> /dev/null; then
            missing_commands+=("$cmd")
        fi
    done
    
    if [[ ${#missing_commands[@]} -gt 0 ]]; then
        log_error "Missing required commands: ${missing_commands[*]}"
        log_error "Please install missing dependencies before running the installer"
        exit 1
    fi
    
    log_info "All required dependencies are available"
}

# Validate disk space
validate_disk_space() {
    log_info "Validating disk space..."
    
    local required_space_gb=5  # Minimum 5GB required
    local available_space_kb
    
    if [[ "$DETECTED_OS" == "linux" ]]; then
        available_space_kb=$(df / | awk 'NR==2 {print $4}')
    elif [[ "$DETECTED_OS" == "macos" ]]; then
        available_space_kb=$(df / | awk 'NR==2 {print $4}')
    fi
    
    local available_space_gb=$((available_space_kb / 1024 / 1024))
    
    if [[ $available_space_gb -lt $required_space_gb ]]; then
        log_error "Insufficient disk space: ${available_space_gb}GB available, ${required_space_gb}GB required"
        exit 1
    fi
    
    log_info "Disk space check passed: ${available_space_gb}GB available"
}

# Validate network connectivity
validate_network_connectivity() {
    log_info "Validating network connectivity..."
    
    local test_urls=("https://github.com" "https://pypi.org")
    local failed_urls=()
    
    for url in "${test_urls[@]}"; do
        if ! curl -s --connect-timeout 10 "$url" > /dev/null; then
            failed_urls+=("$url")
        fi
    done
    
    if [[ ${#failed_urls[@]} -gt 0 ]]; then
        log_warn "Network connectivity issues detected:"
        for url in "${failed_urls[@]}"; do
            log_warn "  Cannot reach: $url"
        done
        log_warn "Some packages may fail to install"
    else
        log_info "Network connectivity check passed"
    fi
}

# Validate package manager availability
validate_package_manager() {
    local package_manager="$1"
    
    case "$package_manager" in
        "apt")
            if ! command -v apt &> /dev/null; then
                log_error "APT package manager not available"
                return 1
            fi
            ;;
        "pacman")
            if ! command -v pacman &> /dev/null; then
                log_error "Pacman package manager not available"
                return 1
            fi
            ;;
        "yum"|"dnf")
            if ! command -v yum &> /dev/null && ! command -v dnf &> /dev/null; then
                log_error "YUM/DNF package manager not available"
                return 1
            fi
            ;;
        "zypper")
            if ! command -v zypper &> /dev/null; then
                log_error "Zypper package manager not available"
                return 1
            fi
            ;;
        *)
            log_error "Unknown package manager: $package_manager"
            return 1
            ;;
    esac
    
    return 0
}

# Validate user permissions
validate_user_permissions() {
    log_info "Validating user permissions..."
    
    if [[ $EUID -eq 0 ]]; then
        log_warn "Running as root - this is not recommended"
        log_warn "Consider running as a regular user with sudo privileges"
    elif ! sudo -n true 2>/dev/null; then
        log_error "Sudo privileges required but not available"
        log_error "Please ensure the user has sudo access"
        exit 1
    else
        log_info "User permissions validated"
    fi
}