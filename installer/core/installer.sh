#!/usr/bin/env bash

# Modern Linux Installation System - Main Installer
# A modular, extensible, and robust system for managing Linux installations

set -euo pipefail

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INSTALLER_DIR="$(dirname "$SCRIPT_DIR")"
DOTFILES_DIR="$(dirname "$INSTALLER_DIR")"

# Source core modules
source "$SCRIPT_DIR/config.sh"
source "$SCRIPT_DIR/logger.sh"
source "$SCRIPT_DIR/validator.sh"
source "$SCRIPT_DIR/rollback.sh"

# Source utilities
source "$INSTALLER_DIR/utils/system.sh"
source "$INSTALLER_DIR/utils/network.sh"
source "$INSTALLER_DIR/utils/backup.sh"

# Default configuration
DEFAULT_CONFIG_FILE="$INSTALLER_DIR/configs/modules.yaml"
CONFIG_FILE="${CONFIG_FILE:-$DEFAULT_CONFIG_FILE}"
LOG_LEVEL="${LOG_LEVEL:-INFO}"
DRY_RUN="${DRY_RUN:-false}"
ROLLBACK_MODE="${ROLLBACK_MODE:-false}"
PROFILE="${PROFILE:-default}"

# Installation state
INSTALLATION_ID=""
INSTALLATION_START_TIME=""
INSTALLED_MODULES=()
FAILED_MODULES=()

# Parse command line arguments
parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --config)
                CONFIG_FILE="$2"
                shift 2
                ;;
            --modules)
                IFS=',' read -ra MODULES <<< "$2"
                shift 2
                ;;
            --profile)
                PROFILE="$2"
                shift 2
                ;;
            --dry-run)
                DRY_RUN=true
                shift
                ;;
            --rollback)
                ROLLBACK_MODE=true
                shift
                ;;
            --log-level)
                LOG_LEVEL="$2"
                shift 2
                ;;
            --help)
                show_help
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
    done
}

# Show help information
show_help() {
    cat << EOF
Modern Linux Installation System

USAGE:
    $0 [OPTIONS]

OPTIONS:
    --config FILE        Configuration file (default: configs/modules.yaml)
    --modules LIST       Comma-separated list of modules to install
    --profile NAME       Installation profile (default: default)
    --dry-run           Show what would be installed without making changes
    --rollback          Rollback the last installation
    --log-level LEVEL   Set log level (DEBUG, INFO, WARN, ERROR)
    --help              Show this help message

EXAMPLES:
    $0                                    # Install everything
    $0 --modules development,ai           # Install specific modules
    $0 --profile developer                # Use developer profile
    $0 --dry-run                          # Test installation
    $0 --rollback                         # Rollback last installation

PROFILES:
    default      - Basic system setup
    developer    - Development tools and environments
    minimal      - Minimal installation
    workstation  - Full workstation setup
    server       - Server-focused installation
EOF
}

# Initialize installation
init_installation() {
    log_info "Initializing Modern Linux Installation System"
    
    # Generate installation ID
    INSTALLATION_ID="install_$(date +%Y%m%d_%H%M%S)_$$"
    INSTALLATION_START_TIME=$(date +%s)
    
    # Create installation log directory
    mkdir -p "$DOTFILES_DIR/.installer/logs"
    
    # Initialize logger
    init_logger "$DOTFILES_DIR/.installer/logs/${INSTALLATION_ID}.log"
    
    log_info "Installation ID: $INSTALLATION_ID"
    log_info "Configuration file: $CONFIG_FILE"
    log_info "Profile: $PROFILE"
    log_info "Dry run: $DRY_RUN"
    
    # Validate system
    validate_system_requirements
    
    # Load configuration
    load_configuration "$CONFIG_FILE"
    
    # Create backup if not in dry-run mode
    if [[ "$DRY_RUN" == "false" ]]; then
        create_system_backup
    fi
}

# Load configuration
load_configuration() {
    local config_file="$1"
    
    if [[ ! -f "$config_file" ]]; then
        log_error "Configuration file not found: $config_file"
        exit 1
    fi
    
    log_info "Loading configuration from: $config_file"
    
    # Load YAML configuration (simplified for now)
    # In a full implementation, you'd use a proper YAML parser
    source "$config_file"
}

# Main installation function
main() {
    parse_arguments "$@"
    
    if [[ "$ROLLBACK_MODE" == "true" ]]; then
        rollback_installation
        exit 0
    fi
    
    init_installation
    
    if [[ "$DRY_RUN" == "true" ]]; then
        log_info "DRY RUN MODE - No changes will be made"
        simulate_installation
    else
        execute_installation
    fi
    
    log_info "Installation completed successfully"
}

# Simulate installation (dry run)
simulate_installation() {
    log_info "Simulating installation..."
    
    # Load modules based on profile or specified modules
    local modules_to_install=()
    
    if [[ ${#MODULES[@]} -gt 0 ]]; then
        modules_to_install=("${MODULES[@]}")
    else
        modules_to_install=($(get_profile_modules "$PROFILE"))
    fi
    
    log_info "Modules to install: ${modules_to_install[*]}"
    
    # Simulate each module installation
    for module in "${modules_to_install[@]}"; do
        log_info "Would install module: $module"
        simulate_module_installation "$module"
    done
}

# Execute actual installation
execute_installation() {
    log_info "Starting installation..."
    
    # Load modules based on profile or specified modules
    local modules_to_install=()
    
    if [[ ${#MODULES[@]} -gt 0 ]]; then
        modules_to_install=("${MODULES[@]}")
    else
        modules_to_install=($(get_profile_modules "$PROFILE"))
    fi
    
    log_info "Installing modules: ${modules_to_install[*]}"
    
    # Install each module
    for module in "${modules_to_install[@]}"; do
        if install_module "$module"; then
            INSTALLED_MODULES+=("$module")
            log_info "Successfully installed module: $module"
        else
            FAILED_MODULES+=("$module")
            log_error "Failed to install module: $module"
        fi
    done
    
    # Report results
    report_installation_results
}

# Install a specific module
install_module() {
    local module="$1"
    local module_dir="$INSTALLER_DIR/modules/$module"
    
    if [[ ! -d "$module_dir" ]]; then
        log_error "Module not found: $module"
        return 1
    fi
    
    log_info "Installing module: $module"
    
    # Check dependencies
    if ! check_module_dependencies "$module"; then
        log_error "Dependencies not met for module: $module"
        return 1
    fi
    
    # Execute module installation
    if [[ -f "$module_dir/install.sh" ]]; then
        source "$module_dir/install.sh"
        if install_module_packages; then
            log_info "Module $module installed successfully"
            return 0
        else
            log_error "Module $module installation failed"
            return 1
        fi
    else
        log_error "No install script found for module: $module"
        return 1
    fi
}

# Simulate module installation
simulate_module_installation() {
    local module="$1"
    local module_dir="$INSTALLER_DIR/modules/$module"
    
    if [[ ! -d "$module_dir" ]]; then
        log_warn "Module not found: $module"
        return 1
    fi
    
    log_info "Would install module: $module"
    
    # Check dependencies
    if ! check_module_dependencies "$module"; then
        log_warn "Dependencies not met for module: $module"
        return 1
    fi
    
    # Show what would be installed
    if [[ -f "$module_dir/packages.yaml" ]]; then
        log_info "Would install packages from: $module_dir/packages.yaml"
    fi
}

# Check module dependencies
check_module_dependencies() {
    local module="$1"
    local deps_file="$INSTALLER_DIR/modules/$module/dependencies.yaml"
    
    if [[ ! -f "$deps_file" ]]; then
        return 0  # No dependencies
    fi
    
    # In a full implementation, you'd parse the dependencies file
    # and check if each dependency is satisfied
    log_debug "Checking dependencies for module: $module"
    return 0
}

# Get modules for a profile
get_profile_modules() {
    local profile="$1"
    local profile_file="$INSTALLER_DIR/configs/profiles/${profile}.yaml"
    
    if [[ ! -f "$profile_file" ]]; then
        log_error "Profile not found: $profile"
        return 1
    fi
    
    # In a full implementation, you'd parse the profile file
    # For now, return some default modules
    case "$profile" in
        "developer")
            echo "core development editors ai"
            ;;
        "minimal")
            echo "core"
            ;;
        "workstation")
            echo "core development editors ai gui fonts"
            ;;
        "server")
            echo "core development"
            ;;
        *)
            echo "core development editors ai gui fonts"
            ;;
    esac
}

# Report installation results
report_installation_results() {
    local end_time=$(date +%s)
    local duration=$((end_time - INSTALLATION_START_TIME))
    
    log_info "Installation completed in ${duration} seconds"
    log_info "Successfully installed: ${#INSTALLED_MODULES[@]} modules"
    log_info "Failed installations: ${#FAILED_MODULES[@]} modules"
    
    if [[ ${#INSTALLED_MODULES[@]} -gt 0 ]]; then
        log_info "Installed modules: ${INSTALLED_MODULES[*]}"
    fi
    
    if [[ ${#FAILED_MODULES[@]} -gt 0 ]]; then
        log_warn "Failed modules: ${FAILED_MODULES[*]}"
        log_warn "Check logs for details: $DOTFILES_DIR/.installer/logs/${INSTALLATION_ID}.log"
    fi
}

# Run main function
main "$@"