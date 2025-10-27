#!/usr/bin/env bash

# Modern Linux Installation System - Configuration Module
# Provides configuration management and validation

# Configuration variables
declare -A CONFIG_VALUES
declare -A MODULE_CONFIGS
declare -A PROFILE_CONFIGS

# Load configuration from YAML file (simplified parser)
load_configuration() {
    local config_file="$1"
    
    if [[ ! -f "$config_file" ]]; then
        log_error "Configuration file not found: $config_file"
        return 1
    fi
    
    log_info "Loading configuration from: $config_file"
    
    # Simple YAML parser for basic configuration
    while IFS= read -r line; do
        # Skip empty lines and comments
        [[ -z "$line" || "$line" =~ ^[[:space:]]*# ]] && continue
        
        # Parse key-value pairs
        if [[ "$line" =~ ^[[:space:]]*([^:]+):[[:space:]]*(.*)$ ]]; then
            local key="${BASH_REMATCH[1]// /}"
            local value="${BASH_REMATCH[2]// /}"
            
            # Remove quotes if present
            value="${value%\"}"
            value="${value#\"}"
            value="${value%\'}"
            value="${value#\'}"
            
            CONFIG_VALUES["$key"]="$value"
        fi
    done < "$config_file"
    
    log_info "Configuration loaded successfully"
}

# Get configuration value
get_config() {
    local key="$1"
    local default_value="${2:-}"
    
    if [[ -n "${CONFIG_VALUES[$key]:-}" ]]; then
        echo "${CONFIG_VALUES[$key]}"
    else
        echo "$default_value"
    fi
}

# Set configuration value
set_config() {
    local key="$1"
    local value="$2"
    
    CONFIG_VALUES["$key"]="$value"
}

# Load module configuration
load_module_config() {
    local module="$1"
    local config_file="$INSTALLER_DIR/modules/$module/config.yaml"
    
    if [[ ! -f "$config_file" ]]; then
        log_debug "No configuration file found for module: $module"
        return 0
    fi
    
    log_debug "Loading configuration for module: $module"
    
    # Load module-specific configuration
    while IFS= read -r line; do
        [[ -z "$line" || "$line" =~ ^[[:space:]]*# ]] && continue
        
        if [[ "$line" =~ ^[[:space:]]*([^:]+):[[:space:]]*(.*)$ ]]; then
            local key="${BASH_REMATCH[1]// /}"
            local value="${BASH_REMATCH[2]// /}"
            
            value="${value%\"}"
            value="${value#\"}"
            value="${value%\'}"
            value="${value#\'}"
            
            MODULE_CONFIGS["${module}.${key}"]="$value"
        fi
    done < "$config_file"
}

# Get module configuration
get_module_config() {
    local module="$1"
    local key="$2"
    local default_value="${3:-}"
    
    local full_key="${module}.${key}"
    
    if [[ -n "${MODULE_CONFIGS[$full_key]:-}" ]]; then
        echo "${MODULE_CONFIGS[$full_key]}"
    else
        echo "$default_value"
    fi
}

# Load profile configuration
load_profile_config() {
    local profile="$1"
    local config_file="$INSTALLER_DIR/configs/profiles/${profile}.yaml"
    
    if [[ ! -f "$config_file" ]]; then
        log_error "Profile configuration not found: $profile"
        return 1
    fi
    
    log_info "Loading profile configuration: $profile"
    
    while IFS= read -r line; do
        [[ -z "$line" || "$line" =~ ^[[:space:]]*# ]] && continue
        
        if [[ "$line" =~ ^[[:space:]]*([^:]+):[[:space:]]*(.*)$ ]]; then
            local key="${BASH_REMATCH[1]// /}"
            local value="${BASH_REMATCH[2]// /}"
            
            value="${value%\"}"
            value="${value#\"}"
            value="${value%\'}"
            value="${value#\'}"
            
            PROFILE_CONFIGS["${profile}.${key}"]="$value"
        fi
    done < "$config_file"
}

# Get profile configuration
get_profile_config() {
    local profile="$1"
    local key="$2"
    local default_value="${3:-}"
    
    local full_key="${profile}.${key}"
    
    if [[ -n "${PROFILE_CONFIGS[$full_key]:-}" ]]; then
        echo "${PROFILE_CONFIGS[$full_key]}"
    else
        echo "$default_value"
    fi
}

# Validate configuration
validate_configuration() {
    log_info "Validating configuration..."
    
    local required_keys=("installer.version" "installer.name")
    local missing_keys=()
    
    for key in "${required_keys[@]}"; do
        if [[ -z "${CONFIG_VALUES[$key]:-}" ]]; then
            missing_keys+=("$key")
        fi
    done
    
    if [[ ${#missing_keys[@]} -gt 0 ]]; then
        log_error "Missing required configuration keys: ${missing_keys[*]}"
        return 1
    fi
    
    log_info "Configuration validation passed"
    return 0
}

# Export configuration to environment
export_configuration() {
    log_debug "Exporting configuration to environment"
    
    for key in "${!CONFIG_VALUES[@]}"; do
        local env_key=$(echo "$key" | tr '.' '_' | tr '[:lower:]' '[:upper:]')
        export "CONFIG_${env_key}"="${CONFIG_VALUES[$key]}"
    done
}

# Create default configuration
create_default_config() {
    local config_file="$1"
    
    cat > "$config_file" << EOF
# Modern Linux Installation System Configuration

installer:
  name: "Modern Linux Installer"
  version: "1.0.0"
  description: "A modular Linux installation system"

logging:
  level: "INFO"
  file: ".installer/logs/install.log"

modules:
  core:
    enabled: true
    priority: 1
  development:
    enabled: true
    priority: 2
  ai:
    enabled: false
    priority: 3
  gui:
    enabled: false
    priority: 4
  fonts:
    enabled: true
    priority: 5

profiles:
  default:
    modules: ["core", "development", "fonts"]
  developer:
    modules: ["core", "development", "ai", "editors"]
  minimal:
    modules: ["core"]
  workstation:
    modules: ["core", "development", "ai", "gui", "fonts", "editors"]
  server:
    modules: ["core", "development"]
EOF
}