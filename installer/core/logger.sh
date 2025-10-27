#!/usr/bin/env bash

# Modern Linux Installation System - Logging Module
# Provides comprehensive logging functionality

# Log levels
readonly LOG_LEVEL_DEBUG=0
readonly LOG_LEVEL_INFO=1
readonly LOG_LEVEL_WARN=2
readonly LOG_LEVEL_ERROR=3

# Current log level
CURRENT_LOG_LEVEL=$LOG_LEVEL_INFO
LOG_FILE=""

# Initialize logger
init_logger() {
    local log_file="$1"
    LOG_FILE="$log_file"
    
    # Create log directory if it doesn't exist
    mkdir -p "$(dirname "$log_file")"
    
    # Set log level based on environment variable
    case "${LOG_LEVEL:-INFO}" in
        "DEBUG") CURRENT_LOG_LEVEL=$LOG_LEVEL_DEBUG ;;
        "INFO")  CURRENT_LOG_LEVEL=$LOG_LEVEL_INFO ;;
        "WARN")  CURRENT_LOG_LEVEL=$LOG_LEVEL_WARN ;;
        "ERROR") CURRENT_LOG_LEVEL=$LOG_LEVEL_ERROR ;;
        *)       CURRENT_LOG_LEVEL=$LOG_LEVEL_INFO ;;
    esac
    
    log_info "Logger initialized - Level: $LOG_LEVEL, File: $log_file"
}

# Log a message
log_message() {
    local level="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    local level_name="$3"
    
    if [[ $level -ge $CURRENT_LOG_LEVEL ]]; then
        local log_entry="[$timestamp] [$level_name] $message"
        
        # Print to console with color
        case $level in
            $LOG_LEVEL_DEBUG) echo -e "\033[36m$log_entry\033[0m" ;;
            $LOG_LEVEL_INFO)  echo -e "\033[32m$log_entry\033[0m" ;;
            $LOG_LEVEL_WARN)  echo -e "\033[33m$log_entry\033[0m" ;;
            $LOG_LEVEL_ERROR) echo -e "\033[31m$log_entry\033[0m" ;;
        esac
        
        # Write to log file
        if [[ -n "$LOG_FILE" ]]; then
            echo "$log_entry" >> "$LOG_FILE"
        fi
    fi
}

# Log debug message
log_debug() {
    log_message $LOG_LEVEL_DEBUG "$1" "DEBUG"
}

# Log info message
log_info() {
    log_message $LOG_LEVEL_INFO "$1" "INFO"
}

# Log warning message
log_warn() {
    log_message $LOG_LEVEL_WARN "$1" "WARN"
}

# Log error message
log_error() {
    log_message $LOG_LEVEL_ERROR "$1" "ERROR"
}

# Log command execution
log_command() {
    local command="$1"
    log_debug "Executing: $command"
    
    if eval "$command" 2>&1 | while read -r line; do
        log_debug "  $line"
    done; then
        log_debug "Command completed successfully"
        return 0
    else
        local exit_code=$?
        log_error "Command failed with exit code: $exit_code"
        return $exit_code
    fi
}

# Log section header
log_section() {
    local section="$1"
    local width=60
    local padding=$(( (width - ${#section} - 2) / 2 ))
    
    log_info "$(printf '=%.0s' {1..$width})"
    log_info "$(printf '%*s %s %*s' $padding '' "$section" $padding '')"
    log_info "$(printf '=%.0s' {1..$width})"
}

# Log progress
log_progress() {
    local current="$1"
    local total="$2"
    local message="${3:-Progress}"
    local percentage=$(( (current * 100) / total ))
    
    log_info "$message: $current/$total ($percentage%)"
}