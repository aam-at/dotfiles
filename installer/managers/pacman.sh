#!/usr/bin/env bash

# Modern Linux Installation System - Pacman Package Manager
# Handles Arch Linux package management

# Pacman package manager functions
pacman_update() {
    log_info "Updating Pacman package database"
    log_command "sudo pacman -Sy"
}

pacman_install() {
    local packages=("$@")
    log_info "Installing packages with Pacman: ${packages[*]}"
    
    # Update package database first
    pacman_update
    
    # Install packages
    local install_cmd="sudo pacman -S --needed --noconfirm ${packages[*]}"
    log_command "$install_cmd"
}

pacman_remove() {
    local packages=("$@")
    log_info "Removing packages with Pacman: ${packages[*]}"
    
    local remove_cmd="sudo pacman -R --noconfirm ${packages[*]}"
    log_command "$remove_cmd"
}

pacman_search() {
    local query="$1"
    log_debug "Searching packages with Pacman: $query"
    
    local search_cmd="pacman -Ss $query"
    log_command "$search_cmd"
}

pacman_list() {
    log_debug "Listing installed packages with Pacman"
    
    local list_cmd="pacman -Q"
    log_command "$list_cmd"
}

pacman_is_installed() {
    local package="$1"
    
    if pacman -Q "$package" &>/dev/null; then
        return 0
    else
        return 1
    fi
}

pacman_info() {
    local package="$1"
    log_debug "Getting package information: $package"
    
    local info_cmd="pacman -Si $package"
    log_command "$info_cmd"
}

# Install packages from AUR using yay
yay_install() {
    local packages=("$@")
    log_info "Installing AUR packages with yay: ${packages[*]}"
    
    # Check if yay is installed
    if ! command -v yay &> /dev/null; then
        log_error "yay is not installed. Please install yay first."
        return 1
    fi
    
    # Install AUR packages
    local install_cmd="yay -S --needed --noconfirm ${packages[*]}"
    log_command "$install_cmd"
}

# Install packages from AUR using paru
paru_install() {
    local packages=("$@")
    log_info "Installing AUR packages with paru: ${packages[*]}"
    
    # Check if paru is installed
    if ! command -v paru &> /dev/null; then
        log_error "paru is not installed. Please install paru first."
        return 1
    fi
    
    # Install AUR packages
    local install_cmd="paru -S --needed --noconfirm ${packages[*]}"
    log_command "$install_cmd"
}

# Install packages from AUR (auto-detect helper)
aur_install() {
    local packages=("$@")
    
    if command -v yay &> /dev/null; then
        yay_install "${packages[@]}"
    elif command -v paru &> /dev/null; then
        paru_install "${packages[@]}"
    else
        log_error "No AUR helper found. Please install yay or paru first."
        return 1
    fi
}

# Clean package cache
pacman_clean() {
    log_info "Cleaning Pacman cache"
    
    local clean_cmd="sudo pacman -Sc --noconfirm"
    log_command "$clean_cmd"
}

# Clean unused packages
pacman_clean_unused() {
    log_info "Cleaning unused packages with Pacman"
    
    local clean_unused_cmd="sudo pacman -Rns $(pacman -Qtdq) --noconfirm"
    log_command "$clean_unused_cmd"
}

# Upgrade all packages
pacman_upgrade() {
    log_info "Upgrading all packages with Pacman"
    
    # Update package database
    pacman_update
    
    # Upgrade packages
    local upgrade_cmd="sudo pacman -Su --noconfirm"
    log_command "$upgrade_cmd"
}

# Install package group
pacman_install_group() {
    local group="$1"
    log_info "Installing package group: $group"
    
    local install_group_cmd="sudo pacman -S --needed --noconfirm $group"
    log_command "$install_group_cmd"
}

# List package groups
pacman_list_groups() {
    log_debug "Listing available package groups"
    
    local list_groups_cmd="pacman -Sg"
    log_command "$list_groups_cmd"
}