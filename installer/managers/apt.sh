#!/usr/bin/env bash

# Modern Linux Installation System - APT Package Manager
# Handles Debian/Ubuntu package management

# APT package manager functions
apt_update() {
    log_info "Updating APT package database"
    log_command "sudo apt update"
}

apt_install() {
    local packages=("$@")
    log_info "Installing packages with APT: ${packages[*]}"
    
    # Update package database first
    apt_update
    
    # Install packages
    local install_cmd="sudo apt install -y ${packages[*]}"
    log_command "$install_cmd"
}

apt_remove() {
    local packages=("$@")
    log_info "Removing packages with APT: ${packages[*]}"
    
    local remove_cmd="sudo apt remove -y ${packages[*]}"
    log_command "$remove_cmd"
}

apt_search() {
    local query="$1"
    log_debug "Searching packages with APT: $query"
    
    local search_cmd="apt search $query"
    log_command "$search_cmd"
}

apt_list() {
    log_debug "Listing installed packages with APT"
    
    local list_cmd="apt list --installed"
    log_command "$list_cmd"
}

apt_is_installed() {
    local package="$1"
    
    if dpkg -l | grep -q "^ii[[:space:]]*$package[[:space:]]"; then
        return 0
    else
        return 1
    fi
}

apt_info() {
    local package="$1"
    log_debug "Getting package information: $package"
    
    local info_cmd="apt show $package"
    log_command "$info_cmd"
}

# Add APT repository
apt_add_repository() {
    local repo="$1"
    log_info "Adding APT repository: $repo"
    
    local add_repo_cmd="sudo add-apt-repository -y $repo"
    log_command "$add_repo_cmd"
    
    # Update package database after adding repository
    apt_update
}

# Install packages from specific repository
apt_install_from_repo() {
    local repo="$1"
    shift
    local packages=("$@")
    
    log_info "Installing packages from repository $repo: ${packages[*]}"
    
    # Add repository
    apt_add_repository "$repo"
    
    # Install packages
    apt_install "${packages[@]}"
}

# Install .deb package
apt_install_deb() {
    local deb_file="$1"
    log_info "Installing .deb package: $deb_file"
    
    local install_deb_cmd="sudo dpkg -i $deb_file"
    log_command "$install_deb_cmd"
    
    # Fix any dependency issues
    local fix_deps_cmd="sudo apt-get install -f"
    log_command "$fix_deps_cmd"
}

# Clean APT cache
apt_clean() {
    log_info "Cleaning APT cache"
    
    local clean_cmd="sudo apt clean"
    log_command "$clean_cmd"
    
    local autoclean_cmd="sudo apt autoclean"
    log_command "$autoclean_cmd"
}

# Upgrade all packages
apt_upgrade() {
    log_info "Upgrading all packages with APT"
    
    # Update package database
    apt_update
    
    # Upgrade packages
    local upgrade_cmd="sudo apt upgrade -y"
    log_command "$upgrade_cmd"
}

# Dist-upgrade (handles dependency changes)
apt_dist_upgrade() {
    log_info "Performing dist-upgrade with APT"
    
    # Update package database
    apt_update
    
    # Dist-upgrade packages
    local dist_upgrade_cmd="sudo apt dist-upgrade -y"
    log_command "$dist_upgrade_cmd"
}