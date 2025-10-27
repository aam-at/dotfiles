# Modern Linux Installation System - Examples

This document provides comprehensive examples of how to use the Modern Linux Installation System.

## Basic Usage

### Install Everything (Default Profile)
```bash
./installer/core/installer.sh
```

### Install Specific Modules
```bash
./installer/core/installer.sh --modules core,development,ai
```

### Use a Profile
```bash
./installer/core/installer.sh --profile developer
```

### Dry Run (Test Installation)
```bash
./installer/core/installer.sh --dry-run
```

### Rollback Last Installation
```bash
./installer/core/installer.sh --rollback
```

## Advanced Usage

### Custom Configuration
```bash
./installer/core/installer.sh --config /path/to/custom/config.yaml
```

### Verbose Logging
```bash
LOG_LEVEL=DEBUG ./installer/core/installer.sh
```

### Install with Specific Log File
```bash
LOG_FILE=/tmp/install.log ./installer/core/installer.sh
```

## Creating Custom Modules

### 1. Create Module Directory
```bash
mkdir -p installer/modules/my_module
```

### 2. Create Package Configuration
```yaml
# installer/modules/my_module/packages.yaml
packages:
  system:
    apt:
      - my-package
      - another-package
    pacman:
      - my-package
      - another-package
  
  python:
    pip:
      - my-python-package

dependencies:
  - curl
  - git

post_install:
  - "echo 'My module installed successfully'"
```

### 3. Create Installation Script
```bash
#!/usr/bin/env bash
# installer/modules/my_module/install.sh

MODULE_NAME="my_module"
MODULE_DESCRIPTION="My custom module"

install_module_packages() {
    log_section "Installing My Module"
    
    # Install system packages
    if command -v apt &> /dev/null; then
        apt_install my-package another-package
    elif command -v pacman &> /dev/null; then
        pacman_install my-package another-package
    fi
    
    # Install Python packages
    pip install my-python-package
    
    log_info "My module installed successfully"
}
```

### 4. Create Dependencies File
```yaml
# installer/modules/my_module/dependencies.yaml
system:
  - curl
  - git
  - python3

packages:
  - my-package
  - another-package

python:
  - my-python-package
```

## Creating Custom Profiles

### 1. Create Profile Configuration
```yaml
# installer/configs/profiles/my_profile.yaml
profile:
  name: "my_profile"
  description: "My custom installation profile"
  version: "1.0.0"

modules:
  - core
  - my_module
  - development

settings:
  install_optional: true
  install_recommended: false
  
  # Custom settings
  install_my_tools: true
  configure_my_service: true

environment:
  MY_ENV_VAR: "my_value"
  MY_OTHER_VAR: "other_value"

post_install:
  - "echo 'My profile installed successfully'"
  - "systemctl enable my-service || true"
```

## Package Manager Examples

### Using APT
```bash
# Install packages
apt_install package1 package2 package3

# Add repository and install
apt_add_repository "ppa:example/ppa"
apt_install package1 package2

# Install .deb file
apt_install_deb /path/to/package.deb

# Update and upgrade
apt_update
apt_upgrade
```

### Using Pacman
```bash
# Install packages
pacman_install package1 package2 package3

# Install AUR packages
aur_install package1 package2

# Update and upgrade
pacman_update
pacman_upgrade
```

### Using Pip
```bash
# Install Python packages
pip_install package1 package2

# Install with specific version
pip_install "package1==1.0.0"

# Install from requirements file
pip_install -r requirements.txt
```

### Using Cargo
```bash
# Install Rust packages
cargo_install package1 package2

# Install with specific version
cargo_install --version 1.0.0 package1
```

## Logging Examples

### Basic Logging
```bash
# Info level (default)
log_info "This is an info message"

# Debug level
log_debug "This is a debug message"

# Warning level
log_warn "This is a warning message"

# Error level
log_error "This is an error message"
```

### Section Logging
```bash
# Create a section header
log_section "Installing My Module"

# Log progress
log_progress 5 10 "Installing packages"
```

### Command Logging
```bash
# Log command execution
log_command "sudo apt install package1"

# Log with custom message
log_info "Installing package1"
log_command "sudo apt install package1"
```

## Error Handling Examples

### Basic Error Handling
```bash
if ! install_package "package1"; then
    log_error "Failed to install package1"
    return 1
fi
```

### Error Recovery
```bash
if ! install_package "package1"; then
    log_warn "Failed to install package1, trying alternative"
    if ! install_alternative "package1"; then
        log_error "All installation methods failed for package1"
        return 1
    fi
fi
```

### Validation
```bash
# Validate before installation
if ! validate_system_requirements; then
    log_error "System requirements not met"
    exit 1
fi

# Validate after installation
if ! validate_installation; then
    log_error "Installation validation failed"
    rollback_installation
    exit 1
fi
```

## Integration Examples

### With Dotbot
```bash
# Install packages first
./installer/core/installer.sh --profile developer

# Then link configuration files
./install
```

### With Ansible
```yaml
# playbook.yml
- name: Install packages using Modern Linux Installer
  shell: ./installer/core/installer.sh --profile developer
  become: yes

- name: Link configuration files
  shell: ./install
  become: no
```

### With Docker
```dockerfile
# Dockerfile
FROM ubuntu:22.04

# Copy installer
COPY installer/ /installer/

# Install packages
RUN /installer/core/installer.sh --profile minimal

# Copy and link configs
COPY . /dotfiles/
RUN cd /dotfiles && ./install
```

## Troubleshooting

### Check Logs
```bash
# View installation logs
tail -f .installer/logs/install_*.log

# View specific log level
grep "ERROR" .installer/logs/install_*.log
```

### Debug Mode
```bash
# Enable debug logging
LOG_LEVEL=DEBUG ./installer/core/installer.sh

# Verbose output
set -x
./installer/core/installer.sh
```

### Validate Installation
```bash
# Check installed packages
./installer/core/installer.sh --dry-run

# Validate configuration
./installer/utils/validate.sh
```

### Rollback
```bash
# Rollback last installation
./installer/core/installer.sh --rollback

# List available rollbacks
ls .installer/backups/
```