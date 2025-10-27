# Modern Linux Installation System

A modular, extensible, and robust system for managing Linux dotfiles and package installations.

## Architecture Overview

```
installer/
├── core/                    # Core installation framework
│   ├── installer.sh         # Main installer entry point
│   ├── config.sh           # Configuration management
│   ├── logger.sh           # Logging system
│   ├── validator.sh         # System validation
│   └── rollback.sh         # Rollback mechanism
├── managers/               # Package manager abstractions
│   ├── base.sh            # Base package manager interface
│   ├── apt.sh             # APT/Debian manager
│   ├── pacman.sh          # Pacman/Arch manager
│   ├── pip.sh             # Python package manager
│   ├── cargo.sh           # Rust package manager
│   └── npm.sh              # Node.js package manager
├── modules/                # Installation modules
│   ├── core/              # Core system packages
│   ├── development/       # Development tools
│   ├── ai/                # AI tools and models
│   ├── gui/               # GUI applications
│   ├── fonts/             # Font packages
│   └── editors/           # Text editors and IDEs
├── configs/               # Module configurations
│   ├── modules.yaml       # Module definitions
│   ├── dependencies.yaml  # Dependency mapping
│   └── profiles/          # Installation profiles
└── utils/                 # Utility functions
    ├── system.sh          # System detection
    ├── network.sh         # Network utilities
    └── backup.sh          # Backup utilities
```

## Key Features

- **Modular Design**: Install only what you need
- **Dependency Resolution**: Automatic dependency management
- **Rollback Support**: Safe installation with rollback capability
- **Progress Tracking**: Real-time installation progress
- **Configuration Validation**: Pre-installation system validation
- **Multi-Distribution**: Support for multiple Linux distributions
- **Profile System**: Predefined installation profiles
- **Dry Run Mode**: Test installations without making changes
- **Logging**: Comprehensive installation logging
- **Resume Support**: Resume failed installations

## Usage

```bash
# Install everything
./installer/core/installer.sh

# Install specific modules
./installer/core/installer.sh --modules development,ai

# Use a profile
./installer/core/installer.sh --profile developer

# Dry run
./installer/core/installer.sh --dry-run

# Rollback last installation
./installer/core/installer.sh --rollback
```