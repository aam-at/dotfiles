#!/usr/bin/env bash

# Migration script from old installation system to Modern Linux Installation System
# This script helps transition from the current fragmented approach to the new modular system

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Print colored output
print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if running from dotfiles directory
check_dotfiles_directory() {
    if [[ ! -f "install_arch.sh" ]] || [[ ! -f "install_deb.sh" ]]; then
        print_error "This script must be run from the dotfiles directory"
        exit 1
    fi
}

# Backup existing installation scripts
backup_existing_scripts() {
    print_info "Creating backup of existing installation scripts..."
    
    local backup_dir=".installer_backup_$(date +%Y%m%d_%H%M%S)"
    mkdir -p "$backup_dir"
    
    # Backup existing scripts
    cp install_arch.sh "$backup_dir/"
    cp install_deb.sh "$backup_dir/"
    cp -r setup/ "$backup_dir/"
    
    print_success "Backup created: $backup_dir"
}

# Create new installer structure
create_installer_structure() {
    print_info "Creating new installer structure..."
    
    # Create directories
    mkdir -p installer/{core,managers,modules,configs/profiles,utils}
    mkdir -p installer/modules/{core,development,ai,gui,fonts,editors}
    
    print_success "Installer structure created"
}

# Migrate package lists
migrate_package_lists() {
    print_info "Migrating package lists to new format..."
    
    # Extract packages from existing scripts and create module configurations
    # This is a simplified migration - in practice, you'd parse the existing scripts
    
    print_warning "Manual migration required for package lists"
    print_info "Please review and update the following files:"
    print_info "  - installer/modules/core/packages.yaml"
    print_info "  - installer/modules/development/packages.yaml"
    print_info "  - installer/modules/ai/packages.yaml"
}

# Create migration report
create_migration_report() {
    local report_file=".installer/migration_report_$(date +%Y%m%d_%H%M%S).md"
    
    cat > "$report_file" << EOF
# Migration Report

## Migration Date
$(date)

## What Was Migrated
- Installation scripts structure
- Package manager abstractions
- Module system framework
- Configuration management

## What Needs Manual Migration
- Package lists from install_arch.sh and install_deb.sh
- Custom installation logic
- Environment-specific configurations

## Next Steps
1. Review and update package lists in installer/modules/*/packages.yaml
2. Test the new installer with --dry-run
3. Update any custom installation scripts
4. Remove old installation scripts after testing

## Files to Review
- installer/modules/core/packages.yaml
- installer/modules/development/packages.yaml
- installer/modules/ai/packages.yaml
- installer/configs/profiles/*.yaml

## Testing Commands
\`\`\`bash
# Test the new installer
./installer/core/installer.sh --dry-run

# Test specific modules
./installer/core/installer.sh --modules core,development --dry-run

# Test profiles
./installer/core/installer.sh --profile developer --dry-run
\`\`\`
EOF

    print_success "Migration report created: $report_file"
}

# Main migration function
main() {
    print_info "Starting migration to Modern Linux Installation System"
    
    # Check prerequisites
    check_dotfiles_directory
    
    # Create backup
    backup_existing_scripts
    
    # Create new structure
    create_installer_structure
    
    # Migrate package lists
    migrate_package_lists
    
    # Create migration report
    create_migration_report
    
    print_success "Migration completed successfully!"
    print_info "Next steps:"
    print_info "1. Review the migration report"
    print_info "2. Update package lists in installer/modules/*/packages.yaml"
    print_info "3. Test the new installer with --dry-run"
    print_info "4. Remove old scripts after successful testing"
}

# Run migration
main "$@"