#!/usr/bin/env bash
#
# age-encrypt.sh - Encrypt and decrypt files using age
#
# Usage:
#   ./age-encrypt.sh encrypt <file>    - Encrypt a file
#   ./age-encrypt.sh decrypt <file>    - Decrypt a file
#   ./age-encrypt.sh init              - Generate age keypair
#   ./age-encrypt.sh show-key          - Show public key
#

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Configuration
AGE_KEY_FILE="${AGE_KEY_FILE:-$HOME/.config/age/keys.txt}"
AGE_DIR="$HOME/.config/age"

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Initialize age keypair
init_keypair() {
    log_info "Initializing age keypair..."
    
    if [ ! -d "$AGE_DIR" ]; then
        mkdir -p "$AGE_DIR"
        chmod 700 "$AGE_DIR"
    fi
    
    if [ -f "$AGE_KEY_FILE" ]; then
        log_error "Key file already exists: $AGE_KEY_FILE"
        echo "Backup your existing key before generating a new one!"
        return 1
    fi
    
    # Generate keypair
    age-keygen -o "$AGE_KEY_FILE" 2>&1 | tee "$AGE_DIR/keyinfo.txt"
    chmod 600 "$AGE_KEY_FILE"
    
    log_success "Keypair generated: $AGE_KEY_FILE"
    echo ""
    echo -e "${YELLOW}IMPORTANT:${NC} Backup your private key!"
    echo "  cp $AGE_KEY_FILE <secure-location>"
    echo ""
    echo "Your public key:"
    grep "^# public key:" "$AGE_KEY_FILE" | cut -d' ' -f4
}

# Show public key
show_public_key() {
    if [ ! -f "$AGE_KEY_FILE" ]; then
        log_error "No key file found. Run: $0 init"
        return 1
    fi
    
    echo "Your age public key:"
    grep "^# public key:" "$AGE_KEY_FILE" | cut -d' ' -f4
}

# Encrypt a file
encrypt_file() {
    local input="$1"
    local output="${2:-$input.age}"
    
    if [ ! -f "$input" ]; then
        log_error "File not found: $input"
        return 1
    fi
    
    if [ ! -f "$AGE_KEY_FILE" ]; then
        log_error "No key file found. Run: $0 init"
        return 1
    fi
    
    local public_key
    public_key=$(grep "^# public key:" "$AGE_KEY_FILE" | cut -d' ' -f4)
    
    log_info "Encrypting: $input -> $output"
    age -r "$public_key" -o "$output" "$input"
    
    log_success "File encrypted: $output"
    echo ""
    echo "To decrypt:"
    echo "  $0 decrypt $output"
}

# Decrypt a file
decrypt_file() {
    local input="$1"
    local output="${2:-${input%.age}}"
    
    if [ ! -f "$input" ]; then
        log_error "File not found: $input"
        return 1
    fi
    
    if [ ! -f "$AGE_KEY_FILE" ]; then
        log_error "No key file found. Run: $0 init"
        return 1
    fi
    
    log_info "Decrypting: $input -> $output"
    age -d -o "$output" "$input"
    
    log_success "File decrypted: $output"
}

# Encrypt a directory
encrypt_directory() {
    local dir="$1"
    local archive="${dir}.tar.age"
    local temp_archive="/tmp/$(basename "$dir").tar"
    
    if [ ! -d "$dir" ]; then
        log_error "Directory not found: $dir"
        return 1
    fi
    
    if [ ! -f "$AGE_KEY_FILE" ]; then
        log_error "No key file found. Run: $0 init"
        return 1
    fi
    
    local public_key
    public_key=$(grep "^# public key:" "$AGE_KEY_FILE" | cut -d' ' -f4)
    
    log_info "Creating archive: $temp_archive"
    tar -cf "$temp_archive" -C "$(dirname "$dir")" "$(basename "$dir")"
    
    log_info "Encrypting archive..."
    age -r "$public_key" -o "$archive" "$temp_archive"
    rm "$temp_archive"
    
    log_success "Directory encrypted: $archive"
}

# Decrypt a directory
decrypt_directory() {
    local archive="$1"
    local output_dir="${2:-.}"
    local temp_archive="/tmp/$(basename "$archive" .age)"
    
    if [ ! -f "$archive" ]; then
        log_error "Archive not found: $archive"
        return 1
    fi
    
    if [ ! -f "$AGE_KEY_FILE" ]; then
        log_error "No key file found. Run: $0 init"
        return 1
    fi
    
    log_info "Decrypting archive..."
    age -d -o "$temp_archive" "$archive"
    
    log_info "Extracting archive..."
    tar -xf "$temp_archive" -C "$output_dir"
    rm "$temp_archive"
    
    log_success "Directory decrypted to: $output_dir"
}

# Show help
show_help() {
    cat << EOF
Age Encryption Helper

Usage: $0 <command> [options]

Commands:
  init                    Generate age keypair
  show-key                Show public key
  encrypt <file>          Encrypt a file
  decrypt <file>          Decrypt a file
  encrypt-dir <dir>       Encrypt a directory
  decrypt-dir <archive>   Decrypt a directory archive

Environment Variables:
  AGE_KEY_FILE            Path to age key file (default: ~/.config/age/keys.txt)

Examples:
  $0 init                              # Generate a new keypair
  $0 show-key                          # Show your public key
  $0 encrypt secrets.txt               # Encrypt secrets.txt
  $0 decrypt secrets.txt.age           # Decrypt secrets.txt.age
  $0 encrypt-dir ~/.ssh                # Encrypt .ssh directory
  $0 decrypt-dir ssh.tar.age ~/        # Decrypt to home directory

EOF
}

# Main
main() {
    if [ $# -lt 1 ]; then
        show_help
        exit 1
    fi
    
    local command="$1"
    shift
    
    case "$command" in
        init)
            init_keypair
            ;;
        show-key)
            show_public_key
            ;;
        encrypt)
            if [ $# -lt 1 ]; then
                log_error "Please specify a file to encrypt"
                exit 1
            fi
            encrypt_file "$@"
            ;;
        decrypt)
            if [ $# -lt 1 ]; then
                log_error "Please specify a file to decrypt"
                exit 1
            fi
            decrypt_file "$@"
            ;;
        encrypt-dir)
            if [ $# -lt 1 ]; then
                log_error "Please specify a directory to encrypt"
                exit 1
            fi
            encrypt_directory "$@"
            ;;
        decrypt-dir)
            if [ $# -lt 1 ]; then
                log_error "Please specify an archive to decrypt"
                exit 1
            fi
            decrypt_directory "$@"
            ;;
        help|--help|-h)
            show_help
            ;;
        *)
            log_error "Unknown command: $command"
            show_help
            exit 1
            ;;
    esac
}

main "$@"
