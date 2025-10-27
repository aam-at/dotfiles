#!/usr/bin/env bash

# Modern Linux Installation System - System Utilities
# Provides system detection and information gathering

# System information variables
DETECTED_OS=""
DETECTED_ARCH=""
DETECTED_DISTRO=""
DETECTED_VERSION=""
DETECTED_KERNEL=""
DETECTED_SHELL=""

# Detect system information
detect_system() {
    log_info "Detecting system information..."
    
    # Detect OS
    case "$OSTYPE" in
        "linux-gnu"*)
            DETECTED_OS="linux"
            ;;
        "darwin"*)
            DETECTED_OS="macos"
            ;;
        "cygwin"*|"msys"*|"win32"*)
            DETECTED_OS="windows"
            ;;
        *)
            DETECTED_OS="unknown"
            ;;
    esac
    
    # Detect architecture
    DETECTED_ARCH=$(uname -m)
    
    # Detect kernel
    DETECTED_KERNEL=$(uname -r)
    
    # Detect shell
    DETECTED_SHELL=$(basename "$SHELL")
    
    # Detect Linux distribution
    if [[ "$DETECTED_OS" == "linux" ]]; then
        detect_linux_distro
    fi
    
    log_info "OS: $DETECTED_OS"
    log_info "Architecture: $DETECTED_ARCH"
    log_info "Kernel: $DETECTED_KERNEL"
    log_info "Shell: $DETECTED_SHELL"
    
    if [[ "$DETECTED_OS" == "linux" ]]; then
        log_info "Distribution: $DETECTED_DISTRO"
        log_info "Version: $DETECTED_VERSION"
    fi
}

# Detect Linux distribution
detect_linux_distro() {
    if [[ -f /etc/os-release ]]; then
        source /etc/os-release
        DETECTED_DISTRO="$ID"
        DETECTED_VERSION="$VERSION_ID"
    elif [[ -f /etc/redhat-release ]]; then
        DETECTED_DISTRO="rhel"
        DETECTED_VERSION=$(cat /etc/redhat-release | grep -oE '[0-9]+' | head -1)
    elif [[ -f /etc/debian_version ]]; then
        DETECTED_DISTRO="debian"
        DETECTED_VERSION=$(cat /etc/debian_version)
    elif [[ -f /etc/arch-release ]]; then
        DETECTED_DISTRO="arch"
        DETECTED_VERSION="rolling"
    else
        DETECTED_DISTRO="unknown"
        DETECTED_VERSION="unknown"
    fi
}

# Check if running in WSL
is_wsl() {
    if [[ -f /proc/version ]] && grep -qi microsoft /proc/version; then
        return 0
    else
        return 1
    fi
}

# Check if running in Docker
is_docker() {
    if [[ -f /.dockerenv ]] || grep -q docker /proc/1/cgroup 2>/dev/null; then
        return 0
    else
        return 1
    fi
}

# Check if running in VM
is_vm() {
    if [[ -d /proc/vz ]] || grep -q "VBOX" /proc/cpuinfo 2>/dev/null || \
       grep -q "QEMU" /proc/cpuinfo 2>/dev/null || \
       grep -q "Xen" /proc/cpuinfo 2>/dev/null; then
        return 0
    else
        return 1
    fi
}

# Get system memory
get_system_memory() {
    if [[ "$DETECTED_OS" == "linux" ]]; then
        if [[ -f /proc/meminfo ]]; then
            grep MemTotal /proc/meminfo | awk '{print $2}' | awk '{print int($1/1024/1024)}'
        else
            echo "0"
        fi
    elif [[ "$DETECTED_OS" == "macos" ]]; then
        sysctl -n hw.memsize | awk '{print int($1/1024/1024/1024)}'
    else
        echo "0"
    fi
}

# Get system CPU cores
get_cpu_cores() {
    if [[ "$DETECTED_OS" == "linux" ]]; then
        nproc 2>/dev/null || echo "1"
    elif [[ "$DETECTED_OS" == "macos" ]]; then
        sysctl -n hw.ncpu 2>/dev/null || echo "1"
    else
        echo "1"
    fi
}

# Get disk space
get_disk_space() {
    local path="${1:-/}"
    
    if [[ "$DETECTED_OS" == "linux" ]]; then
        df "$path" | awk 'NR==2 {print int($4/1024/1024)}'
    elif [[ "$DETECTED_OS" == "macos" ]]; then
        df "$path" | awk 'NR==2 {print int($4/1024/1024)}'
    else
        echo "0"
    fi
}

# Check if command exists
command_exists() {
    local cmd="$1"
    command -v "$cmd" &> /dev/null
}

# Check if file exists and is executable
is_executable() {
    local file="$1"
    [[ -f "$file" && -x "$file" ]]
}

# Check if directory exists and is writable
is_writable() {
    local dir="$1"
    [[ -d "$dir" && -w "$dir" ]]
}

# Get user home directory
get_home_dir() {
    echo "${HOME:-/home/$USER}"
}

# Get user name
get_username() {
    echo "${USER:-$(whoami)}"
}

# Check if running as root
is_root() {
    [[ $EUID -eq 0 ]]
}

# Check if user has sudo privileges
has_sudo() {
    sudo -n true 2>/dev/null
}

# Get system uptime
get_uptime() {
    if [[ "$DETECTED_OS" == "linux" ]]; then
        uptime -p 2>/dev/null || uptime
    elif [[ "$DETECTED_OS" == "macos" ]]; then
        uptime
    else
        echo "Unknown"
    fi
}

# Get system load average
get_load_average() {
    if [[ "$DETECTED_OS" == "linux" ]]; then
        cat /proc/loadavg 2>/dev/null || echo "0 0 0"
    elif [[ "$DETECTED_OS" == "macos" ]]; then
        uptime | awk -F'load averages:' '{print $2}' | awk '{print $1, $2, $3}'
    else
        echo "0 0 0"
    fi
}

# Check if system is 64-bit
is_64bit() {
    [[ "$DETECTED_ARCH" == "x86_64" || "$DETECTED_ARCH" == "amd64" || "$DETECTED_ARCH" == "aarch64" || "$DETECTED_ARCH" == "arm64" ]]
}

# Check if system supports virtualization
supports_virtualization() {
    if [[ "$DETECTED_OS" == "linux" ]]; then
        if grep -q vmx /proc/cpuinfo 2>/dev/null || grep -q svm /proc/cpuinfo 2>/dev/null; then
            return 0
        fi
    fi
    return 1
}

# Get system information summary
get_system_summary() {
    cat << EOF
System Information Summary:
  OS: $DETECTED_OS
  Architecture: $DETECTED_ARCH
  Kernel: $DETECTED_KERNEL
  Shell: $DETECTED_SHELL
  Memory: $(get_system_memory)GB
  CPU Cores: $(get_cpu_cores)
  Disk Space: $(get_disk_space)GB
  Uptime: $(get_uptime)
  Load Average: $(get_load_average)
  WSL: $(is_wsl && echo "Yes" || echo "No")
  Docker: $(is_docker && echo "Yes" || echo "No")
  VM: $(is_vm && echo "Yes" || echo "No")
  Root: $(is_root && echo "Yes" || echo "No")
  Sudo: $(has_sudo && echo "Yes" || echo "No")
EOF
}