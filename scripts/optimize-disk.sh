#!/usr/bin/env bash
# Clear package-manager and development-tool caches, then report recovered space.
# Usage: optimize-disk.sh [--dry-run] [--system] [--full-bun-cache] [--containers]
set -uo pipefail

dry_run=false
system=false
containers=false
full_bun_cache=false
clean_tmp=true

usage() {
  cat <<'EOF'
Usage: optimize-disk.sh [OPTIONS]

Clear caches for installed development tools and remove age-expired temporary files
under /tmp and /var/tmp. The script reports free space before and after cleanup.
Missing tools are skipped. Temporary-file cleanup follows systemd's configured ages
and may prompt for sudo.

Options:
  --dry-run         Show cleanup steps without running them
  --system          Also clean the detected APT, DNF, pacman, or Zypper cache
                    (requires root or sudo)
  --full-bun-cache  Remove Bun's full cache and shared package store instead of
                    pruning old entries; affected projects may need `bun install`
                    again
  --no-clean-tmp    Skip age-based cleanup of /tmp and /var/tmp
  --containers      Also prune stopped Docker containers, unused networks, and
                    dangling images; Docker volumes are kept
  -h, --help        Show this help

By default, the script cleans available uv, Bun, npm, pnpm, Yarn, pip, Poetry,
Conda, Pixi, Yay, Go, and Docker build caches, then cleans expired temporary files.
System package caches and broader Docker cleanup require their options above.

Examples:
  optimize-disk.sh --dry-run
  optimize-disk.sh --system --containers
EOF
}

while (($#)); do
  case "$1" in
    --dry-run) dry_run=true ;;
    --system) system=true ;;
    --full-bun-cache) full_bun_cache=true ;;
    --no-clean-tmp) clean_tmp=false ;;
    --containers) containers=true ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

free_bytes() {
  df --output=avail -B1 "$HOME" | tail -n 1 | tr -d ' '
}

human_bytes() {
  numfmt --to=iec --suffix=B "$1" 2>/dev/null || printf '%s bytes' "$1"
}

run_cache_tool() {
  local label=$1
  shift
  if ! command -v "$1" >/dev/null 2>&1; then
    return 0
  fi
  if "$dry_run"; then
    printf '[dry-run] %s\n' "$label"
  elif ! "$@" >/dev/null; then
    printf 'Warning: %s failed\n' "$label" >&2
  else
    printf 'Cleared %s\n' "$label"
  fi
}

run_root_cache_tool() {
  local label=$1
  shift
  if "$dry_run"; then
    printf '[dry-run] %s\n' "$label"
  elif ((EUID == 0)); then
    run_cache_tool "$label" "$@"
  elif command -v sudo >/dev/null 2>&1; then
    run_cache_tool "$label" sudo "$@"
  else
    printf 'Warning: skipped %s (requires root or sudo)\n' "$label" >&2
  fi
}

before=$(free_bytes)

# Tool-managed caches use their own commands where available.
run_cache_tool 'uv cache' uv cache clean
if "$full_bun_cache"; then
  run_cache_tool 'Bun full cache and shared package store' bun pm cache rm
else
  run_cache_tool 'old Bun cache entries' bun pm cache prune
fi
run_cache_tool 'npm cache' npm cache clean --force
run_cache_tool 'pnpm unused package cache' pnpm store prune
run_cache_tool 'Yarn cache' yarn cache clean
run_cache_tool 'pip cache' pip cache purge
run_cache_tool 'Poetry cache' poetry --no-interaction cache clear --all
run_cache_tool 'Conda caches and unused packages' conda clean --all --yes
run_cache_tool 'Pixi caches' pixi clean cache --yes
run_cache_tool 'Yay AUR build cache' yay -Sc --aur --noconfirm
run_cache_tool 'Go build cache' go clean -cache
run_cache_tool 'Docker build cache' docker builder prune --force

# Cargo maintains its own global-cache cleanup policy. Avoid deleting its internal
# cache directories directly; Cargo cleans unused downloads during normal use.

if "$clean_tmp"; then
  if command -v systemd-tmpfiles >/dev/null 2>&1; then
    run_root_cache_tool 'expired temporary files under /tmp and /var/tmp' \
      systemd-tmpfiles --clean --prefix=/tmp --prefix=/var/tmp
  else
    printf 'systemd-tmpfiles is not installed; skipped age-based temporary-file cleanup.\n' >&2
  fi
fi

if "$system"; then
  if command -v apt-get >/dev/null 2>&1; then
    run_root_cache_tool 'APT package cache' apt-get clean
  elif command -v dnf >/dev/null 2>&1; then
    run_root_cache_tool 'DNF package cache' dnf clean all
  elif command -v pacman >/dev/null 2>&1; then
    run_root_cache_tool 'Pacman package cache' pacman -Sc --noconfirm
  elif command -v zypper >/dev/null 2>&1; then
    run_root_cache_tool 'Zypper package cache' zypper clean --all
  else
    printf 'No supported system package manager found; skipped system cache.\n'
  fi
fi

if "$containers"; then
  run_cache_tool 'Docker unused resources (volumes are kept)' docker system prune --force
fi

after=$(free_bytes)
if [[ $before =~ ^[0-9]+$ && $after =~ ^[0-9]+$ ]]; then
  gained=$((after - before))
  printf 'Free on %s: %s -> %s (%s gained)\n' \
    "$(df --output=source "$HOME" | tail -n 1 | xargs)" \
    "$(human_bytes "$before")" "$(human_bytes "$after")" "$(human_bytes "$gained")"
else
  printf 'Could not measure free space before and after cleanup.\n'
fi
