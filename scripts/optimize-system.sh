#!/usr/bin/env bash
# Report system health; run system maintenance only when explicitly requested.
set -uo pipefail

dry_run=false
trim_now=false
clean_tmp=false
flush_dns=false
startup=false
rebuild_fonts=false
disk_health=false
firmware_updates=false
max_uptime_days=7

usage() {
  cat <<'EOF'
Usage: optimize-system.sh [OPTIONS]

Report uptime, filesystem space, failed services, crash dumps, reboot status, SSD
trim schedule, power profile, and Btrfs usage/scrub status when the root filesystem
uses Btrfs. Maintenance actions are opt-in; read-only status checks always run.

Options:
  --trim-now           Run fstrim on supported mounted filesystems (root or sudo)
  --clean-tmp          Run configured age-based cleanup under /tmp and /var/tmp
                       (root or sudo)
  --flush-dns          Flush systemd-resolved's DNS cache if the service is active
  --startup            List enabled system and user services
  --rebuild-font-cache Rebuild fontconfig caches with fc-cache
  --disk-health        List NVMe devices and read their SMART health logs (sudo may
                       prompt)
  --firmware-updates   Check for firmware updates using fwupd's cached metadata;
                       does not refresh metadata or install updates
  --max-uptime-days N  Warn when uptime exceeds N days (default: 7)
  --dry-run            Show requested maintenance commands without applying them
  -h, --help           Show this help

Examples:
  optimize-system.sh
  optimize-system.sh --disk-health --firmware-updates
  optimize-system.sh --trim-now --clean-tmp --dry-run
EOF
}

while (($#)); do
  case "$1" in
    --trim-now) trim_now=true; shift ;;
    --clean-tmp) clean_tmp=true; shift ;;
    --flush-dns) flush_dns=true; shift ;;
    --startup) startup=true; shift ;;
    --rebuild-font-cache) rebuild_fonts=true; shift ;;
    --disk-health) disk_health=true; shift ;;
    --firmware-updates) firmware_updates=true; shift ;;
    --dry-run) dry_run=true; shift ;;
    --max-uptime-days)
      if (($# < 2)) || [[ ! $2 =~ ^[0-9]+$ ]] || (( 10#$2 < 1 )); then
        echo '--max-uptime-days requires a positive integer.' >&2
        exit 2
      fi
      max_uptime_days=$((10#$2))
      shift 2
      ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
done

run_root() {
  if "$dry_run"; then
    printf '[dry-run]'
    printf ' %q' "$@"
    printf '\n'
  elif ((EUID == 0)); then
    "$@"
  elif command -v sudo >/dev/null 2>&1; then
    sudo "$@"
  else
    printf 'This action requires root or sudo: %s\n' "$1" >&2
    return 1
  fi
}

printf '%s\n' 'Uptime:'
uptime -p
uptime_seconds=$(awk '{printf "%d", $1}' /proc/uptime)
uptime_days=$((uptime_seconds / 86400))
printf 'Up for %s days.\n' "$uptime_days"
if ((uptime_seconds > max_uptime_days * 86400)); then
  printf 'Warning: uptime exceeds %s days; consider a planned restart if updates or leaks warrant it.\n' \
    "$max_uptime_days" >&2
fi

printf '\n%s\n' 'Filesystem space:'
df -hT / "$HOME"

if command -v btrfs >/dev/null 2>&1 &&
  [[ $(findmnt -no FSTYPE / 2>/dev/null || true) == btrfs ]]; then
  printf '\n%s\n' 'Btrfs filesystem usage:'
  btrfs filesystem usage /
  printf '\n%s\n' 'Btrfs scrub status:'
  btrfs scrub status /
fi

printf '\n%s\n' 'Power profile:'
if command -v powerprofilesctl >/dev/null 2>&1; then
  powerprofilesctl get || printf 'Power Profiles Daemon is not responding.\n'
else
  printf 'powerprofilesctl is not installed.\n'
fi

printf '\n%s\n' 'Failed system services:'
if command -v systemctl >/dev/null 2>&1; then
  systemctl --failed --no-pager --plain || true
  printf '\n%s\n' 'Failed user services:'
  systemctl --user --failed --no-pager --plain 2>/dev/null || true

  printf '\n%s\n' 'SSD trim timer:'
  if trim_timer_status=$(systemctl is-enabled fstrim.timer 2>/dev/null); then
    printf 'Enabled: %s\n' "$trim_timer_status"
  else
    printf 'Enabled: no (or unavailable)\n'
  fi
  systemctl list-timers --all fstrim.timer --no-pager 2>/dev/null ||
    printf 'Could not read fstrim.timer status.\n'
else
  printf 'systemctl is not available.\n'
fi

printf '\n%s\n' 'Reboot status:'
if [[ -e /run/reboot-required ]]; then
  printf 'A reboot is requested by the package manager.\n'
  if [[ -r /run/reboot-required.pkgs ]]; then
    cat /run/reboot-required.pkgs
  fi
elif command -v needs-restarting >/dev/null 2>&1; then
  needs-restarting -r >/dev/null 2>&1
  reboot_status=$?
  case "$reboot_status" in
    0) printf 'No reboot requirement reported by needs-restarting.\n' ;;
    1) printf 'needs-restarting reports a reboot is required.\n' ;;
    *) printf 'Could not determine reboot status (needs-restarting exit %s).\n' \
      "$reboot_status" ;;
  esac
else
  printf 'No standard reboot-required marker was found.\n'
fi

printf '\n%s\n' 'Core dumps from the last 7 days:'
if command -v coredumpctl >/dev/null 2>&1; then
  coredumpctl --since '7 days ago' --no-pager list 2>/dev/null ||
    printf 'No core dump list is available.\n'
else
  printf 'coredumpctl is not installed.\n'
fi

if "$startup"; then
  printf '\n%s\n' 'Enabled system services:'
  systemctl list-unit-files --type=service --state=enabled --no-pager 2>/dev/null || true
  printf '\n%s\n' 'Enabled user services:'
  systemctl --user list-unit-files --type=service --state=enabled --no-pager 2>/dev/null || true
fi

if "$trim_now"; then
  if command -v fstrim >/dev/null 2>&1; then
    run_root fstrim --all --verbose ||
      printf 'fstrim failed on one or more filesystems.\n' >&2
  else
    printf 'fstrim is not installed.\n' >&2
  fi
fi

if "$clean_tmp"; then
  if command -v systemd-tmpfiles >/dev/null 2>&1; then
    run_root systemd-tmpfiles --clean --prefix=/tmp --prefix=/var/tmp ||
      printf 'systemd-tmpfiles cleanup failed.\n' >&2
  else
    printf 'systemd-tmpfiles is not installed.\n' >&2
  fi
fi

if "$flush_dns"; then
  if command -v resolvectl >/dev/null 2>&1 &&
    systemctl is-active --quiet systemd-resolved; then
    if "$dry_run"; then
      printf '[dry-run] resolvectl flush-caches\n'
    elif resolvectl flush-caches; then
      printf 'Flushed systemd-resolved DNS cache.\n'
    else
      printf 'Could not flush the systemd-resolved DNS cache.\n' >&2
    fi
  else
    printf 'systemd-resolved is not active; skipped DNS cache flush.\n' >&2
  fi
fi

if "$rebuild_fonts"; then
  if command -v fc-cache >/dev/null 2>&1; then
    if "$dry_run"; then
      printf '[dry-run] fc-cache --force\n'
    elif fc-cache --force; then
      printf 'Rebuilt fontconfig caches.\n'
    else
      printf 'Could not rebuild fontconfig caches.\n' >&2
    fi
  else
    printf 'fc-cache is not installed.\n' >&2
  fi
fi

if "$disk_health"; then
  if command -v nvme >/dev/null 2>&1; then
    printf '\n%s\n' 'NVMe devices:'
    nvme list
    found_nvme=false
    for controller in /dev/nvme[0-9]*; do
      [[ -c $controller && $controller =~ ^/dev/nvme[0-9]+$ ]] || continue
      found_nvme=true
      printf '\nHealth log for %s:\n' "$controller"
      run_root nvme log smart "$controller" ||
        printf 'Could not read health log for %s.\n' "$controller" >&2
    done
    if ! "$found_nvme"; then
      printf 'No NVMe controller devices were found.\n'
    fi
  else
    printf 'nvme-cli is not installed.\n' >&2
  fi
fi

if "$firmware_updates"; then
  if command -v fwupdmgr >/dev/null 2>&1; then
    printf '\n%s\n' 'Firmware updates (using cached metadata):'
    fwupdmgr get-updates
    fwupd_status=$?
    case "$fwupd_status" in
      0|2) ;;
      *) printf 'Could not query fwupd for available firmware updates (exit %s).\n' \
        "$fwupd_status" >&2 ;;
    esac
  else
    printf 'fwupd is not installed.\n' >&2
  fi
fi
