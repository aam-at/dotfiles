#!/usr/bin/env bash
# Report memory use and optionally send SIGTERM to explicitly selected PIDs.
# Linux reclaims page cache automatically; --drop-caches is only for benchmarks.
set -uo pipefail

dry_run=false
drop_caches=false
stop_requested=false
declare -a stop_pids=()

usage() {
  cat <<'EOF'
Usage: optimize-memory.sh [OPTIONS]

Report RAM, swap, the largest resident processes, and Linux memory pressure. The
report runs even when an action below is requested.

Options:
  --stop PID     Send SIGTERM to the specified process (repeatable; PID must be > 1)
  --drop-caches  Sync filesystems, then drop clean page, dentry, and inode caches
                 (root required; use only for filesystem benchmarks)
  --dry-run      Show requested actions without signaling processes or dropping caches
  -h, --help     Show this help

Dropping caches can slow later file access; Linux normally reclaims them as needed.

Examples:
  optimize-memory.sh
  optimize-memory.sh --stop 1234 --dry-run
EOF
}

while (($#)); do
  case "$1" in
    --stop)
      if (($# < 2)) || [[ ! $2 =~ ^[0-9]+$ ]] || (( 10#$2 < 2 )); then
        echo '--stop requires a numeric PID greater than 1.' >&2
        exit 2
      fi
      stop_pids+=("$2")
      stop_requested=true
      shift 2
      ;;
    --drop-caches) drop_caches=true; shift ;;
    --dry-run) dry_run=true; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
done

printf '%s\n' 'Memory and swap:'
free -h

printf '\n%s\n' 'Largest resident processes:'
ps -eo pid,ppid,user,%mem,rss,etimes,comm --sort=-rss | awk '
  NR == 1 { printf "%-8s %-8s %-16s %6s %9s %8s %s\n", "PID", "PPID", "USER", "%MEM", "RSS(MiB)", "ELAPSED", "COMMAND"; next }
  NR <= 11 { printf "%-8s %-8s %-16s %6s %9.1f %8s %s\n", $1, $2, $3, $4, $5 / 1024, $6, $7 }
'

if [[ -r /proc/pressure/memory ]]; then
  printf '\n%s\n' 'Memory pressure (PSI):'
  cat /proc/pressure/memory
fi

if "$stop_requested"; then
  for pid in "${stop_pids[@]}"; do
    if ! kill -0 "$pid" 2>/dev/null; then
      printf 'PID %s is not running; skipped.\n' "$pid" >&2
    elif "$dry_run"; then
      printf '[dry-run] send SIGTERM to PID %s\n' "$pid"
    elif kill -TERM "$pid"; then
      printf 'Sent SIGTERM to PID %s.\n' "$pid"
    else
      printf 'Could not signal PID %s.\n' "$pid" >&2
    fi
  done
fi

if "$drop_caches"; then
  printf '%s\n' 'Warning: dropping kernel caches is for repeatable filesystem benchmarks; it can slow later file access.' >&2
  if "$dry_run"; then
    printf '[dry-run] sync, then set vm.drop_caches=3\n'
  elif ((EUID != 0)); then
    printf 'Run with sudo to drop kernel caches.\n' >&2
    exit 1
  elif sync && sysctl -w vm.drop_caches=3; then
    :
  else
    printf 'Failed to drop kernel caches.\n' >&2
    exit 1
  fi
fi

if "$stop_requested" || "$drop_caches"; then
  printf '\n%s\n' 'Memory after requested actions:'
  free -h
fi
