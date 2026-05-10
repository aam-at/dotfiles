#!/usr/bin/env bash
set -euo pipefail

JOBS=4
DRY_RUN=false

usage() {
  echo "Usage: $(basename "$0") [OPTIONS] <pdf_folder> <md_folder>"
  echo ""
  echo "Options:"
  echo "  -j, --jobs N     Parallel jobs (default: 4, use 1 for sequential)"
  echo "  -n, --dry-run    Show what would be converted without running docling"
  echo "  -h, --help       Show this help message"
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    -j|--jobs)    JOBS="$2"; shift 2 ;;
    -n|--dry-run) DRY_RUN=true; shift ;;
    -h|--help)    usage; exit 0 ;;
    --)           shift; break ;;
    -*)           echo "Unknown option: $1" >&2; usage >&2; exit 1 ;;
    *)            break ;;
  esac
done

if [[ $# -ne 2 ]]; then
  usage >&2
  exit 1
fi

PDF_FOLDER="$1"
MD_FOLDER="$2"
HASH_FILE="$MD_FOLDER/checklist.chk"
LOCK_FILE="$MD_FOLDER/checklist.chk.lock"
COUNTER_DIR=$(mktemp -d)

trap 'rm -f "$LOCK_FILE"; rm -rf "$COUNTER_DIR"' EXIT

if ! command -v docling &>/dev/null; then
  echo "Error: docling is not installed or not in PATH" >&2
  exit 1
fi

if [[ ! -d "$PDF_FOLDER" ]]; then
  echo "Error: PDF folder does not exist: $PDF_FOLDER" >&2
  exit 1
fi

mkdir -p "$MD_FOLDER"
if [[ ! -f "$HASH_FILE" ]]; then
  touch "$HASH_FILE"
  echo "Created hash file: $HASH_FILE"
fi

process_pdf() {
  local pdf="$1"
  local md_folder="$2"
  local hash_file="$3"
  local lock_file="$4"
  local dry_run="$5"
  local counter_dir="$6"

  local filename
  filename=$(basename "$pdf")

  local computed_hash
  computed_hash=$(md5sum "$pdf" | awk '{print $1}')

  local file_hash
  file_hash=$(grep -E "^[0-9a-f]{32}  ${filename}$" "$hash_file" | awk '{print $1}' || true)

  if [[ "$computed_hash" == "$file_hash" ]]; then
    echo "Skipping $filename (unchanged)"
    touch "${counter_dir}/skipped_$$_${RANDOM}"
    return
  fi

  if [[ "$dry_run" == "true" ]]; then
    if [[ -z "$file_hash" ]]; then
      echo "[dry-run] Would convert $filename (new)"
    else
      echo "[dry-run] Would convert $filename (changed)"
    fi
    touch "${counter_dir}/skipped_$$_${RANDOM}"
    return
  fi

  if [[ -z "$file_hash" ]]; then
    echo "Converting $filename (new)..."
  else
    echo "Converting $filename (changed)..."
  fi

  if docling "$pdf" --to md --output "$md_folder"; then
    flock -x "$lock_file" -c "
      grep -v '  ${filename}$' '${hash_file}' > '${hash_file}.tmp' || true
      echo '${computed_hash}  ${filename}' >> '${hash_file}.tmp'
      mv '${hash_file}.tmp' '${hash_file}'
    "
    echo "Converted: $filename"
    touch "${counter_dir}/converted_$$_${RANDOM}"
  else
    echo "Error: docling failed for $filename" >&2
    touch "${counter_dir}/failed_$$_${RANDOM}"
  fi
}

export -f process_pdf

shopt -s nullglob
pdfs=("$PDF_FOLDER"/*.pdf)

if [[ ${#pdfs[@]} -eq 0 ]]; then
  echo "No PDF files found in $PDF_FOLDER"
  exit 0
fi

printf '%s\n' "${pdfs[@]}" \
  | xargs -P "$JOBS" -I{} bash -c \
      'process_pdf "$@"' _ \
      {} "$MD_FOLDER" "$HASH_FILE" "$LOCK_FILE" "$DRY_RUN" "$COUNTER_DIR"

sort -k2 "$HASH_FILE" -o "$HASH_FILE"

converted=$(find "$COUNTER_DIR" -name 'converted_*' | wc -l | tr -d ' ')
skipped=$(find "$COUNTER_DIR" -name 'skipped_*' | wc -l | tr -d ' ')
failed=$(find "$COUNTER_DIR" -name 'failed_*' | wc -l | tr -d ' ')

echo "Done. $converted converted, $skipped skipped, $failed failed."

if [[ $failed -gt 0 ]]; then
  exit 1
fi
