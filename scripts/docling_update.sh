#!/usr/bin/env bash
set -euo pipefail

JOBS=4
DRY_RUN=false

usage() {
  echo "Usage: $(basename "$0") [OPTIONS] <pdf_folder> [<pdf_folder> ...]"
  echo ""
  echo "Options:"
  echo "  -j, --jobs N     Parallel jobs (default: 4, use 1 for sequential)"
  echo "  -n, --dry-run    Show what would be converted without running docling"
  echo "  -h, --help       Show this help message"
  echo ""
  echo "Each Markdown destination is derived by appending _md to its PDF folder."
  echo "For example:"
  echo "  $(basename "$0") papers mypapers"
  echo "  # converts into papers_md and mypapers_md"
}

while [[ $# -gt 0 ]]; do
  case "$1" in
  -j | --jobs)
    JOBS="$2"
    shift 2
    ;;
  -n | --dry-run)
    DRY_RUN=true
    shift
    ;;
  -h | --help)
    usage
    exit 0
    ;;
  --)
    shift
    break
    ;;
  -*)
    echo "Unknown option: $1" >&2
    usage >&2
    exit 1
    ;;
  *) break ;;
  esac
done

if (($# < 1)); then
  usage >&2
  exit 1
fi

COUNTER_DIR=$(mktemp -d)
LOCK_FILES=()

cleanup() {
  rm -f "${LOCK_FILES[@]}"
  rm -rf "$COUNTER_DIR"
}
trap cleanup EXIT

if ! command -v docling &>/dev/null; then
  echo "Error: docling is not installed or not in PATH" >&2
  exit 1
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
  local stem
  stem="${filename%.*}"
  local output_dir="$md_folder/$stem"

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

  mkdir -p "$output_dir"
  local pdf_rel
  pdf_rel=$(realpath --relative-to="$output_dir" "$pdf")

  if (
    cd "$output_dir"
    docling "$pdf_rel" --to md --image-export-mode referenced --output .
  ); then
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

process_collection() {
  local pdf_folder="$1"
  local md_folder="$2"
  local hash_file="$md_folder/checklist.chk"
  local lock_file="$md_folder/checklist.chk.lock"
  local pdfs

  if [[ ! -d "$pdf_folder" ]]; then
    echo "Error: PDF folder does not exist: $pdf_folder" >&2
    return 1
  fi

  mkdir -p "$md_folder"
  if [[ ! -f "$hash_file" ]]; then
    touch "$hash_file"
    echo "Created hash file: $hash_file"
  fi
  LOCK_FILES+=("$lock_file")

  shopt -s nullglob
  pdfs=("$pdf_folder"/*.pdf)
  if [[ ${#pdfs[@]} -eq 0 ]]; then
    echo "No PDF files found in $pdf_folder"
    return 0
  fi

  printf '%s\n' "${pdfs[@]}" |
    xargs -P "$JOBS" -I{} bash -c \
      'process_pdf "$@"' _ \
      {} "$md_folder" "$hash_file" "$lock_file" "$DRY_RUN" "$COUNTER_DIR"

  sort -k2 "$hash_file" -o "$hash_file"
}

while [[ $# -gt 0 ]]; do
  process_collection "$1" "${1}_md"
  shift
done

converted=$(find "$COUNTER_DIR" -name 'converted_*' | wc -l | tr -d ' ')
skipped=$(find "$COUNTER_DIR" -name 'skipped_*' | wc -l | tr -d ' ')
failed=$(find "$COUNTER_DIR" -name 'failed_*' | wc -l | tr -d ' ')

echo "Done. $converted converted, $skipped skipped, $failed failed."

if [[ $failed -gt 0 ]]; then
  exit 1
fi
