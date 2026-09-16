#!/usr/bin/env bash
set -euo pipefail

JOBS=4
DRY_RUN=false

usage() {
  cat <<EOF
Usage: $(basename "$0") [OPTIONS] <pdf_folder> [<pdf_folder> ...]

Options:
  -j, --jobs N     Parallel jobs (default: 4, use 1 for sequential)
  -n, --dry-run    Show what would be converted without running docling
  -h, --help       Show this help message

Each Markdown destination is derived by appending _md to its PDF folder.
For example:
  $(basename "$0") papers mypapers
  # converts into papers_md and mypapers_md

A PDF named <bibtex_key>_<material>.pdf (e.g. smith2020_slides.pdf) is
converted into <md_folder>/<bibtex_key>/, next to the paper's own
output, as <bibtex_key>_<material>.md.
EOF
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

  # Companion materials (slides, poster, ...) for a paper share the paper's
  # bibtex-key folder instead of getting one of their own. docling names each
  # file's own markdown/artifacts after its full stem (e.g. "<key>_slides.md"
  # and "<key>_slides_artifacts/"), so different materials for the same key
  # cannot collide there.
  local bibtex_key="${stem%_*}"

  local output_dir="$md_folder/$bibtex_key"
  local artifacts_name="${stem}_artifacts"
  local stage_dir=""

  local computed_hash
  computed_hash=$(md5sum "$pdf" | awk '{print $1}')

  local file_hash
  file_hash=$(grep -E "^[0-9a-f]{32}  ${filename}$" "$hash_file" | awk '{print $1}' || true)

  local output_markdown="$output_dir/$stem.md"
  if [[ "$computed_hash" == "$file_hash" && -s "$output_markdown" ]]; then
    echo "Skipping $filename (unchanged)"
    touch "${counter_dir}/skipped_$$_${RANDOM}"
    return
  fi

  if [[ "$dry_run" == "true" ]]; then
    if [[ "$computed_hash" == "$file_hash" ]]; then
      echo "[dry-run] Would convert $filename (recorded output missing or incomplete)"
    elif [[ -z "$file_hash" ]]; then
      echo "[dry-run] Would convert $filename (new)"
    else
      echo "[dry-run] Would convert $filename (changed)"
    fi
    touch "${counter_dir}/skipped_$$_${RANDOM}"
    return
  fi

  if [[ "$computed_hash" == "$file_hash" ]]; then
    echo "Converting $filename (recorded output missing or incomplete)..."
  elif [[ -z "$file_hash" ]]; then
    echo "Converting $filename (new)..."
  else
    echo "Converting $filename (changed)..."
  fi

  # Convert into a sibling staging directory.  Docling sometimes logs a
  # document-level failure while still exiting with status 0; writing directly
  # to the live directory would then leave partial output behind.
  stage_dir=$(realpath "$(mktemp -d "$md_folder/.${stem}.docling.XXXXXX")")
  local pdf_rel
  pdf_rel=$(realpath --relative-to="$stage_dir" "$pdf")

  local log_file="$stage_dir/docling.log"
  local conversion_succeeded=false
  local attempt_opts
  # docling_parse and GPU OCR are the two native paths that fail on some
  # otherwise valid PDFs; the second attempt falls back to PDFium/CPU OCR.
  for attempt_opts in "" "--pdf-backend pypdfium2 --device cpu"; do
    if [[ -n "$attempt_opts" ]]; then
      echo "Retrying $filename with the PDFium/CPU OCR fallback..." >&2
      rm -rf "$stage_dir"
      stage_dir=$(realpath "$(mktemp -d "$md_folder/.${stem}.docling.XXXXXX")")
      pdf_rel=$(realpath --relative-to="$stage_dir" "$pdf")
      log_file="$stage_dir/docling.log"
    fi
    if (
      cd "$stage_dir"
      # shellcheck disable=SC2086
      docling "$pdf_rel" --to md --image-export-mode referenced --output . $attempt_opts >"$log_file" 2>&1
    ) && [[ -s "$stage_dir/$stem.md" ]] &&
      ! grep -Eqi 'Document .* failed to convert|ERROR.*failed' "$log_file"; then
      conversion_succeeded=true
      break
    fi
  done

  if [[ "$conversion_succeeded" == "true" ]]; then
    # A completed, non-empty Markdown file is the success contract.  Only now
    # replace any prior output and record the input hash.  output_dir may be
    # shared with other materials for the same bibtex key, so only this
    # stem's own markdown/artifacts are touched -- siblings are left alone.
    rm -f "$log_file"
    mkdir -p "$output_dir"

    local backup_dir=""
    if [[ -e "$output_markdown" || -e "$output_dir/$artifacts_name" ]]; then
      backup_dir=$(mktemp -d "$md_folder/.${stem}.previous.XXXXXX")
      [[ -e "$output_markdown" ]] && mv "$output_markdown" "$backup_dir/"
      [[ -e "$output_dir/$artifacts_name" ]] && mv "$output_dir/$artifacts_name" "$backup_dir/"
    fi

    local installed=true
    mv "$stage_dir/$stem.md" "$output_dir/" || installed=false
    if [[ "$installed" == "true" && -e "$stage_dir/$artifacts_name" ]]; then
      mv "$stage_dir/$artifacts_name" "$output_dir/" || installed=false
    fi

    if [[ "$installed" != "true" ]]; then
      rm -f "$output_markdown"
      rm -rf "$output_dir/$artifacts_name"
      if [[ -n "$backup_dir" ]]; then
        [[ -e "$backup_dir/$stem.md" ]] && mv "$backup_dir/$stem.md" "$output_dir/"
        [[ -e "$backup_dir/$artifacts_name" ]] && mv "$backup_dir/$artifacts_name" "$output_dir/"
        rm -rf "$backup_dir"
      fi
      echo "Error: could not install converted output for $filename" >&2
      touch "${counter_dir}/failed_$$_${RANDOM}"
      return
    fi
    rm -rf "$stage_dir" "$backup_dir"

    local hash_tmp
    hash_tmp=$(mktemp "$md_folder/.checklist.chk.XXXXXX")
    if ! (
      flock -x 9
      awk -v filename="$filename" '$2 != filename' "$hash_file" > "$hash_tmp"
      printf '%s  %s\n' "$computed_hash" "$filename" >> "$hash_tmp"
      mv "$hash_tmp" "$hash_file"
    ) 9>"$lock_file"; then
      rm -f "$hash_tmp"
      echo "Error: could not update hash for $filename" >&2
      touch "${counter_dir}/failed_$$_${RANDOM}"
      return
    fi
    echo "Converted: $filename"
    touch "${counter_dir}/converted_$$_${RANDOM}"
  else
    echo "Error: docling failed for $filename" >&2
    [[ -f "$log_file" ]] && sed -n '1,160p' "$log_file" >&2
    rm -rf "$stage_dir"
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
