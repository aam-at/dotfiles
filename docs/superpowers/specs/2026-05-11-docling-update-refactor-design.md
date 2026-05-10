# docling_update.sh Refactor Design

## Overview

Refactor `scripts/docling_update.sh` to improve UX, robustness, and performance. The script converts PDFs to Markdown using `docling`, tracking processed files via an md5 hash checklist to skip unchanged PDFs.

## Interface

```
Usage: docling_update.sh [OPTIONS] <pdf_folder> <md_folder>

Options:
  -j, --jobs N     Number of parallel jobs (default: 4, use 1 for sequential)
  -n, --dry-run    Show what would be converted without executing docling
  -h, --help       Show this help message
```

- `<pdf_folder>`: Directory containing PDF files to convert
- `<md_folder>`: Output directory for Markdown files; also where `checklist.chk` lives
- `checklist.chk` is auto-created if it does not exist (no manual pre-creation required)

## Architecture

### Startup

1. Parse flags with a `while` loop (`-j`/`--jobs`, `-n`/`--dry-run`, `-h`/`--help`)
2. Validate `pdf_folder` exists
3. Check `docling` is installed (`command -v docling`) and exit with a clear message if not
4. Auto-create `checklist.chk` if missing (`touch`)
5. Create a lockfile at `checklist.chk.lock`; register `trap` to remove it on exit

### `process_pdf` function

Called once per PDF, in parallel via `xargs -P`. Steps:

1. Compute md5: `md5sum "$pdf" | awk '{print $1}'`
2. Look up stored hash in `checklist.chk` via `grep`
3. **Hash matches** → print `Skipping <filename> (unchanged)` and return
4. **Hash missing or mismatched** → in dry-run mode, print what would run and return; otherwise run `docling "$pdf" --to md --output "$md_folder"`
5. On docling success: use `flock checklist.chk.lock` to safely remove old entry and append new hash
6. On docling failure: print error; increment a shared failure counter (via a temp file counter)

### Main loop

```bash
printf '%s\n' "$PDF_FOLDER"/*.pdf | xargs -P "$JOBS" -I{} bash -c 'process_pdf "$@"' _ {}
```

After all jobs complete, sort `checklist.chk` by filename (`sort -k2`).

### Summary

Print: `Done. X converted, Y skipped, Z failed.`

Exit with code 1 if any PDF failed conversion, 0 otherwise.

## Error Handling

- `set -euo pipefail` at top of script
- Individual PDF failures are caught and logged; processing continues for remaining PDFs
- Exit code reflects whether any failure occurred
- Lockfile is cleaned up via `trap ... EXIT`

## Changes from Current Script

| Before | After |
|--------|-------|
| Hash file must exist or script exits | Auto-created if missing |
| Sequential processing only | Parallel via `xargs -P` with `-j` flag |
| No dry-run mode | `--dry-run` flag added |
| Array printed incorrectly in echo | Fixed to print actual command string |
| No upfront dependency check | `docling` checked at startup |
| No processing summary | Summary line at end |
