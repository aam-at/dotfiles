# pdf-pages.yazi

Preview PDFs one page at a time in Yazi, with page navigation and persisted page state.

## Requirements

- `pdftoppm` from Poppler
- `pdfinfo` from Poppler

## Usage

The plugin is wired as a custom previewer for `application/pdf`, then bound to a few manager keys:

- `[` / `PageUp` for previous page
- `]` / `PageDown` for next page
- `0` / `Home` for first page

The last page you viewed for a PDF is stored in `~/.local/state/yazi/pdf-pages.tsv`, so the next time you hover the same file it resumes on that page.

PDF zoom is intentionally not implemented. The normal `+` / `-` bindings continue to use Yazi's built-in image zoom plugin for images only.
