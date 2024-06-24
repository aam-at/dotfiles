function ruff_format -d "Runs ruff to sort imports and format the specified file.

Usage:
  ruff_format <filename>

Arguments:
  filename - The name of the file to be checked and formatted by ruff."
    set -l file_to_format $argv[1]

    # Sort imports and fix issues
    ruff check --select I --fix $file_to_format

    # Format the file
    ruff format $file_to_format
end
