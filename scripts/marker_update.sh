#!/usr/bin/env bash

# Check if all required arguments are provided
if [ "$#" -ne 2 ]; then
  echo "Usage: $0 <pdf_folder> <md_folder>"
  echo "Example: $0 /path/to/pdfs /path/to/mds"
  exit 1
fi

PDF_FOLDER="$1"
MD_FOLDER="$2"
HASH_FILE="$MD_FOLDER/checklist.chk"

# Check if the PDF folder exists
if [ ! -d "$PDF_FOLDER" ]; then
  echo "Error: PDF folder does not exist"
  exit 1
fi

# Check if the hash file exists
if [ ! -f "$HASH_FILE" ]; then
  echo "Error: Hash file does not exist"
  exit 1
fi

# Process each PDF file in the folder
for pdf in "$PDF_FOLDER"/*.pdf; do
  if [ -f "$pdf" ]; then
    filename=$(basename "$pdf")
    computed_hash=$(md5sum "$pdf" | awk '{print $1}')
    file_hash=$(grep -E "([0-9a-f]{32})  ${filename}$" "$HASH_FILE" | awk '{print $1}')
    cmd=("marker_single" "--output_dir" "$MD_FOLDER" "$pdf")
    if [ -z "$file_hash" ]; then
      echo "Hash not found for $filename. Running $cmd."
      if "${cmd[@]}"; then
        echo "$computed_hash  $filename" >>"$HASH_FILE"
        echo "Hash updated for $filename"
      else
        echo "Error: marker_single command failed for $filename"
      fi
    elif [ "$computed_hash" != "$file_hash" ]; then
      echo "Hash mismatch for $filename. Running $cmd."
      if "${cmd[@]}"; then
        sed -i "/ ${filename}$/d" "$HASH_FILE"
        echo "$computed_hash  $filename" >>"$HASH_FILE"
        echo "Hash updated for $filename"
      else
        echo "Error: marker_single command failed for $filename"
      fi
    fi
  fi
done

# Sort the hash file by filename
sort -k2 "$HASH_FILE" -o "$HASH_FILE"

echo "Hash file has been updated."
