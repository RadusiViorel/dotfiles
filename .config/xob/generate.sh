#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TARGET_SCRIPT="$SCRIPT_DIR/style.sh"

# Read all folder names in that directory
for dir in "$SCRIPT_DIR"/*/; do
    # Skip if no directories are found
    [ -d "$dir" ] || continue

    # Print only the folder name
    folder_name="$(basename "$dir")"

    # Call the other script with the folder name as argument
    "$TARGET_SCRIPT" "$folder_name"
done
