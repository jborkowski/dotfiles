#!/bin/bash
# Resolves category names to file paths based on context-categories.json

CONFIG_FILE="${GEMINI_CONFIG_DIR:-.gemini}/context-categories.json"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Use config from script directory if not found in current location
if [ ! -f "$CONFIG_FILE" ]; then
    CONFIG_FILE="$SCRIPT_DIR/context-categories.json"
fi

if [ ! -f "$CONFIG_FILE" ]; then
    echo "Error: Configuration file not found: $CONFIG_FILE" >&2
    exit 1
fi

# Check if jq is available
if ! command -v jq &> /dev/null; then
    echo "Error: jq is required but not installed" >&2
    exit 1
fi

if [ $# -eq 0 ]; then
    echo "Usage: resolve-categories.sh <category1> [category2] [...]" >&2
    echo "" >&2
    echo "Available categories:" >&2
    jq -r '.categories | keys[]' "$CONFIG_FILE" | sed 's/^/  - /' >&2
    exit 1
fi

# Collect all file patterns for requested categories
FILE_PATTERNS=()

for category in "$@"; do
    # Check if category exists
    if ! jq -e ".categories[\"$category\"]" "$CONFIG_FILE" > /dev/null 2>&1; then
        echo "Warning: Unknown category '$category', skipping" >&2
        continue
    fi

    # Get file patterns for this category
    while IFS= read -r pattern; do
        FILE_PATTERNS+=("$pattern")
    done < <(jq -r ".categories[\"$category\"].files[]" "$CONFIG_FILE")
done

# Output all file patterns, one per line
printf '%s\n' "${FILE_PATTERNS[@]}"
