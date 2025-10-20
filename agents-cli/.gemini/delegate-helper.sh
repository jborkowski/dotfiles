#!/bin/bash
# Gemini Delegation Helper
# Usage: delegate-helper.sh <command-type> <query>
# Example: delegate-helper.sh analyze "How does authentication work?"
# Example: delegate-helper.sh patterns "CATEGORIES: database,api Find query patterns"

set -euo pipefail

COMMAND_TYPE="$1"
shift
QUERY="$*"

# Map command types to Gemini commands
case "$COMMAND_TYPE" in
    analyze|a)
        GEMINI_COMMAND="/codebase-analyzer"
        OUTPUT_PREFIX="analyze"
        ;;
    patterns|p)
        GEMINI_COMMAND="/codebase-pattern-finder"
        OUTPUT_PREFIX="patterns"
        ;;
    locate|l)
        GEMINI_COMMAND="/codebase-locator"
        OUTPUT_PREFIX="locate"
        ;;
    research|r)
        GEMINI_COMMAND="/web-search-researcher"
        OUTPUT_PREFIX="research"
        ;;
    *)
        echo "Error: Unknown command type '$COMMAND_TYPE'"
        echo "Usage: $0 <analyze|patterns|locate|research> <query>"
        echo ""
        echo "Short forms: a|p|l|r"
        echo ""
        echo "Examples:"
        echo "  $0 analyze 'How does the user repository work?'"
        echo "  $0 patterns 'CATEGORIES: database Find query composition patterns'"
        echo "  $0 locate 'CATEGORIES: frontend Where are user components?'"
        echo "  $0 research 'Best practices for Opaleye queries'"
        exit 1
        ;;
esac

# Always use current project's .gemini/output directory
OUTPUT_DIR=".gemini/output"

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

# Generate output file with absolute path
OUTPUT_FILE="$(cd "$OUTPUT_DIR" && pwd)/${OUTPUT_PREFIX}-$(date +%s).md"

# Parse categories from query if provided
CATEGORIES=""
CLEANED_QUERY="$QUERY"

if [[ "$QUERY" =~ CATEGORIES:[[:space:]]*([a-zA-Z0-9,_-]+) ]]; then
    CATEGORIES="${BASH_REMATCH[1]}"
    # Remove CATEGORIES prefix from query
    CLEANED_QUERY="${QUERY#*CATEGORIES:*([a-zA-Z0-9,_-])*}"
    CLEANED_QUERY="${CLEANED_QUERY## }"
fi

# Build the prompt
PROMPT="OUTPUT_FILE: $OUTPUT_FILE"

if [ -n "$CATEGORIES" ]; then
    PROMPT="$PROMPT | CATEGORIES: $CATEGORIES"
fi

PROMPT="$PROMPT | $CLEANED_QUERY"

# Invoke Gemini
echo "Delegating to Gemini ($COMMAND_TYPE)..."
echo "Query: $CLEANED_QUERY"
if [ -n "$CATEGORIES" ]; then
    echo "Categories: $CATEGORIES"
fi
echo ""

gemini "$GEMINI_COMMAND" "$PROMPT"

echo ""
echo "✓ Gemini completed. Results saved to: $OUTPUT_FILE"
echo ""

# Output the file path for Claude to read
echo "$OUTPUT_FILE"
