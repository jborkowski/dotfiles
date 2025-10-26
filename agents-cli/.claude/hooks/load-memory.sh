#!/bin/bash
# SessionStart hook to load Mem0 knowledge graph context
# This hook automatically injects memory context at the start of each Claude session

set -euo pipefail

# Mem0 server configuration
MEM0_SERVER="${MEM0_SERVER:-https://mem0.j14i.me}"
MEM0_USER_ID="${MEM0_USER_ID:-jonatan}"

# Get project context
PROJECT_NAME=$(basename "$PWD")
CURRENT_DIR="$PWD"

# Function to search Mem0 memories via REST API
call_memory_search() {
    local query="$1"

    # Call Mem0 /search endpoint
    response=$(curl -s -X POST "${MEM0_SERVER}/search" \
        -H "Content-Type: application/json" \
        -d "{\"query\":\"$query\",\"user_id\":\"$MEM0_USER_ID\"}" 2>/dev/null)

    # Extract and format results
    if [ -n "$response" ] && [ "$response" != "null" ]; then
        echo "$response" | jq -r '.results[]? | "- \(.memory)"' 2>/dev/null || echo ""
    else
        echo ""
    fi
}

# Function to retrieve all memories for the user
read_memory_graph() {
    # Call Mem0 /memories endpoint to get all memories
    response=$(curl -s -X GET "${MEM0_SERVER}/memories?user_id=$MEM0_USER_ID" 2>/dev/null)

    # Extract and format all memories
    if [ -n "$response" ] && [ "$response" != "null" ]; then
        echo "$response" | jq -r '.results[]? | "- \(.memory)"' 2>/dev/null || echo ""
    else
        echo ""
    fi
}

# Build context from multiple sources
MEMORY_CONTEXT=""

# Add project-specific memory
PROJECT_MEMORY=$(call_memory_search "$PROJECT_NAME")
if [ -n "$PROJECT_MEMORY" ]; then
    MEMORY_CONTEXT+="## Project Memory: $PROJECT_NAME\n\n$PROJECT_MEMORY\n\n"
fi

# Add current directory context
DIR_MEMORY=$(call_memory_search "$CURRENT_DIR")
if [ -n "$DIR_MEMORY" ]; then
    MEMORY_CONTEXT+="## Directory Context\n\n$DIR_MEMORY\n\n"
fi

# Add general user preferences and patterns
USER_MEMORY=$(call_memory_search "Jonatan preferences patterns")
if [ -n "$USER_MEMORY" ]; then
    MEMORY_CONTEXT+="## User Preferences & Patterns\n\n$USER_MEMORY\n\n"
fi

# If no specific memory found, load recent highlights from full graph
if [ -z "$MEMORY_CONTEXT" ]; then
    FULL_GRAPH=$(read_memory_graph)
    if [ -n "$FULL_GRAPH" ]; then
        MEMORY_CONTEXT="## Recent Knowledge Graph\n\n$FULL_GRAPH"
    fi
fi

# Return JSON with additionalContext for Claude
cat <<EOF
{
  "hookSpecificOutput": {
    "hookEventName": "SessionStart",
    "additionalContext": "# Mem0 Knowledge Graph Context\n\n${MEMORY_CONTEXT}\n\n---\n\nThis context has been automatically loaded from your Mem0 knowledge graph. Continue to update it iteratively as you work."
  }
}
EOF
