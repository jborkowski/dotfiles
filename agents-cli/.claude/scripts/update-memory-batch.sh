#!/bin/bash
# Batch memory update script - analyzes conversation transcripts and updates Mem0
# Run this periodically (e.g., via cron/launchd every few hours)

set -euo pipefail

# Configuration
TRANSCRIPT_DIR="${CLAUDE_TRANSCRIPT_DIR:-$HOME/.claude/transcripts}"
PROCESSED_DIR="${CLAUDE_TRANSCRIPT_PROCESSED:-$HOME/.claude/transcripts-processed}"
MEMORY_QUEUE="/tmp/claude-memory-queue.txt"

# Create directories if they don't exist
mkdir -p "$PROCESSED_DIR"

# Log function
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >&2
}

# Extract learnings from transcript
# This is a simplified version - could be enhanced with LLM analysis
extract_learnings() {
    local transcript_file="$1"

    # Parse JSON transcript and look for patterns indicating memory-worthy content
    # Patterns to look for:
    # - User preferences mentioned
    # - Technical decisions made
    # - Patterns discovered
    # - New entities/concepts introduced

    if ! [ -f "$transcript_file" ]; then
        return 1
    fi

    # Simple grep-based extraction (can be enhanced)
    # Look for markers like "preference:", "pattern:", "decision:", etc.
    grep -i -E '(preference|pattern|decision|important|remember|always|never)' "$transcript_file" || true
}

# Update Mem0 with learnings via REST API
update_memory() {
    local learning="$1"
    local mem0_server="${MEM0_SERVER:-https://mem0.j14i.me}"
    local user_id="${MEM0_USER_ID:-jonatan}"

    log "Updating memory with: $learning"

    # Escape quotes in learning text for JSON
    local escaped_learning=$(echo "$learning" | sed 's/"/\\"/g')

    # Create memory entry via Mem0 API
    response=$(curl -s -X POST "${mem0_server}/memories" \
        -H "Content-Type: application/json" \
        -d "{
            \"messages\": [
                {\"role\": \"user\", \"content\": \"$escaped_learning\"}
            ],
            \"user_id\": \"$user_id\"
        }" 2>/dev/null)

    if [ $? -eq 0 ]; then
        log "Memory updated successfully"
    else
        log "Failed to update memory"
    fi
}

# Process transcripts
process_transcripts() {
    local count=0

    if ! [ -d "$TRANSCRIPT_DIR" ]; then
        log "Transcript directory not found: $TRANSCRIPT_DIR"
        return 0
    fi

    for transcript in "$TRANSCRIPT_DIR"/*.json; do
        # Skip if no files found
        [ -e "$transcript" ] || continue

        log "Processing: $(basename "$transcript")"

        # Extract learnings
        learnings=$(extract_learnings "$transcript")

        if [ -n "$learnings" ]; then
            log "Found learnings in $(basename "$transcript")"
            # Queue for memory update
            echo "$learnings" >> "$MEMORY_QUEUE"
        fi

        # Move to processed directory
        mv "$transcript" "$PROCESSED_DIR/"
        count=$((count + 1))
    done

    log "Processed $count transcripts"
}

# Process queued memory updates
process_memory_queue() {
    if ! [ -f "$MEMORY_QUEUE" ]; then
        return 0
    fi

    log "Processing memory queue"

    while IFS= read -r learning; do
        update_memory "$learning"
    done < "$MEMORY_QUEUE"

    # Clear queue after processing
    rm -f "$MEMORY_QUEUE"
}

# Main execution
main() {
    log "Starting batch memory update"

    process_transcripts
    process_memory_queue

    log "Batch memory update complete"
}

# Run main function
main "$@"
