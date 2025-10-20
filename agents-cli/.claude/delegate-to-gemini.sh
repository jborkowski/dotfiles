#!/bin/bash
# Wrapper script to delegate tasks from Claude Code to Gemini CLI

# Usage: delegate-to-gemini.sh <command> <prompt>

COMMAND="$1"
shift
PROMPT="$*"

if [ -z "$COMMAND" ] || [ -z "$PROMPT" ]; then
    echo "Usage: delegate-to-gemini.sh <command> <prompt>"
    exit 1
fi

# Invoke Gemini CLI with the specified command and prompt
gemini "/$COMMAND" "$PROMPT"
