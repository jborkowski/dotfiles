---
description: Delegate pattern finding to Gemini - finds similar code implementations and examples. Usage: /ask-gemini-patterns [CATEGORIES: cat1,cat2] <query>
---

Execute this shell command to delegate the task to Gemini CLI:

```bash
OUTPUT_FILE=".gemini/output/patterns-$(date +%s).md"

# Parse categories from input if provided
INPUT="$@"
CATEGORIES=""
QUERY="$INPUT"

if [[ "$INPUT" =~ CATEGORIES:[[:space:]]*([a-zA-Z0-9,_-]+) ]]; then
    CATEGORIES="${BASH_REMATCH[1]}"
    QUERY="${INPUT#*CATEGORIES:*([a-zA-Z0-9,_-])*}"
    QUERY="${QUERY## }"
fi

# Build the prompt
PROMPT="OUTPUT_FILE: $OUTPUT_FILE"

if [ -n "$CATEGORIES" ]; then
    PROMPT="$PROMPT | CATEGORIES: $CATEGORIES"
fi

PROMPT="$PROMPT | $QUERY"

# Invoke Gemini
gemini "/codebase-pattern-finder" "$PROMPT"
echo "Gemini completed. Results saved to: $OUTPUT_FILE"
```

After completion, read the output file and present the findings to the user.

**Usage Examples:**
- `/ask-gemini-patterns CATEGORIES: api-layer,services Find error handling patterns`
- `/ask-gemini-patterns Find pagination patterns`
