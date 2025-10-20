---
description: Delegate file location to Gemini - finds where code lives in the codebase. Usage: /ask-gemini-locate [CATEGORIES: cat1,cat2] <query>
---

Execute this shell command to delegate the task to Gemini CLI:

```bash
OUTPUT_FILE=".gemini/output/locate-$(date +%s).md"

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
gemini "/codebase-locator" "$PROMPT"
echo "Gemini completed. Results saved to: $OUTPUT_FILE"
```

After completion, read the output file and present the findings to the user.

**Usage Examples:**
- `/ask-gemini-locate CATEGORIES: frontend,services Find user authentication components`
- `/ask-gemini-locate Find database migration files`
