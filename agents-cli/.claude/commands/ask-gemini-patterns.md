---
description: Delegate pattern finding to Gemini - finds similar code implementations and examples. Usage: /ask-gemini-patterns [CATEGORIES: cat1,cat2] <query>
---

Execute this shell command to delegate the task to Gemini CLI:

```bash
OUTPUT_FILE=$(bash ~/.dotfiles/agents-cli/.gemini/delegate-helper.sh patterns "$@")
```

After completion, read the output file and present the findings to the user.

**Usage Examples:**
- `/ask-gemini-patterns CATEGORIES: api-layer,services Find error handling patterns`
- `/ask-gemini-patterns Find pagination patterns`
