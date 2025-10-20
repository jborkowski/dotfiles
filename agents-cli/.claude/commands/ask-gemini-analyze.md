---
description: Delegate code analysis to Gemini - analyzes implementation details with precise references. Usage: /ask-gemini-analyze [CATEGORIES: cat1,cat2] <query>
---

Execute this shell command to delegate the task to Gemini CLI:

```bash
OUTPUT_FILE=$(bash ~/.dotfiles/agents-cli/.gemini/delegate-helper.sh analyze "$@")
```

After completion, read the output file and present the findings to the user.

**Usage Examples:**
- `/ask-gemini-analyze CATEGORIES: api-layer,database Analyze the webhook processing flow`
- `/ask-gemini-analyze How does user session management work?`
