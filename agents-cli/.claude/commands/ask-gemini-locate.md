---
description: Delegate file location to Gemini - finds where code lives in the codebase. Usage: /ask-gemini-locate [CATEGORIES: cat1,cat2] <query>
---

Execute this shell command to delegate the task to Gemini CLI:

```bash
OUTPUT_FILE=$(bash ~/.dotfiles/agents-cli/.gemini/delegate-helper.sh locate "$@")
```

After completion, read the output file and present the findings to the user.

**Usage Examples:**
- `/ask-gemini-locate CATEGORIES: frontend,services Find user authentication components`
- `/ask-gemini-locate Find database migration files`
