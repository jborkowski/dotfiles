---
description: Delegate web research to Gemini - searches and synthesizes information from the web
---

Execute this shell command to delegate the task to Gemini CLI:

```bash
OUTPUT_FILE=$(bash ~/.dotfiles/agents-cli/.gemini/delegate-helper.sh research "$@")
```

After completion, read the output file and present the findings to the user.
