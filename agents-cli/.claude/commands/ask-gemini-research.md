---
description: Delegate web research to Gemini - searches and synthesizes information from the web
---

Execute this shell command to delegate the task to Gemini CLI:

```bash
OUTPUT_FILE=".gemini/output/research-$(date +%s).md"
gemini "/web-search-researcher" "OUTPUT_FILE: $OUTPUT_FILE | $@"
echo "Gemini completed. Results saved to: $OUTPUT_FILE"
```

After completion, read the output file and present the findings to the user.
