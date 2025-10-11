#!/bin/sh
# ==============================================================================
#  Load secrets from Bitwarden Secrets Manager if BW_SECRET is set
# ==============================================================================
if [ -n "$BW_SECRET" ]; then
  # First, check if the 'bws' command-line tool is even installed
  if command -v bws > /dev/null 2>&1; then
    # Use >&2 to print status messages to stderr, which is best practice
    echo "🔐 BW_SECRET found. Loading secrets from Bitwarden..." >&2
    # Fetch secrets, prepend "export ", and evaluate the commands in the current shell
    eval "$(bws secret list --output env -t "$BW_SECRET" | sed 's/^/export /')"
  else
    # Print a warning if the secret is set but the tool is missing
    echo "⚠️  Warning: BW_SECRET is set, but the 'bws' command was not found." >&2
  fi
fi
