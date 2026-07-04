# gac: AI commit using llm (Simon Willison) + macOS Keychain
#
# Auto-detects git or jj repo. For git, uses staging area. For jj, all
# changes are auto-tracked (no staging).
#
# Usage:
#   gac                          # commit changes
#   gac -a                       # (git) git add -A first; (jj) no-op, always all
#   gac "custom prompt"          # override the prompt template
#
# Setup:
#   store-openai-key             # prompts for your OpenAI key, stores in Keychain
#   store-openai-key sk-xxx      # store directly
#
# Requires: llm, git | jj

gac() {
  local add_all=false
  local custom_prompt=""

  # Parse args
  while [[ $# -gt 0 ]]; do
    case "$1" in
      -a) add_all=true; shift ;;
      -h|--help)
        echo "Usage: gac [-a] [\"custom prompt\"]"
        echo "  -a    git add -A before committing"
        return 0
        ;;
      *)
        if [[ -z "$custom_prompt" ]]; then
          custom_prompt="$1"
          shift
        else
          echo "Unknown argument: $1" >&2
          return 1
        fi
        ;;
    esac
  done

  # Read OpenAI key from Keychain
  local openai_key=""
  if [[ "$(uname)" == "Darwin" ]]; then
    openai_key="$(security find-generic-password -s 'openai-api-key' -w 2>/dev/null)" || true
  fi
  if [[ -z "$openai_key" ]]; then
    echo "OpenAI key not found in Keychain." >&2
    echo "  Run: store-openai-key" >&2
    return 1
  fi

  # Detect VCS: prefer jj if in a jj repo, otherwise git
  local vcs="git"
  if jj root &>/dev/null; then
    vcs="jj"
  fi

  # Stage (git only)
  if [[ "$vcs" == "git" ]]; then
    if $add_all; then
      git add -A || return 1
    fi
  fi

  # Get diff
  local diff
  if [[ "$vcs" == "jj" ]]; then
    diff="$(jj diff 2>/dev/null)"
  else
    diff="$(git diff --cached 2>/dev/null)"
  fi
  if [[ -z "$diff" ]]; then
    if [[ "$vcs" == "git" && "$add_all" == false ]]; then
      echo "No staged changes. Use 'gac -a' to add all, or stage manually first." >&2
    else
      echo "Nothing to commit." >&2
    fi
    return 1
  fi

  # Build prompt
  local prompt="${custom_prompt:-Generate a concise git commit message in Google conventional format (type(scope): description). Use types like feat, fix, chore, refactor, docs, test, style, perf. Keep the subject under 72 chars. If needed, add a blank line then bullet points for details. Output ONLY the commit message, no extra text.}"

  # Generate commit message
  echo "Generating commit message..."
  local commit_msg
  commit_msg="$(OPENAI_API_KEY="$openai_key" llm -m gpt-5-mini "$prompt" <<< "$diff" 2>/dev/null)" || {
    echo "llm failed — check your API key and network" >&2
    return 1
  }

  if [[ -z "$commit_msg" ]]; then
    echo "llm returned empty response" >&2
    return 1
  fi

  # Show and confirm
  echo "---"
  echo "$commit_msg"
  echo "---"
  echo -n "Commit with this message? [Y/n/e(dit)] "
  local reply
  read -r reply
  case "$reply" in
    [Nn]*) echo "Aborted."; return 1 ;;
    [Ee]*)
      # Open in editor
      local tmpfile
      tmpfile="$(mktemp)"
      echo "$commit_msg" > "$tmpfile"
      ${EDITOR:-nvim} "$tmpfile"
      commit_msg="$(cat "$tmpfile")"
      rm -f "$tmpfile"
      if [[ -z "$commit_msg" ]]; then
        echo "Empty message after edit. Aborted." >&2
        return 1
      fi
      ;&
    *)
      if [[ "$vcs" == "jj" ]]; then
        jj commit -m "$commit_msg"
      else
        git commit -m "$commit_msg"
      fi
      ;;
  esac
}

# Helper: store OpenAI key in macOS Keychain
store-openai-key() {
  if [[ -n "$1" ]]; then
    security add-generic-password -s openai-api-key -a "$USER" -w "$1" -U
    echo "OpenAI key stored in Keychain"
  else
    echo "Usage: store-openai-key sk-xxxxxxxxxxxx" >&2
    return 1
  fi
}
