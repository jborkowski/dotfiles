hl_howto() {
   local sources_dir="${AI_THAT_WORKS_SOURCES:-$HOME/sources}"
   local repo_dir="$sources_dir/ai-that-works"

   if [[ ! -d "$repo_dir" ]]; then
     mkdir -p "$sources_dir"
     gh repo clone ai-that-works/ai-that-works "$repo_dir"
   fi

   ln -sf "$repo_dir/HOWTO.md" ./HOWTO.md
   echo "→ $repo_dir/HOWTO.md  ->  ./HOWTO.md"
 }

# token-revoke: rotate a leaked API token in macOS Keychain.
#
# Usage:
#   token-revoke <provider> [account]       rotate a known provider
#   token-revoke -l|--list                  show all providers + keychain status
#   token-revoke -s <svc> [-a <acc>] [--url <url>]   ad-hoc provider
#
# Strategies (tried in order): api → cli → browser → paste fallback
# Browser strategy: opens token page, polls clipboard for new token, auto-stores.

typeset -gA _tr_service _tr_account _tr_strategy _tr_url _tr_mint _tr_pattern

_tr_register() {
  _tr_service[gh]="gh:github.com"
  _tr_account[gh]="default"
  _tr_strategy[gh]="cli"
  _tr_mint[gh]="gh auth login"
  _tr_pattern[gh]='(ghp_|gho_|github_pat_)[A-Za-z0-9_]{36,}'

  _tr_service[hf]="hf-token"
  _tr_strategy[hf]="browser"
  _tr_url[hf]="https://huggingface.co/settings/tokens"
  _tr_pattern[hf]='hf_[A-Za-z0-9]{30,}'

  _tr_service[anthropic]="ANTHROPIC_API_KEY"
  _tr_strategy[anthropic]="browser"
  _tr_url[anthropic]="https://console.anthropic.com/settings/keys"
  _tr_pattern[anthropic]='sk-ant-api[A-Za-z0-9_-]{30,}'

  _tr_service[openai]="openai-api-key"
  _tr_strategy[openai]="browser"
  _tr_url[openai]="https://platform.openai.com/api-keys"
  _tr_pattern[openai]='sk-[A-Za-z0-9]{20,}'

  _tr_service[deepseek]="deepseek-api-key"
  _tr_strategy[deepseek]="browser"
  _tr_url[deepseek]="https://platform.deepseek.com/api_keys"
  _tr_pattern[deepseek]='(sk-|dsk_)[A-Za-z0-9]{20,}'

  _tr_service[gemini]="gemini-api-key"
  _tr_strategy[gemini]="browser"
  _tr_url[gemini]="https://aistudio.google.com/apikey"
  _tr_pattern[gemini]='AI[A-Za-z0-9_-]{30,}'

  _tr_service[runpod]="RUNPOD_API_KEY"
  _tr_strategy[runpod]="browser"
  _tr_url[runpod]="https://www.runpod.io/console/user/settings"
  _tr_pattern[runpod]='[A-Za-z0-9]{30,}'

  _tr_service[wandb]="WANDB_API_KEY"
  _tr_strategy[wandb]="browser"
  _tr_url[wandb]="https://wandb.ai/authorize"
  _tr_pattern[wandb]='[A-Za-z0-9]{30,}'
}

_tr_store() {
  local service="$1" account="$2" token="$3" label="${4:-token-revoke}"
  local -a cmd=(security add-generic-password -s "$service" -w "$token" -U)
  [[ -n "$account" ]] && cmd+=(-a "$account")
  cmd+=(-J "$label")
  "${cmd[@]}" 2>/dev/null
}

_tr_fetch_current() {
  local service="$1" account="$2"
  local -a cmd=(security find-generic-password -s "$service" -w)
  [[ -n "$account" ]] && cmd+=(-a "$account")
  "${cmd[@]}" 2>/dev/null
}

_tr_discover_account() {
  local service="$1"
  security dump-keychain 2>/dev/null \
    | grep -A20 "\"svce\"<blob>=\"$service\"" \
    | grep '"acct"<blob>=' \
    | head -1 \
    | sed 's/.*<blob>=//' \
    | tr -d '"'
}

_tr_delete() {
  local service="$1" account="$2"
  local -a cmd=(security delete-generic-password -s "$service")
  [[ -n "$account" ]] && cmd+=(-a "$account")
  "${cmd[@]}" &>/dev/null
}

# Poll pbpaste for a new token matching a regex. Prints the token on success.
_tr_watch_clipboard() {
  local pattern="$1" timeout="${2:-60}"
  local old_clip new_clip i=0
  old_clip="$(pbpaste 2>/dev/null)" || old_clip=""

  while (( i < timeout )); do
    sleep 1; (( i++ ))
    new_clip="$(pbpaste 2>/dev/null)" || continue
    if [[ "$new_clip" != "$old_clip" && "$new_clip" =~ $pattern ]]; then
      echo "$new_clip"
      return 0
    fi
    [[ "$TERM" != "dumb" ]] && printf '.' >&2
  done
  echo "" >&2
  echo "token-revoke: timed out waiting for clipboard (${timeout}s)" >&2
  return 1
}

_tr_list() {
  _tr_register
  local p svc acc current st
  local -a sorted
  sorted=(${(on)${(k)_tr_service}})

  printf "%-12s %-24s %-12s %-10s %s\n" "PROVIDER" "SERVICE" "ACCOUNT" "STATUS" "STRATEGY"
  printf "%-12s %-24s %-12s %-10s %s\n" "--------" "-------" "------" "------" "--------"

  for p in "${sorted[@]}"; do
    svc="${_tr_service[$p]}"
    acc="${_tr_account[$p]:-}"
    current="$(_tr_fetch_current "$svc" "$acc")"
    [[ -n "$current" ]] && st="stored" || st="missing"
    printf "%-12s %-24s %-12s %-10s %s\n" \
      "$p" "$svc" "${acc:-(auto)}" "$st" "${_tr_strategy[$p]}"
  done
}

token-revoke() {
  (( ${#_tr_service} )) || _tr_register

  local provider service account strategy url pattern
  local ad_hoc_svc="" ad_hoc_acc="" ad_hoc_url=""

  # ── Arg parsing ──
  if (( $# == 0 )); then
    echo "Usage: token-revoke <provider> [account]" >&2
    echo "       token-revoke -l|--list" >&2
    echo "       token-revoke -s <service> [-a <account>] [--url <url>]" >&2
    echo "Providers: ${(j:, :)${(k)_tr_service}}" >&2
    return 1
  fi

  case "$1" in
    -l|--list) _tr_list; return $? ;;
    -s)
      shift; ad_hoc_svc="$1"; shift
      [[ -z "$ad_hoc_svc" ]] && { echo "token-revoke: -s requires a service name" >&2; return 1; }
      while [[ $# -gt 0 ]]; do
        case "$1" in
          -a) ad_hoc_acc="$2"; shift 2 ;;
          --url) ad_hoc_url="$2"; shift 2 ;;
          *) echo "token-revoke: unknown option: $1" >&2; return 1 ;;
        esac
      done
      provider="_ad_hoc"
      _tr_service[$provider]="$ad_hoc_svc"
      _tr_account[$provider]="$ad_hoc_acc"
      _tr_strategy[$provider]="browser"
      _tr_url[$provider]="$ad_hoc_url"
      _tr_pattern[$provider]='[A-Za-z0-9_-]{20,}'
      ;;
    -*)
      echo "token-revoke: unknown option: $1" >&2; return 1
      ;;
    *)
      provider="$1"; shift
      [[ -z "${_tr_service[$provider]}" ]] && {
        echo "token-revoke: unknown provider '$provider'" >&2
        echo "Providers: ${(j:, :)${(k)_tr_service}}" >&2
        return 1
      }
      [[ $# -gt 0 ]] && account="$1" || account="${_tr_account[$provider]:-}"
      ;;
  esac

  service="${_tr_service[$provider]}"
  strategy="${_tr_strategy[$provider]}"
  url="${_tr_url[$provider]:-}"
  pattern="${_tr_pattern[$provider]}"

  # ── Read current token ──
  local old_token
  old_token="$(_tr_fetch_current "$service" "$account")"

  if [[ -n "$old_token" ]]; then
    echo "🔑 ${old_token:0:8}…${old_token: -4} (${#old_token} chars)"
  else
    echo "ℹ️  No current token in $service — fresh mint only"
  fi

  # Discover the real account name so re-store preserves it.
  local real_account="$account"
  if [[ -z "$real_account" && -n "$old_token" ]]; then
    real_account="$(_tr_discover_account "$service")"
    [[ "$real_account" == "<NULL>" || -z "$real_account" ]] && real_account=""
  fi

  # ── Delete old token ──
  if [[ -n "$old_token" ]]; then
    _tr_delete "$service" "$account"
    echo "🗑  Deleted from Keychain"
  fi

  # ── Try strategies ──
  local new_token=""

  # api — placeholder for providers with token management APIs
  if [[ -z "$new_token" && "$strategy" == "api" ]]; then
    echo "API strategy not yet implemented for $provider" >&2
  fi

  # cli — run provider CLI auth, grab fresh token
  if [[ -z "$new_token" && "$strategy" == "cli" ]]; then
    local mint_cmd="${_tr_mint[$provider]}"
    echo "Running: $mint_cmd"
    echo "───"
    eval "$mint_cmd"
    local rc=$?
    echo "───"
    if (( rc == 0 )); then
      case "$provider" in
        gh) new_token="$(gh auth token 2>/dev/null)" ;;
      esac
      if [[ -n "$new_token" ]]; then
        echo "✓ Got fresh token: ${new_token:0:8}…${new_token: -4}"
      else
        echo "⚠  CLI succeeded but no token captured" >&2
      fi
    else
      echo "⚠  CLI failed (exit $rc)" >&2
    fi
  fi

  # browser — open token page, watch clipboard
  if [[ -z "$new_token" && "$strategy" == "browser" && -n "$url" ]]; then
    echo "Opening $url"
    echo "Create a new token, then copy it — watching clipboard…"
    open "$url"
    new_token="$(_tr_watch_clipboard "$pattern" 60)"
  fi

  # fallback — manual paste
  if [[ -z "$new_token" ]]; then
    echo -n "Paste new token (Ctrl-C to abort): "
    read -rs new_token; echo
    if [[ -z "$new_token" ]]; then
      echo "Aborted." >&2
      # Restore old token if we deleted it
      if [[ -n "$old_token" ]]; then
        echo "Restoring old token…" >&2
        _tr_store "$service" "$real_account" "$old_token"
      fi
      return 1
    fi
  fi

  # ── Store new token ──
  _tr_store "$service" "$real_account" "$new_token" "${provider} rotated" || {
    echo "token-revoke: failed to store new token" >&2; return 1
  }

  echo "✅ $service${real_account:+ ($real_account)} — ${new_token:0:8}…${new_token: -4}"

  # Remind to revoke old token at provider dashboard.
  if [[ -n "$old_token" && -n "$url" ]]; then
    echo ""
    echo "Old token deleted from Keychain. Revoke it at:"
    echo "  $url"
  fi
}
