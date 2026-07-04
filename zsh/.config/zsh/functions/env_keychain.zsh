# import_env: migrate raw secrets from a .env into the TouchID-gated macOS
# Keychain, rewriting each migrated line to fetch its value back at source-time.
#
# Usage:
#   import_env [-u|--upgrade] [path]   # default ./.env
#   env_status [path]                  # show which keys are migrated vs plain
#
# Flow: write secrets as raw KEY=value in .env, run import_env. Secret-looking
# keys (name matches KEY/TOKEN/SECRET/PASSWORD/PASS/API, case-insensitive) move
# into the Keychain via `envkc`; the line becomes KEY="$(envkc get …)". Override
# per line with a trailing `# secret` (force migrate) or `# keep` (force plain).
# Idempotent: already-migrated lines are skipped. To update a secret, paste a
# fresh raw KEY=value over its $(envkc get …) line and re-run.
#
# --upgrade also converts legacy `KEY=$( security find-generic-password … )`
# lines: reads the value from the old keychain (no TouchID) and re-stores it in
# the TouchID-gated keychain. Leaves the old item in place — delete it yourself
# with `security delete-generic-password` once you've confirmed the new one.
#
# Requires: envkc (build with build-envkc.sh), a project = git repo basename.

import_env() {
  local file="./.env" upgrade=false
  while (( $# )); do
    case "$1" in
      -u|--upgrade) upgrade=true ;;
      *) file="$1" ;;
    esac
    shift
  done
  if [[ ! -f "$file" ]]; then
    echo "import_env: no such file: $file" >&2
    return 1
  fi
  if ! command -v envkc >/dev/null 2>&1; then
    echo "import_env: envkc not found — build it with build-envkc.sh" >&2
    return 1
  fi

  local project
  project="$(git -C "${file:h}" rev-parse --show-toplevel 2>/dev/null)" \
    && project="${project:t}" || project="${PWD:t}"

  local tmp imported=0 upgraded=0 kept=0 skipped=0
  tmp="$(mktemp)" || return 1

  local line key rhs value marker svc acc
  while IFS= read -r line || [[ -n "$line" ]]; do
    # Pass through blanks, comments, and non-assignments untouched.
    if [[ ! "$line" =~ '^(export[[:space:]]+)?([A-Za-z_][A-Za-z0-9_]*)=(.*)$' ]]; then
      print -r -- "$line" >>"$tmp"; continue
    fi
    local prefix="$match[1]" ; key="$match[2]" ; rhs="$match[3]"

    # --upgrade: convert legacy `$( security find-generic-password … )` lines by
    # reading the old keychain item and re-storing it TouchID-gated. We parse out
    # -s/-a and call security ourselves (no eval of the .env line).
    if $upgrade && [[ "$rhs" == *'security find-generic-password'* ]]; then
      svc="" ; acc=""
      [[ "$rhs" =~ "-s[[:space:]]+['\"]?([^'\" ]+)" ]] && svc="$match[1]"
      [[ "$rhs" =~ "-a[[:space:]]+['\"]?([^'\" ]+)" ]] && acc="$match[1]"
      if [[ -z "$svc" ]]; then
        echo "import_env: can't parse security line for $key — leaving as-is" >&2
        print -r -- "$line" >>"$tmp"; ((skipped++)); continue
      fi
      local -a aopt; aopt=(); [[ -n "$acc" ]] && aopt=(-a "$acc")
      value="$(security find-generic-password -s "$svc" "${aopt[@]}" -w 2>/dev/null)"
      if [[ -z "$value" ]]; then
        echo "import_env: legacy lookup failed for $key (-s $svc) — leaving as-is" >&2
        print -r -- "$line" >>"$tmp"; ((skipped++)); continue
      fi
      if ! printf '%s' "$value" | envkc set -p "$project" -k "$key"; then
        echo "import_env: failed to store $key — leaving as-is" >&2
        print -r -- "$line" >>"$tmp"; continue
      fi
      value=""
      print -r -- "${prefix}${key}=\"\$(envkc get -p '${project}' -k '${key}')\"" >>"$tmp"
      ((upgraded++)); continue
    fi

    # Already migrated / dynamic value (any command substitution) → leave as-is.
    if [[ "$rhs" == *'$('* ]]; then
      print -r -- "$line" >>"$tmp"; ((skipped++)); continue
    fi

    # Trailing `# secret` / `# keep` marker overrides the name heuristic.
    marker=""
    if [[ "$rhs" =~ '^(.*[^[:space:]])[[:space:]]+#[[:space:]]*(secret|keep)[[:space:]]*$' ]]; then
      value="$match[1]" ; marker="$match[2]"
    else
      value="$rhs"
    fi
    value="${value%\"}" ; value="${value#\"}"   # strip one layer of quotes
    value="${value%\'}" ; value="${value#\'}"

    local migrate=false
    case "$marker" in
      secret) migrate=true ;;
      keep)   migrate=false ;;
      *) [[ "${key:u}" == (*KEY*|*TOKEN*|*SECRET*|*PASSWORD*|*PASS*|*API*) ]] && migrate=true ;;
    esac

    if ! $migrate; then
      print -r -- "$line" >>"$tmp"; ((kept++)); continue
    fi

    if ! printf '%s' "$value" | envkc set -p "$project" -k "$key"; then
      echo "import_env: failed to store $key — leaving line raw" >&2
      print -r -- "$line" >>"$tmp"; continue
    fi
    print -r -- "${prefix}${key}=\"\$(envkc get -p '${project}' -k '${key}')\"" >>"$tmp"
    ((imported++))
  done <"$file"

  # Preserve mode, then atomically replace.
  chmod "$(stat -f '%Lp' "$file")" "$tmp" 2>/dev/null
  mv "$tmp" "$file" || { rm -f "$tmp"; return 1; }

  echo "import_env [$project]: $imported imported, $upgraded upgraded, $kept kept plain, $skipped skipped"
}

env_status() {
  local file="${1:-./.env}"
  [[ -f "$file" ]] || { echo "env_status: no such file: $file" >&2; return 1; }
  local line
  while IFS= read -r line || [[ -n "$line" ]]; do
    [[ "$line" =~ '^(export[[:space:]]+)?([A-Za-z_][A-Za-z0-9_]*)=(.*)$' ]] || continue
    if [[ "$match[3]" == *'envkc get'* ]]; then
      print -r -- "  🔐 $match[2]"
    else
      print -r -- "  📄 $match[2]"
    fi
  done <"$file"
}
