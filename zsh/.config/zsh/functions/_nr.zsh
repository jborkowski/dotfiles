# _nr: Project bootstrapper
# Creates a directory, inits jj (git-backed), adds .gitignore, creates a private GitHub repo.
#
# Usage:
#   _nr <project-name>                  private repo, default profile
#   _nr <project-name> --public         public repo
#   _nr <project-name> --profile work   use a different GitHub account
#   _nr <project-name> -d "desc"        with description
#
# Auth: reads token from macOS Keychain (service: gh:github.com, account: <profile>).
#   Falls back to gh auth token if no Keychain entry exists, and auto-caches it.
#
# Requirements: jj, gh

_nr() {
  local name="$1"
  shift 2>/dev/null

  if [[ -z "$name" ]]; then
    echo "Usage: _nr <project-name> [--public] [--description \"...\"]" >&2
    return 1
  fi

  local visibility="--private"
  local description=""
  local gh_profile="default"

  while [[ $# -gt 0 ]]; do
    case "$1" in
      --public) visibility="--public"; shift ;;
      --profile|-p) gh_profile="$2"; shift 2 ;;
      --description|-d) description="$2"; shift 2 ;;
      *) echo "Unknown option: $1" >&2; return 1 ;;
    esac
  done

  local base_dir="${HOME}/sources"
  local project_path="${base_dir}/${name}"

  if [[ -d "$project_path" ]]; then
    echo "Error: directory '$project_path' already exists" >&2
    return 1
  fi

  # 1. Create the dir and init jj (git-backed, colocated)
  echo "Creating project '$name' in $base_dir..."
  jj git init "$project_path" || return 1
  cd "$project_path" || return 1

  # 2. Write a sensible .gitignore
  cat > .gitignore <<- 'EOF'
	# OS
	.DS_Store
	Thumbs.db
	node_modules/
	.env
	.env.local
	*.log
	.vscode/
	.idea/
	__pycache__/
	*.pyc
	*.pyo
	*.swp
	*.swo
	*~
	EOF

  # 3. Create initial commit
  jj new
  jj describe -m "chore: scaffold project structure"
  git branch -M main 2>/dev/null || true

  # gh repo create without --push (jj doesn't use git branches, push fails)
  # We push via jj git push after the repo is created.
  local gh_args=("$name" "$visibility")
  if [[ -n "$description" ]]; then
    gh_args+=("--description" "$description")
  fi

  local gh_ok=false
  echo "Creating GitHub repo ($visibility) as profile '$gh_profile'..."
  if [[ "$gh_profile" == "default" ]]; then
    if gh repo create "${gh_args[@]}"; then
      gh_ok=true
    fi
  else
    local __gh_token=""
    if [[ "$(uname)" == "Darwin" ]]; then
      __gh_token="$(security find-generic-password -s 'gh:github.com' -a "$gh_profile" -w 2>/dev/null)" || true
      if [[ -z "$__gh_token" ]]; then
        __gh_token="$(gh auth token --user "$gh_profile" 2>/dev/null)" || true
        if [[ -n "$__gh_token" ]]; then
          security add-generic-password -s 'gh:github.com' -a "$gh_profile" -w "$__gh_token" -U 2>/dev/null || true
        fi
      fi
    else
      __gh_token="$(gh auth token --user "$gh_profile" 2>/dev/null)" || true
    fi
    if [[ -z "$__gh_token" ]]; then
      echo "No token found for profile '$gh_profile'." >&2
      echo "  Run: gh auth login  (then re-run _nr)" >&2
      return 1
    fi
    if GH_TOKEN="$__gh_token" gh repo create "${gh_args[@]}"; then
      gh_ok=true
    fi
    unset __gh_token
  fi

  if $gh_ok; then
    echo "GitHub repo created"
    # Set up remote and push via jj
    local gh_user="$(gh api user --jq .login 2>/dev/null || echo "$USER")"
    local repo_url="git@github.com:${gh_user}/${name}.git"
    jj git remote add origin "$repo_url" 2>/dev/null || true
    jj git push --remote origin --change @ 2>/dev/null && echo "Pushed to GitHub" || true
  else
    echo "gh repo create failed — see above for details" >&2
  fi

  echo "Done! Project '$name' is ready in $(pwd)"
  echo "  jj log  — view history"
  echo "  jj git push  — push to GitHub"
}
