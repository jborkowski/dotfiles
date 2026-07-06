# _nr: Project bootstrapper
# Creates a directory, inits git, adds .gitignore, creates a private GitHub repo.
# Registers the repo in Orca when the orca CLI is available.
#
# Usage:
#   _nr <project-name>                  private repo, default profile
#   nr <project-name>                   alias for _nr
#   _nr <project-name> --public         public repo
#   _nr <project-name> --profile work   use a different GitHub account
#   _nr <project-name> -d "desc"        with description
#
# Auth: reads token from macOS Keychain (service: gh:github.com, account: <profile>).
#   Falls back to gh auth token if no Keychain entry exists, and auto-caches it.
#
# Requirements: git, gh

_nr() {
  local name="$1"
  shift

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

  echo "Creating project '$name' in $base_dir..."
  mkdir -p "$project_path"
  cd "$project_path"
  git init --initial-branch=main

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

  git add -A
  git commit -m "chore: scaffold project structure"

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
    local __gh_token
    if [[ "$(uname)" == "Darwin" ]]; then
      __gh_token="$(security find-generic-password -s 'gh:github.com' -a "$gh_profile" -w 2>/dev/null)"
      if [[ -z "$__gh_token" ]]; then
        __gh_token="$(gh auth token --user "$gh_profile" 2>/dev/null)"
        if [[ -n "$__gh_token" ]]; then
          security add-generic-password -s 'gh:github.com' -a "$gh_profile" -w "$__gh_token" -U
        fi
      fi
    else
      __gh_token="$(gh auth token --user "$gh_profile" 2>/dev/null)"
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
    local gh_user="$(gh api user --jq .login)"
    local repo_url="git@github.com:${gh_user}/${name}.git"
    git remote add origin "$repo_url"
    git push -u origin main && echo "✓ Pushed to GitHub"
  else
    echo "gh repo create failed — see above for details" >&2
  fi

  if command -v orca > /dev/null; then
    echo "Registering repo in Orca..."
    if orca repo add --path "$project_path"; then
      echo "✓ Added to Orca"
    else
      echo "orca repo add failed — repo is still ready locally" >&2
    fi
  fi

  echo "Done! Project '$name' is ready in $(pwd)"
  echo "  git push  — push to GitHub"
}
