#!/bin/bash
# Git prompt functions for bash

# Get current git branch
git_branch() {
    local branch
    if branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null); then
        if [[ "$branch" == "HEAD" ]]; then
            # Detached HEAD state, show commit hash
            branch=$(git rev-parse --short HEAD 2>/dev/null)
        fi
        echo "$branch"
    fi
}

# Check if git directory is dirty
git_dirty() {
    if [[ -n $(git status --porcelain 2>/dev/null) ]]; then
        echo "*"
    fi
}

# Get git status relative to upstream
git_upstream_status() {
    local upstream ahead behind
    upstream=$(git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null)

    if [[ -n "$upstream" ]]; then
        ahead=$(git rev-list @{u}..HEAD --count 2>/dev/null)
        behind=$(git rev-list HEAD..@{u} --count 2>/dev/null)

        if [[ "$ahead" -gt 0 ]] && [[ "$behind" -gt 0 ]]; then
            echo "⇕"
        elif [[ "$ahead" -gt 0 ]]; then
            echo "↑$ahead"
        elif [[ "$behind" -gt 0 ]]; then
            echo "↓$behind"
        fi
    fi
}

# Complete git prompt info
git_prompt_info() {
    local branch dirty upstream
    branch=$(git_branch)

    if [[ -n "$branch" ]]; then
        dirty=$(git_dirty)
        upstream=$(git_upstream_status)
        echo " ($branch$dirty$upstream)"
    fi
}

# Export functions for use in prompt
export -f git_branch
export -f git_dirty
export -f git_upstream_status
export -f git_prompt_info
