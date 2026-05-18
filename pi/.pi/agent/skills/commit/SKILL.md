---
name: commit
description: >-
  Create git commits with conventional commit messages (type: description).
  Use when asked to commit, save changes to git, or write a commit message.
user-invocable: true
---

# Commit (conventional commits)

Create focused git commits using the [Blockly conventional commit format](https://developers.google.com/blockly/guides/contribute/get-started/commits).

## Message format

```
<type>[!]: <description>

[optional body]

[optional footer]
```

- **Type** (required, lowercase): `chore` | `deprecate` | `feat` | `fix` | `release`
- **Breaking change**: append `!` after the type (e.g. `fix!: remove legacy API`)
- **Description** (required): imperative, concise, under 256 characters
- **Body** (optional): blank line after description; wrap lines at 256 characters
- **Footer** (optional): blank line after body

### Type guide

| Type | Use for |
|------|---------|
| `feat` | New behavior or capability |
| `fix` | Bug or error correction |
| `chore` | Tooling, deps, CI, routine maintenance |
| `deprecate` | Deprecating existing behavior |
| `release` | Version or release prep |

Examples: `feat: add worktree baseRef setting`, `fix: ignore stale lock files`, `chore: bump eslint`

## Workflow

1. **Inspect** (run in parallel):
   - `git status`
   - `git diff` (staged and unstaged)
   - `git log -5 --oneline` (match repo tone)

2. **Plan** one or more atomic commits:
   - Group related files only
   - Pick the correct type per commit
   - Draft each message in conventional form
   - Do not stage secrets (`.env`, credentials, keys)

3. **Confirm** before committing:
   - List files per commit
   - Show each full message (`type: description` plus body if any)
   - Ask: "I plan to create [N] commit(s) with these messages. Proceed?"

4. **Execute** after approval:
   - `git add` with explicit paths only (never `git add -A`, `git add .`, or `git add -i`)
   - Commit with a HEREDOC:

```bash
git commit -m "$(cat <<'EOF'
feat: short imperative summary

Optional body explaining why, not what line-by-line.

EOF
)"
```

   - `git status` after each commit
   - `git log --oneline -n <count>` when done

## Rules

- Write messages as the user would; no `Co-Authored-By`, no "Generated with …" footers
- Never update git config
- Never skip hooks (`--no-verify`) unless the user explicitly asks
- Never amend unless the user explicitly asks and amend preconditions are met
- If there is nothing to commit, say so — do not create an empty commit
- Only commit when the user asked to commit (this skill does not imply permission to push)
