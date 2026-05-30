---
name: commit
description: >-
  Create commits with conventional messages (type: description). Uses jj when
  `jj root` succeeds; otherwise git. Use when asked to commit or write a message.
user-invocable: true
---

# Commit (conventional commits)

[Blockly conventional commit format](https://developers.google.com/blockly/guides/contribute/get-started/commits):

```
<type>[!]: <description>

[optional body]
```

- **Type**: `chore` | `deprecate` | `feat` | `fix` | `release` (lowercase)
- **Breaking**: `!` after type (e.g. `fix!: remove legacy API`)
- **Description**: imperative, under 256 chars

| Type | Use for |
|------|---------|
| `feat` | New behavior |
| `fix` | Bug fix |
| `chore` | Tooling, deps, CI |
| `deprecate` | Deprecating behavior |
| `release` | Version / release prep |

## Detect VCS

```bash
jj root 2>/dev/null && echo jj || echo git
```

Use **jj** when `jj root` succeeds; otherwise **git**.

## Workflow

1. **Inspect** (parallel):
   - **jj**: `jj status`, `jj diff`, `jj log -5 --oneline`
   - **git**: `git status`, `git diff`, `git log -5 --oneline`

2. **Plan** atomic commits: related files only, correct type, no secrets (`.env`, keys).

3. **Confirm** before committing — list files and full messages; ask to proceed.

4. **Execute** after approval:

### jj (no staging — all tracked edits are in `@`)

Single commit on `@`:

```bash
jj describe -m "$(cat <<'EOF'
feat: short imperative summary

Optional body.

EOF
)"
```

Finalize `@` and open a fresh empty change on top:

```bash
jj commit -m "$(cat <<'EOF'
feat: short imperative summary
EOF
)"
```

Multiple atomic commits from one `@`: `jj split` (by paths or hunks), describe each resulting change, then `jj new` between splits as needed.

Verify: `jj status`, `jj log -5 --oneline`.

### git

```bash
git add <explicit paths>   # never -A, ., or -i
git commit -m "$(cat <<'EOF'
feat: short imperative summary
EOF
)"
git status
```

Multiple commits: repeat add + commit per group. Finish with `git log --oneline -n <count>`.

## Rules

- No `Co-Authored-By` or "Generated with …" footers
- Never update git/jj config; never skip hooks unless user asks
- Never amend/squash unless user asks and preconditions are met
- Nothing to commit → say so; do not empty-commit
- Commit only when asked (no push unless asked)
