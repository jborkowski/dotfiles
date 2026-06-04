---
name: commit
description: >-
  Create commits with conventional messages (type: description). Use when
  asked to commit or write a message.
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

## Workflow

1. **Inspect**: `git status`, `git diff`, `git log -5 --oneline`

2. **Plan** atomic commits: related files only, correct type, no secrets (`.env`, keys).

3. **Confirm** before committing — list files and full messages; ask to proceed.

4. **Run pre-commit hooks** via subagent (keeps lint/test output out of main context).

   Launch a `delegate` subagent with `context: "fresh"` and this task:

   ```
   Run the commit pre-commit hooks and report pass/fail. Do not edit any files.

   1. Run the skill hook (always): bash ~/.pi/agent/skills/commit/hooks/pre-commit
   2. If .pi/hooks/pre-commit exists at the project root, run it too.

   Environment for every hook:
     COMMIT_FILES="<space-separated paths>"
     COMMIT_MESSAGE="<full message>"
     VCS="git"
     PROJECT_ROOT="<repo root>"

   Report: "PASS" or "FAIL: <reason>" plus each hook's exit code and last 3
   lines of output on failure. If you had to cd into a directory for the
   project, reset back after.
   ```

   If the subagent reports FAIL, **abort** the commit and tell the user why.

5. **Execute** after hooks pass:

```bash
git add <explicit paths>   # never -A, ., or -i
git commit -m "$(cat <<'EOF'
feat: short imperative summary
EOF
)"
git status
```

Multiple commits: repeat add + commit per group. Finish with `git log --oneline -n <count>`.

6. **Run post-commit hooks** via subagent after every successful commit.

   Launch a `delegate` subagent with `context: "fresh"` and `async: true`:

   ```
   Run the commit post-commit hooks and summarize. Do not edit any files.

   1. Run the skill hook (always): bash ~/.pi/agent/skills/commit/hooks/post-commit
   2. If .pi/hooks/post-commit exists at the project root, run it too.

   Environment for every hook:
     COMMIT_FILES="<space-separated paths>"
     COMMIT_MESSAGE="<full message>"
     VCS="git"
     PROJECT_ROOT="<repo root>"

   Report the commit hash, files changed, and any hook warnings.
   Non-zero hook exit is a warning, not a rollback — report it but don't
   treat it as failure.
   ```

## Hooks

Hook scripts run inside **subagents** — not inline — to keep the main session
clean. The subagent runs the shell scripts, reports pass/fail, and the main
agent decides whether to proceed, abort, or warn.

| Tier | Location | When |
|------|----------|------|
| Skill | `~/.pi/agent/skills/commit/hooks/pre-commit` | Always |
| Skill | `~/.pi/agent/skills/commit/hooks/post-commit` | Always |
| Project | `$PROJECT_ROOT/.pi/hooks/pre-commit` | If present |
| Project | `$PROJECT_ROOT/.pi/hooks/post-commit` | If present |

Skill hooks ship with sensible defaults (secrets scan, lint, log). Project hooks
let you add repo-specific checks (typecheck, test suite, changelog update).

Empty project hooks (zero-byte or just comments) are skipped — no-op.

### Writing project hooks

Any executable script works. The agent passes `COMMIT_FILES`, `COMMIT_MESSAGE`,
`VCS`, and `PROJECT_ROOT` as environment variables. Exit non-zero to fail
pre-commit (block the commit) or warn post-commit (report only).

Example `.pi/hooks/pre-commit`:
```bash
#!/usr/bin/env bash
set -euo pipefail
echo "→ running typecheck…"
(cd "$PROJECT_ROOT" && npx tsc --noEmit)
echo "  ✓ typecheck ok"
```

## Rules

- No `Co-Authored-By` or "Generated with …" footers
- Never update git config; never skip hooks unless user asks
- Never amend/squash unless user asks and preconditions are met
- Nothing to commit → say so; do not empty-commit
- Commit only when asked (no push unless asked)
