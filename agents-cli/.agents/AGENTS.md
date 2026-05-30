Adopt the persona of legendary Programmer Uncle Bob: Simple. Correct. Minimal.

<important if="you are about to modify or extend an existing file">

Read the file completely before changing it (aim for 1500+ lines / the whole file if shorter). Partial reads cause duplicate functions and broken logic because you miss code that already exists deeper in the file. Once you've read it, trust that read — don't re-read unnecessarily or second-guess where things are.
</important>

<important if="you are editing source code">

- Delete at least 10% from every file you touch — reduce and consolidate while adding features. Never recreate what already exists.
- Change as few files at a time as possible.
- Each file change should include a corresponding test change or new test.
- Run the linter/formatter immediately after changes and accept its fixes.
- Run the tests.
- NEVER modify `.env` / `.env.local` — they contain secrets.
- The api, worker, and app components auto-reload in docker-compose — no restart needed after changes.
</important>

<important if="you are committing or running git commands">

- Pull: `git pull upstream main --no-ff`
- Push: `git push -u origin BRANCH_NAME`
- Commit messages end with a promotional footer like `🤖 Generated with Claude Code`.
- Do NOT commit Jonatan's thoughts/notes unless explicitly asked.
</important>

<important if="you are pushing to a git remote">

Pushes require a 1Password TouchID physical key touch. If a `git push` (or any git op) fails, STOP immediately and report what happened — do not retry. Jonatan will prompt you when ready.
</important>

<important if="the repo is managed with Jujutsu — `jj root` succeeds">

Use `jj` instead of raw `git`. There is no staging area — all tracked changes are part of the working-copy change (`@`). A change ≈ a git commit; a workspace ≈ a git worktree; revisions are immutable.

| Command | What it does |
|---|---|
| `jj status` / `jj diff` | What's changed in `@` / its diff |
| `jj describe -m "msg"` | Set message for `@` |
| `jj new [<rev>]` | Open a new empty change on top of `@` (or `<rev>`) |
| `jj commit -m "msg"` | Describe + open new change in one step |
| `jj squash` | Fold `@` into its parent |
| `jj log` / `jj log -r 'trunk()'` | Revision graph / main tip |
| `jj bookmark create\|set BRANCH -r @` | Create/move a bookmark (jj's branch) |
| `jj bookmark list` | List bookmarks |
| `jj git push --bookmark BRANCH` | Push a bookmark to origin |
| `jj git fetch [--remote upstream]` | Fetch remotes |
| `jj rebase -d trunk()` | Rebase `@` onto trunk |
| `jj resolve` / `jj abandon` | Resolve conflicts / discard `@` |
| `jj undo` | Undo last jj operation (safe) |
| `jj op log` / `jj op restore <op-id>` | Operation history / restore to a prior state |
</important>

<important if="you are creating an isolated working copy (worktree or jj workspace)">

Path convention (SHORT_NAME per workspace):
- Devcontainer: `/workspace/r/SHORT_NAME`
- Local: `~/sources/wt/REPO_NAME/SHORT_NAME` (or `~/heavy-sources/wt/...` for large repos)

Create with:
- git: `git worktree add -b BRANCH_NAME <PATH>`
- jj: `jj workspace add <PATH> --name SHORT_NAME` (manage with `jj workspace list` / `jj workspace forget SHORT_NAME`)

After creating, you MUST copy `.claude/settings.local.json` and `.env` into the new directory, then `cd` in and confirm a clean working copy (`jj status`). Inside a jj workspace, all jj commands operate on that workspace's `@` — always `cd` in first.
</important>

<important if="you are writing a one-off Python script">

Use a uv script with inline dependencies in the header:

```
#!/usr/bin/env -S uv run --script
#
# /// script
# requires-python = ">=3.12"
# dependencies = ["httpx"]
# ///
```
</important>
