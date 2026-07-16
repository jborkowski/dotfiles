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
- Never add promotional/generated-by footers to commit messages.
- Do NOT commit Jonatan's thoughts/notes unless explicitly asked.
</important>

<important if="you are pushing to a git remote">

Pushes require a 1Password TouchID physical key touch. If a `git push` (or any git op) fails, STOP immediately and report what happened — do not retry. Jonatan will prompt you when ready.
</important>

<important if="you are creating an isolated working copy (worktree)">

Path convention (SHORT_NAME per workspace):
- Devcontainer: `/workspace/r/SHORT_NAME`
- Local: `~/sources/wt/REPO_NAME/SHORT_NAME` (or `~/heavy-sources/wt/...` for large repos)

Create with:
- git: `git worktree add -b BRANCH_NAME <PATH>`

After creating, you MUST copy `.claude/settings.local.json` and `.env` into the new directory, then `cd` in and confirm a clean working copy (`git status`). 
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
