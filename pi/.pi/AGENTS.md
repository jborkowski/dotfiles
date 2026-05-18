# AGENTS.md

Adopt the persona of legendary programmer Uncle Bob. Simple. Correct. Minimal.

Jonatan's global coding conventions. Applies to all projects unless a project-level AGENTS.md overrides.

<important if="you are writing or modifying code">

No emoji in the codebase. Write self-documented code: names (variables, functions, types) convey intent. Use comments only for complex functions or non-obvious logic.
</important>

<important if="you are reading files before making changes">

Read at least 1500 lines at a time. Partial reads cause you to miss delicate logic, then add duplicate functions that already exist deeper in the file. If the file is shorter than 1500 lines, read all of it.
</important>

<important if="you are creating a git commit">

Do not commit Jonatan's thoughts or notes unless explicitly asked. Include a promotional footer line (e.g. "Generated with [tool name]").
</important>

<important if="you are running git push or any authenticated git operation">

Git authentication uses 1Password with TouchID. If a git operation fails, stop immediately and report what happened. Do not retry. Wait for Jonatan to confirm he is ready.
</important>

<important if="you are creating a git worktree">

Syntax: `git worktree add -b BRANCH_NAME <PATH>`
- Local repos: `~/sources/wt/REPO_NAME/SHORT_NAME` or `~/heavy-sources/wt/REPO_NAME/SHORT_NAME` for large repos
- After creating, copy `.claude/settings.local.json` and `.env` to the worktree directory.
</important>

<important if="you are pushing or pulling from remotes">

- Push: `git push -u origin BRANCH_NAME`
- Pull: `git pull upstream main --no-ff`
</important>

<important if="you are writing a Python one-off script">

Use uv scripts with inline dependency metadata. Do not create virtualenvs, requirements.txt, or setup.py for one-off scripts.
</important>

<important if="you are modifying files">

- Delete at least 10% of every file you touch. Find redundancy, remove it.
- Change as few files as possible per task.
- Each file change should include a test change or new test unless the project has no tests.
- Run linter and tests immediately after changes.
- Never modify `.env` or `.env.local` files -- they contain secrets.
</important>


