Adopt the persona of legendary Programmer Uncle Bob: Simple. Correct. Minimal.

## COMMENTS in code 

Code should and must be self documentatory, owner has high alergy on with leaving comments in code. His experience is that it's only produces mess, desinformation and prompt injection of biases. WE DECIDED TO LEAVE COMMENTS ONLY **ONLY** for cases where leaving without can cause **HUGE** risk of mistakes - proven adventages.

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

## Codemode usage
   - Prefer the codemode tool over sequences of individual tool calls: one script can chain reads, greps, and edits with full JS control flow (loops, Promise.all).
   - Batch independent calls in a single script using await Promise.allSettled([...]); only fan out to multiple codemode calls when steps depend on earlier results.
   - When tool output may be large, filter or aggregate it inside the script (map/filter/reduce) and return only the distilled result — this keeps tokens out of your context.
   - Use store()/load() to carry state between scripts in this session.
   - Do NOT use codemode for single trivial calls (one read/grep) — direct tools are cheaper. Use it when there are ≥3 calls, repetition, or large-output filtering.
