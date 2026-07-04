---
name: llm-wiki
description: |
  Operate an LLM Wiki repo per AGENTS.md. Subcommands: add, ingest, query, lint,
  reindex, status, help (see ## Subcommands). Use for source ingest, cited queries,
  lint/reindex, and wiki maintenance. Triggers: /llm-wiki, ingest source, query the
  wiki, wiki lint, wiki reindex, wiki status.
---

# llm-wiki — operate the LLM Wiki

This skill drives the wiki defined by `AGENTS.md` at the repo root. **Read `AGENTS.md` first** — it is the canonical schema. This file only describes the operational dispatch.

## Subcommands

Dispatches by first arg:

- `add <path-or-url> [as <type>]` — copy a source into `raw/<type>/` without processing
- `ingest <path-or-url> [as <type>]` — add (if needed) then process: summary page, entity/concept updates, index, log
- `query <question>` — search the wiki, answer with [[citations]], file as synthesis if novel
- `lint` — health-check: contradictions, orphans, stale claims, missing cross-refs
- `reindex` — regenerate `meta/registry.json` + `meta/backlinks.json`
- `status` — counts per page type + last 5 log entries
- `help` — show this list

> **Multi-tool support:** The scripts accept `--wiki-root` and read `LLM_WIKI_ROOT`.
> Global skill: `~/.pi/agent/skills/llm-wiki/SKILL.md`. Run from anywhere:
> ```bash
> LLM_WIKI_ROOT=/path/to/repo uv run /path/to/repo/skills/llm-wiki/reindex.py
> ```

The argument string is `<subcommand> [<args>...]`. Parse the first whitespace-separated token; everything after it is the subcommand's payload.

If args are empty, show `help`.

---

## Common preflight

Before any subcommand, verify the repo is shaped correctly:

```bash
test -f AGENTS.md && test -f index.md && test -f log.md && test -d raw && test -d wiki
```

If any check fails, stop and tell the user the repo isn't initialised as an LLM Wiki.

Today's date for log entries: use the date in the current-conversation environment block (preferred) or `date +%Y-%m-%d` as a fallback.

**Fast lookup layer.** `meta/registry.json` is a generated index of every page
(type, title, aliases, tags, dates, `sources`, outbound links, excerpt, and a
stable `source_id` for sources). `meta/backlinks.json` is the inverted index
(slug → inbound slugs; an empty list ⇒ orphan). The registry also carries
top-level `orphans` and `broken_links`. Prefer these over re-reading every page
or grepping. Regenerate them with the `reindex` subcommand (it is deterministic
and idempotent). If they are missing or stale, run `reindex` first.

---

## Subcommand: `add <path-or-url> [as <type>]`

Copy a source into `raw/<subdir>/` **without** processing it into the wiki.
Useful when the human wants to stage a batch and ingest later.

**Resolving `<type>` → `raw/` subdir:**

| `as` value     | Subdir              | Default for                                      |
| -------------- | ------------------- | ------------------------------------------------ |
| `article`      | `raw/articles/`     | `.md` from web clipper, web pages, blog posts    |
| `paper`        | `raw/papers/`       | `.pdf` academic papers                           |
| `book`         | `raw/books/`        | book chapters, ebook excerpts                    |
| `transcript`   | `raw/transcripts/`  | podcast / video / meeting transcripts            |
| `note`         | `raw/notes/`        | personal journal entries, hand-typed notes       |

If `as <type>` is omitted, infer from extension and content; if ambiguous, **ask** the user via AskUserQuestion with the candidate options.

**Steps:**

1. **Determine source location:**
   - URL → fetch with WebFetch (note: web clipper produces better markdown — flag this to the user). Save to a `.md` file with a kebab-slug filename derived from the page title.
   - Local path inside the repo (already under `raw/`) → no copy needed, just confirm slug.
   - Local path outside the repo → copy to `raw/<subdir>/<kebab-slug>.<ext>`.
2. **Slug:** kebab-case the title. Must be globally unique under `wiki/sources/` since the source page will share this slug. If a collision exists, suffix with `-2`, `-3`, ….
3. **Report** the final path to the user. Do **not** touch `wiki/`, `index.md`, or `log.md` yet — `add` is staging only.

---

## Subcommand: `ingest <path-or-url> [as <type>]`

Follow `AGENTS.md` §5.1 exactly. In short:

1. If the source isn't yet under `raw/`, run the `add` flow first to stage it.
2. **Read it fully** (entire file, not chunks).
3. **Discuss** key takeaways with the human and confirm angle/emphasis before filing. For very short sources (< ~500 words) or when the user has set a batch mode, skip the discussion.
4. **Create `wiki/sources/<slug>.md`** with frontmatter:

   ```yaml
   ---
   type: source
   title: <human-readable title>
   aliases: []
   tags: []
   created: <today>
   updated: <today>
   sources: []
   source_type: article | paper | book | transcript | note
   source_url: <url or omit>
   source_date: <publication date or omit>
   raw_path: raw/<subdir>/<file>
   ---
   ```

   Body: 1–3 paragraph summary + bullet list of key claims/facts/quotes.
5. **Touch entity / concept / topic pages.** For every named thing or idea that matters in this source:
   - Look it up in `index.md` first to avoid near-duplicates.
   - Existing page → update body, append `[[<source-slug>]]` to `sources:` frontmatter, bump `updated:`. Flag contradictions inline: `> ⚠ contradiction with [[other-source]]: ...`.
   - No page → create one in the correct `wiki/<category>/` directory with full frontmatter.
6. **Update `index.md`** — add a one-line entry under the right group for each new page.
7. **Append to `log.md`** (newest at bottom):

   ```
   ## [YYYY-MM-DD] ingest | <Source Title>
   - source: [[<source-slug>]]
   - new pages: [[a]], [[b]]
   - updated pages: [[c]], [[d]]
   - notes: <contradictions, gaps, follow-ups, or "—">
   ```

8. **Reindex.** Run `uv run skills/llm-wiki/reindex.py` so the new/updated
   pages land in `meta/registry.json` + `meta/backlinks.json` and the new source
   gets its stable `source_id`.

10–15 page touches per ingest is normal. Don't artificially limit scope.

---

## Subcommand: `query <question>`

Follow `AGENTS.md` §5.2.

1. **Find candidate pages** by searching `meta/registry.json` (match the
   question's terms against `title`, `aliases`, `tags`, and `excerpt`). Fall
   back to `index.md` if the registry is absent. Use `meta/backlinks.json` to
   pull in closely-connected neighbours of a strong hit.
2. **Read** the candidates, plus pages they link to that look relevant.
3. **Synthesize** an answer for the user. Cite as `[[wiki-page-slug]]` — cite wiki pages, not raw sources directly (pages cite sources; you cite pages).
4. **Decide if it's worth filing:**
   - File when the answer is novel: a new comparison, an analysis spanning multiple sources, a discovered connection, a derived insight.
   - **Don't** file trivial lookups, single-page summaries, or restatements.
   - When borderline, ask the user with AskUserQuestion.
5. **If filing**, create `wiki/syntheses/<slug>.md` with frontmatter (`type: synthesis`, populated `sources:` listing the wiki pages it draws from) and add a line to `index.md` under `## Syntheses`.
6. **Append to `log.md`:**

   ```
   ## [YYYY-MM-DD] query | <one-line question>
   - read: [[a]], [[b]]
   - filed: [[<synthesis-slug>]]      # omit this line entirely if not filed
   ```

---

## Subcommand: `lint`

Follow `AGENTS.md` §5.3. **Run `reindex` first**, then read `meta/registry.json`
— it already computes the mechanical findings. Audit for:

- **Broken links** — `registry.json → broken_links` (link targets that resolve
  to no slug *or alias*). Either fix the link, create the missing page, or note
  it as a follow-up.
- **Orphan pages** — `registry.json → orphans` (pages with no inbound link).
- **Missing-page concepts** — a broken-link target referenced 3+ times is a
  strong candidate for its own page (e.g. a concept stub everyone links to).
- **Contradictions** across pages — search for `⚠` markers and for pages whose
  multiple `sources` disagree.
- **Stale claims** newer sources have superseded (compare `sources` by `source_date`).
- **Missing cross-references** — page A's excerpt/body mentions concept X but X
  is not in its `outbound` list.
- **Frontmatter drift** — `updated:` older than the file's actual mtime.
- **Index drift** — pages in `registry.json` not listed in `index.md`, or index
  entries pointing to slugs absent from the registry.
- **Data gaps** — questions/topics that recur in `log.md` but are thin in the
  wiki; suggest follow-up sources or a web search to the user.

**Apply fixes** for mechanical issues (frontmatter sync, index sync, missing backlinks). **Ask** before any structural change (merging pages, renaming slugs, marking pages `status: deprecated`).

After applying fixes that touched `wiki/`, run `reindex` again so the registry
reflects them. Append a `lint` entry to `log.md`:

```
## [YYYY-MM-DD] lint
- findings: <short list>
- fixes: <short list>
- follow-ups: <questions / suggested sources for the human>
```

---

## Subcommand: `reindex`

Regenerate the machine-readable indexes. No page content changes; safe to run
anytime (deterministic + idempotent — reruns are byte-identical and source IDs
never get renumbered).

```bash
uv run skills/llm-wiki/reindex.py
```

It prints page counts, orphan count, and broken-link count. Run it after any
`ingest` or `lint` that altered `wiki/`, or whenever the indexes look stale.
Report the summary line to the user.

---

## Subcommand: `status`

A quick read-only dashboard. No mutations.

```bash
echo "== counts =="
for d in entities concepts topics sources syntheses; do
  n=$(find wiki/$d -name '*.md' -not -name '.gitkeep' 2>/dev/null | wc -l | tr -d ' ')
  printf "  %-10s %s\n" "$d" "$n"
done
echo
echo "== health (from meta/registry.json, if present) =="
test -f meta/registry.json && python3 -c "
import json
r = json.load(open('meta/registry.json'))
print(f\"  generated   {r['generated']}\")
print(f\"  orphans     {len(r['orphans'])}\")
print(f\"  broken links {sum(len(v) for v in r['broken_links'].values())}\")
" || echo "  (none — run /llm-wiki reindex)"
echo
echo "== recent log =="
grep "^## \[" log.md | tail -5
```

Report the output to the user verbatim. Optionally add a one-line take if anything looks off (e.g., no ingests in 30 days, or a stale `generated` date).

---

## Subcommand: `help`

Print the subcommand list from **## Subcommands** above. No mutations.

---

## Hard rules (mirroring AGENTS.md §8)

1. Never modify anything under `raw/`. Read-only.
2. Never delete a wiki page without explicit human OK. Use `status: deprecated` in frontmatter.
3. Always update `index.md` **and** `log.md` whenever `wiki/` changes.
4. Search `index.md` before creating any new page — near-duplicates are the wiki's biggest threat.
5. Bump `updated:` on every edit; `created:` is immutable.
6. Use `[[wikilinks]]` for all internal references. No raw markdown links between wiki pages.
7. Flag contradictions inline; don't silently overwrite.
8. One entity / one concept / one source per page. Don't merge.

If a hard rule would be broken, **stop and ask** rather than proceeding.
