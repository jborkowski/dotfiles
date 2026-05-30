#!/usr/bin/env -S uv run --script
#
# /// script
# requires-python = ">=3.12"
# dependencies = []
# ///
"""Regenerate the LLM Wiki's machine-readable indexes.

Scans every page under `wiki/` and writes two generated artifacts under `meta/`:

  meta/registry.json   one record per page (type, title, aliases, tags, dates,
                       sources, outbound links, excerpt, and — for source pages —
                       a stable SRC id). Keyed by slug for O(1) lookup.
  meta/backlinks.json  inverted index slug -> [inbound slugs]. An empty list
                       means the page is an orphan.

Design notes:
  * No external deps. The `sources:` frontmatter field uses bare `[[slug]]`
    wikilink shorthand which is NOT valid YAML, so we parse frontmatter
    line-by-line and pull links with a regex rather than a YAML library.
  * Stable source IDs are assigned ONCE and preserved across runs by merging
    with the prior registry. Adding a new source never renumbers existing ones.
    The `[[slug]]` remains the citation key; SRC ids are an extra handle.

Usage:
  uv run skills/llm-wiki/reindex.py                     # cwd = wiki root
  uv run skills/llm-wiki/reindex.py --wiki-root <path>  # explicit root
  # Reads LLM_WIKI_ROOT env var; falls back to cwd.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sys
from datetime import date
from pathlib import Path


def resolve_root() -> Path:
    """Resolve the wiki repo root from CLI arg, env var, or cwd."""
    # Parse --wiki-root from argv before argparse to avoid conflict with unknown args
    for i, arg in enumerate(sys.argv):
        if arg == "--wiki-root" and i + 1 < len(sys.argv):
            return Path(sys.argv[i + 1]).resolve()
        if arg.startswith("--wiki-root="):
            return Path(arg.split("=", 1)[1]).resolve()
    env = os.environ.get("LLM_WIKI_ROOT", "").strip()
    if env:
        return Path(env).resolve()
    return Path.cwd()


ROOT = resolve_root()
WIKI_DIR = ROOT / "wiki"
META_DIR = ROOT / "meta"
REGISTRY_PATH = META_DIR / "registry.json"
BACKLINKS_PATH = META_DIR / "backlinks.json"

PAGE_TYPES = ("entities", "concepts", "topics", "sources", "syntheses")

LINK_RE = re.compile(r"\[\[([^\]]+)\]\]")
FRONTMATTER_RE = re.compile(r"^---\s*\n(.*?)\n---\s*\n?(.*)$", re.DOTALL)


def link_slugs(text: str) -> list[str]:
    """Every [[wikilink]] target in `text`, normalized to a bare slug.

    Handles `[[slug]]`, `[[slug|alias]]`, `[[slug#fragment]]`, and defensive
    `[[dir/slug]]` forms. Preserves first-seen order, de-duplicated. Targets are
    kept as written (slug *or* alias); resolution to a canonical slug happens
    later via the alias index.
    """
    out: list[str] = []
    for raw in LINK_RE.findall(text):
        slug = raw.split("|", 1)[0]  # drop display alias
        slug = slug.split("#", 1)[0].strip()  # drop heading fragment
        slug = slug.split("/")[-1]  # tolerate folder-qualified links
        if slug and slug not in out:
            out.append(slug)
    return out


def parse_flow_list(value: str) -> list[str]:
    """Parse a simple YAML flow list `[a, b, "c"]` into trimmed strings."""
    value = value.strip()
    if value.startswith("[") and value.endswith("]"):
        value = value[1:-1]
    items = []
    for part in value.split(","):
        part = part.strip().strip('"').strip("'").strip()
        if part:
            items.append(part)
    return items


def unquote(value: str) -> str:
    value = value.strip()
    if len(value) >= 2 and value[0] in "\"'" and value[-1] == value[0]:
        return value[1:-1]
    return value


def split_frontmatter(content: str) -> tuple[dict[str, str], str]:
    """Return (raw frontmatter key->value strings, body)."""
    m = FRONTMATTER_RE.match(content)
    if not m:
        return {}, content
    block, body = m.group(1), m.group(2)
    fm: dict[str, str] = {}
    current_key: str | None = None
    for line in block.splitlines():
        if not line.strip():
            continue
        # `key: value` at column 0 starts a field; indented lines continue it.
        m2 = re.match(r"^(\w[\w\-]*):\s?(.*)$", line)
        if m2 and not line.startswith((" ", "\t")):
            current_key = m2.group(1)
            fm[current_key] = m2.group(2)
        elif current_key is not None:
            fm[current_key] += " " + line.strip()
    return fm, body


def excerpt_of(body: str, limit: int = 200) -> str:
    """First substantive prose line, links flattened to plain text."""
    for line in body.splitlines():
        s = line.strip()
        if not s or s.startswith(("#", ">", "|", "-", "*", "```")):
            continue
        s = LINK_RE.sub(lambda m: m.group(1).split("|")[-1].split("/")[-1], s)
        return s[:limit].rstrip()
    return ""


def load_prior_source_ids() -> dict[str, str]:
    """slug -> source_id from the existing registry, so ids stay stable."""
    if not REGISTRY_PATH.exists():
        return {}
    try:
        prior = json.loads(REGISTRY_PATH.read_text())
    except (json.JSONDecodeError, OSError):
        return {}
    return {
        slug: rec["source_id"]
        for slug, rec in prior.get("pages", {}).items()
        if rec.get("source_id")
    }


def assign_source_id(when: str, taken: set[str]) -> str:
    """Next free SRC-<date>-NNN for `when` (YYYY-MM-DD), avoiding `taken`."""
    day = (when or "0000-00-00").strip()[:10] or "0000-00-00"
    n = 1
    while True:
        candidate = f"SRC-{day}-{n:03d}"
        if candidate not in taken:
            return candidate
        n += 1


def build() -> int:
    if not WIKI_DIR.is_dir():
        print(f"error: no wiki/ found at {ROOT} (set LLM_WIKI_ROOT or use --wiki-root)", file=sys.stderr)
        return 1

    META_DIR.mkdir(exist_ok=True)
    prior_ids = load_prior_source_ids()
    taken_ids = set(prior_ids.values())

    pages: dict[str, dict] = {}
    counts = {t: 0 for t in PAGE_TYPES}

    for path in sorted(WIKI_DIR.rglob("*.md")):
        slug = path.stem
        fm, body = split_frontmatter(path.read_text())
        ptype = fm.get("type", "").strip()
        section = path.parent.name
        if section in counts:
            counts[section] += 1

        rec: dict = {
            "slug": slug,
            "type": ptype or section.rstrip("s"),
            "title": unquote(fm.get("title", slug)),
            "path": str(path.relative_to(ROOT)),
            "aliases": parse_flow_list(fm.get("aliases", "")),
            "tags": parse_flow_list(fm.get("tags", "")),
            "created": fm.get("created", "").strip(),
            "updated": fm.get("updated", "").strip(),
            "sources": link_slugs(fm.get("sources", "")),
            "outbound": link_slugs(body),
            "excerpt": excerpt_of(body),
        }

        if section == "sources" or ptype == "source":
            for key in ("source_type", "source_url", "source_date", "raw_path"):
                if fm.get(key, "").strip():
                    rec[key] = fm[key].strip()
            sid = prior_ids.get(slug)
            if not sid:
                sid = assign_source_id(rec.get("source_date") or rec["created"], taken_ids)
                taken_ids.add(sid)
            rec["source_id"] = sid

        pages[slug] = rec

    # Resolve a link target (slug or alias, any case) to its canonical slug.
    # Obsidian-style: [[LoRA]] and [[lora]] both land on lora.md.
    alias_index: dict[str, str] = {}
    for slug, rec in pages.items():
        alias_index[slug.lower()] = slug
        for alias in rec["aliases"]:
            alias_index.setdefault(alias.lower(), slug)

    def resolve(target: str) -> str | None:
        return alias_index.get(target.lower())

    # Inverted index: every page lists who links to it (frontmatter + body),
    # keyed by canonical slug. Unresolvable targets are recorded as broken.
    backlinks: dict[str, list[str]] = {slug: [] for slug in pages}
    broken: dict[str, list[str]] = {}
    for slug, rec in pages.items():
        for target in dict.fromkeys(rec["outbound"] + rec["sources"]):
            canonical = resolve(target)
            if canonical is None:
                broken.setdefault(slug, []).append(target)
            elif canonical != slug and slug not in backlinks[canonical]:
                backlinks[canonical].append(slug)

    orphans = sorted(s for s, ins in backlinks.items() if not ins)

    registry = {
        "generated": date.today().isoformat(),
        "page_count": len(pages),
        "counts": counts,
        "orphans": orphans,
        "broken_links": broken,
        "pages": pages,
    }

    REGISTRY_PATH.write_text(json.dumps(registry, indent=2, ensure_ascii=False) + "\n")
    BACKLINKS_PATH.write_text(json.dumps(backlinks, indent=2, ensure_ascii=False) + "\n")

    print(f"indexed {len(pages)} pages -> {REGISTRY_PATH}, {BACKLINKS_PATH}")
    print(f"  counts: " + ", ".join(f"{k}={v}" for k, v in counts.items()))
    print(f"  orphans: {len(orphans)}   broken links: {sum(len(v) for v in broken.values())}")
    return 0


if __name__ == "__main__":
    # Strip --wiki-root before argparse (handled above in resolve_root)
    argv = [a for i, a in enumerate(sys.argv) if a != "--wiki-root" and (i == 0 or sys.argv[i - 1] != "--wiki-root") and not a.startswith("--wiki-root=")]
    sys.argv = argv
    raise SystemExit(build())
