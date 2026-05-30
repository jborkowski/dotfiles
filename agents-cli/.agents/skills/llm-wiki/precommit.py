#!/usr/bin/env -S uv run --script
#
# /// script
# requires-python = ">=3.12"
# dependencies = []
# ///
"""Wiki-aware pre-commit fixer for staged `wiki/` pages.

Posture: AUTO-FIX & restage (never blocks the commit). For each file given as
an argument it:

  1. Bumps `updated:` in the frontmatter to today (the page is being committed,
     so it changed).
  2. Normalizes whitespace OUTSIDE fenced code blocks — strips trailing spaces,
     collapses 3+ blank lines to one, and ensures a single trailing newline.
  3. Validates required frontmatter (type/title/created/updated, valid type)
     and prints WARN lines for anything missing — but does NOT fail.

Deliberately does NOT run a general markdown formatter: mdformat/prettier escape
`[[wikilinks]]` (`\\[[x]\\]`) and choke on the non-YAML `sources:` shorthand,
which would corrupt the link graph. So formatting here is the safe subset only —
it never touches wikilinks, frontmatter values, tables, or code.

Usage:
  uv run skills/llm-wiki/precommit.py wiki/a.md wiki/b.md
  # --wiki-root is accepted but not required (file paths are explicit).
Exit code is always 0 (warnings are advisory).
"""

from __future__ import annotations

import os
import re
import sys
from datetime import date
from pathlib import Path

REQUIRED = ("type", "title", "created", "updated")
VALID_TYPES = {"entity", "concept", "topic", "source", "synthesis"}
TODAY = date.today().isoformat()


def fix_file(path: Path) -> tuple[bool, list[str]]:
    """Returns (changed, warnings)."""
    original = path.read_text()
    warnings: list[str] = []

    # --- split frontmatter (first --- ... --- fence) -------------------------
    m = re.match(r"^(---\s*\n)(.*?\n)(---\s*\n)(.*)$", original, re.DOTALL)
    if m:
        open_f, block, close_f, body = m.groups()
        keys = {
            mm.group(1)
            for line in block.splitlines()
            if (mm := re.match(r"^(\w[\w\-]*):", line))
        }
        for req in REQUIRED:
            if req not in keys:
                warnings.append(f"missing frontmatter field `{req}:`")
        tm = re.search(r"^type:\s*(.+?)\s*$", block, re.MULTILINE)
        if tm and tm.group(1) not in VALID_TYPES:
            warnings.append(f"invalid type `{tm.group(1)}` (expected {sorted(VALID_TYPES)})")
        # auto-fix: bump updated: to today
        if "updated" in keys:
            block = re.sub(r"^updated:.*$", f"updated: {TODAY}", block, count=1, flags=re.MULTILINE)
        text = open_f + block + close_f + body
    else:
        warnings.append("no YAML frontmatter found")
        text = original

    text = normalize_whitespace(text)

    changed = text != original
    if changed:
        path.write_text(text)
    return changed, warnings


def normalize_whitespace(text: str) -> str:
    """Strip trailing spaces + collapse blank runs OUTSIDE fenced code blocks;
    guarantee exactly one trailing newline. Code fences are left untouched so
    Lisp blocks keep their exact spacing."""
    out: list[str] = []
    in_fence = False
    blanks = 0
    for line in text.splitlines():
        if re.match(r"^\s*(```|~~~)", line):
            in_fence = not in_fence
            out.append(line.rstrip())
            blanks = 0
            continue
        if in_fence:
            out.append(line)  # verbatim inside code
            continue
        stripped = line.rstrip()
        if stripped == "":
            blanks += 1
            if blanks > 1:
                continue  # collapse 2+ consecutive blanks to 1
        else:
            blanks = 0
        out.append(stripped)
    while out and out[-1] == "":
        out.pop()
    return "\n".join(out) + "\n"


def main(argv: list[str]) -> int:
    any_changed = False
    for arg in argv:
        path = Path(arg)
        if not path.is_file():
            continue
        changed, warnings = fix_file(path)
        for w in warnings:
            print(f"  wiki/precommit WARN  {arg}: {w}", file=sys.stderr)
        if changed:
            any_changed = True
            print(f"  wiki/precommit fixed {arg}")
    if any_changed:
        print("  wiki/precommit: pages normalized + updated: bumped (restaged by hook)")
    return 0  # advisory only — never block the commit


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
