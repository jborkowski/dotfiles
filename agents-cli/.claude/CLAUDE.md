# Claude Development Guidelines

## Code Style

- **Use comments sparingly** - Only comment complex code
- **No promotional footers** - Do not add `🤖 Generated with [Claude Code](https://claude.com/claude-code)`

## Git Workflow

- **Commit incrementally and atomically** - Only commit compiling code
- **Do not commit user's thoughts** - Don't commit Jonatan's thoughts/notes unless explicitly asked
- **Atomic commits** - Each commit should represent a single, complete change

## Critical Rules

- **DO NOT TOUCH `*/translations.json`** - Use dedicated translation tools instead
  - **ASK THE USER** before making any translation changes
  - **ASK, ASK, ASK!!!**

## Build & Compilation

- **Always use post-processor when compiling and building** - Use `errstream -- <exec>` format
- **Build scripts** - Can run without `tail -50`; errstream will truncate output automatically
- **Errstream usage** - Do not use pipes or `2>&1` when using errstream

## Working with Translations

- **Search efficiently** - When searching translations (locales JSON), read only ONE file at a time (e.g., just English locales)

## Key Development Strategy: Example-First Approach

**Look at existing examples FIRST before writing any new code**

### The Right Workflow

1. **Find similar existing code** (2-3 minutes)
   - Look for similar files in the same directory structure
   - Example: Before creating `TestData.hs`, check existing files like `Pizza.hs`
   
2. **Understand patterns and types**
   - Look at imports, helper functions, and data structure patterns
   - Check for helper modules like `Common.hs`
   - See actual usage examples to understand types
   
3. **Copy the structure**
   - Use the same import patterns found in working code
   - Reuse existing helper functions instead of reinventing them
   
4. **Adapt the specific implementation**
   - Modify the copied structure for your specific needs

### Example: Creating Test Files

**What to do:**
- Before creating `TestData.hs`, read existing test file: `/workspace/restaumatic/apps/higgs/test/Restaumatic/Menu/V2/Examples/Pizza.hs`
- Copy working import patterns like:
  ```haskell
  import Restaumatic.Menu.V2.Types
  import Restaumatic.Menu.V2.Parametric
  ```
  instead of guessing individual imports
- Find existing helper functions (like `pl`) instead of creating new ones
- Look at how existing code constructs objects to understand type usage

**What NOT to do:**
1. ❌ Write code based on assumptions
2. ❌ Fix compilation error #1
3. ❌ Fix compilation error #2
4. ❌ Repeat 10+ times...

### Core Principle

**The codebase already has the answers - look there first!**

## Memory & Knowledge Management

**Memory context is automatically loaded at session start** - The knowledge graph is injected into your context when the session begins.

### How It Works

**Passive Auto-Loading**: Memory context loads automatically via SessionStart hook
- Hook location: `.claude/hooks/load-memory.sh`
- Fetches relevant context from Mem0 based on project, directory, and user preferences
- No manual fetching needed - context is already available when session starts

**Manual Saving**: Use `/save-memory` command when you discover important information worth persisting
- Only save meaningful learnings, patterns, decisions, or preferences
- The command analyzes session context and updates Mem0
- Focus on quality over quantity

**Batch Processing**: Background script processes conversation logs periodically
- Script location: `.claude/scripts/update-memory-batch.sh`
- Can be run via cron/launchd for automatic memory updates from transcripts
- Complements manual saves by capturing patterns over time

### When to Use `/save-memory`

Use the manual save command when you discover:
- Important architectural decisions
- User preferences or workflow patterns
- Project-specific conventions or best practices
- Technology stack or tooling choices
- Patterns that should be remembered across sessions


## Task Master AI Instructions (if project contains .taskmaster directory)
**Import Task Master's development workflow commands and guidelines, treat as if import is in the main CLAUDE.md file.**
@./.taskmaster/CLAUDE.md
