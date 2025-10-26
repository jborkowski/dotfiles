# Gemini Development Guidelines

## Code Style

- **Use comments sparingly** - Only comment complex code
- **No promotional footers** - Do not add generated-by footers or similar promotional content

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

**CRITICAL: Always iteratively and proactively fetch and update information in memory**

**Memory is your persistent knowledge base - use it continuously throughout every session**

### Core Principle

**Proactive & Iterative**: Don't just read memory at the start and write at the end. Continuously fetch, update, and enrich your knowledge graph as you work.

### Available Memory Tools

- `memory-read_graph` - Read the entire knowledge graph
- `memory-search_nodes` - Search for nodes based on a query
- `memory-open_nodes` - Open specific nodes by their names
- `memory-create_entities` - Create new entities in the knowledge graph
- `memory-add_observations` - Add observations to existing entities
- `memory-create_relations` - Create relations between entities
- `memory-delete_entities` - Delete entities from the graph
- `memory-delete_observations` - Delete specific observations
- `memory-delete_relations` - Delete relations between entities

### Iterative Memory Workflow

**1. START: Always fetch context first**
   - Use `memory-read_graph` to see all stored knowledge
   - Use `memory-search_nodes` to find relevant entities
   - Use `memory-open_nodes` to get details on specific concepts

**2. DURING: Update as you discover**
   - **Create entities** immediately when encountering new concepts:
     - Projects, codebases, technologies
     - Important files, modules, or components
     - User preferences and decisions

   - **Add observations** continuously as you learn:
     - Update context in real-time
     - Document patterns and conventions as you find them
     - Note important relationships immediately

   - **Create relations** to link concepts:
     - Connect related components
     - Link decisions to implementations
     - Map dependencies as you discover them

**3. ITERATE: Refetch and refine**
   - Search memory again when context changes
   - Update observations with new insights
   - Remove outdated information immediately
   - Enrich entities with additional details

**4. PERSIST: Save important discoveries**
   - Store architectural decisions
   - Persist user preferences
   - Document patterns for future use

### When to Use Memory

- **Every session start** - Fetch existing knowledge
- **Every new concept** - Create entities immediately
- **Every discovery** - Add observations in real-time
- **Every pattern found** - Document for future sessions
- **Every user preference** - Persist for consistency
- **Every important decision** - Store architectural choices

### Best Practices

- **Proactive fetching** - Search memory before AND during work
- **Immediate updates** - Don't batch memory operations, update as you go
- **Iterative enrichment** - Add details as you learn more
- **Relationship mapping** - Connect entities to build knowledge graph
- **Continuous cleanup** - Remove outdated info immediately
- **Rich observations** - Be specific and detailed in observations

## Gemini Added Memories
- Use <exec> 2>& | errstream to run all scripts
