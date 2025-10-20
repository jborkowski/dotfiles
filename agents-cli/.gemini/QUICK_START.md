# Quick Start: Memory-Based Project Context

Your project-specific patterns are now stored in the MCP memory system and will be automatically fetched by Gemini when analyzing code.

## What's Already Set Up

### ✅ Memory Entities Created

Three entities are already in your knowledge graph:

1. **Opaleye** (Framework)
   - Custom Haskell ORM with extensible records
   - Requires attention to field order
   - Uses arrow notation for queries

2. **Extensible Records** (Type System Pattern)
   - **CRITICAL**: Field order is sensitive!
   - `'["id", "name"]` ≠ `'["name", "id"]`
   - Use `#!` operator for field access

3. **Project Code Conventions** (Coding Standards)
   - Custom frameworks don't follow global rules
   - Always check existing examples first
   - Field order matters in record types

### ✅ Gemini Commands Updated

All Gemini commands now automatically:
- Search memory for project-specific patterns
- Open relevant entities before analyzing
- Apply custom rules during code analysis

### ✅ Tools Available

All MCP tools are now whitelisted in Gemini settings:
- `memory-search_nodes` - Search for patterns
- `memory-open_nodes` - Get full details
- `memory-create_entities` - Add new patterns
- `memory-add_observations` - Update existing patterns
- Plus: web search (Exa, Tavily, DuckDuckGo), docs (Context7), and more

## How to Use

### Basic Usage (Automatic)

Just run Gemini commands as normal - memory is fetched automatically:

```bash
/ask-gemini-patterns Find database query patterns
/ask-gemini-analyze How does the user repository work?
```

Gemini will:
1. Search memory for "Opaleye", "Extensible Records", etc.
2. Open those entities to get full context
3. Apply your project-specific rules during analysis

### Add New Patterns

When you discover new project-specific patterns, add them to memory:

**Example 1: Add to existing entity**
```
Ask Claude:
"Add observation to Opaleye entity: Always use qualified imports for Opaleye modules"
```

**Example 2: Create new entity**
```
Ask Claude:
"Create memory entity for 'Servant API Pattern' as type 'Framework' with observations:
- Custom servant combinators for authentication
- API routes defined in src/Api/Routes.hs
- Handler pattern uses ReaderT AppContext"
```

**Example 3: Create relation**
```
Ask Claude:
"Create relation: Opaleye uses Servant API Pattern"
```

### View Current Patterns

```
Ask Claude:
"Search memory for Opaleye"
"Open memory nodes: Opaleye, Extensible Records"
"Read entire memory graph"
```

## Adding More Project Context

Use the **SETUP_PROJECT_CONTEXT.md** guide to systematically add:

1. **Custom Frameworks**
   - Name, purpose, key differences
   - Critical rules and constraints
   - Common mistakes to avoid

2. **Type System Patterns**
   - Order sensitivity
   - Type constraints
   - Field access patterns

3. **Architectural Patterns**
   - File organization
   - Module structure
   - Naming conventions

4. **Code Examples**
   - Working examples with file:line
   - When to use each pattern
   - Alternatives and why they're wrong

## Example: Adding a New Pattern

Let's say you discover a new custom validation pattern:

```
Ask Claude:
"Create memory entity for 'Custom Validation Pattern' as type 'Technical Pattern' with observations:
- All validations must return Either ValidationError a
- Validators compose with >>= operator
- Never throw exceptions in validation code
- Example: src/Validators/User.hs:23-45
- Common mistake: Using Maybe instead of Either"
```

Then create a relation:

```
Ask Claude:
"Create relation from 'Project Code Conventions' to 'Custom Validation Pattern' with relation 'enforces'"
```

## Verification

Test that Gemini is using your project context:

```bash
# Test 1: Ask about field order
/ask-gemini-analyze What should I watch out for when working with records?

# Expected: Mentions field order sensitivity

# Test 2: Ask about database
/ask-gemini-patterns Find query composition patterns

# Expected: References Opaleye and arrow notation
```

## Benefits

✅ **No manual category flags** - Memory is always available
✅ **Consistent across tools** - Both Claude and Gemini use same memory
✅ **Easy to update** - Add observations as you discover patterns
✅ **Shared knowledge** - Team can build collective understanding
✅ **No file sync** - Centralized in MCP memory server

## Next Steps

1. **Review** `SETUP_PROJECT_CONTEXT.md` for systematic documentation
2. **Add** more patterns as you discover them
3. **Test** Gemini commands to verify memory usage
4. **Update** observations when patterns change

## Troubleshooting

### Gemini not using project context?

Check that memory tools are working:
```bash
gemini -p "Use memory-search_nodes to search for Opaleye" --yolo
```

### Want to see all patterns?

```
Ask Claude: "Read entire memory graph and show me all Framework and Pattern entities"
```

### Pattern not found?

Create it:
```
Ask Claude: "Create memory entity for [Pattern Name] with observations: [details]"
```

---

**Remember**: Memory is your persistent knowledge base. Keep it updated!
