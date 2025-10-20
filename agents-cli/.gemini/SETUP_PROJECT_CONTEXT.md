# Project-Specific Context Setup Guide

This guide helps you collect and store your custom frameworks, patterns, and conventions in the MCP memory system so that Gemini can use them when analyzing your code.

## Why Store Project Context in Memory?

- ✅ **Always available**: No need to include category flags every time
- ✅ **Automatically fetched**: Gemini checks memory before analyzing
- ✅ **Easy to update**: Add observations as you discover new patterns
- ✅ **Shared knowledge**: Both Claude and Gemini can access it
- ✅ **No manual file sync**: Memory is centralized

## Setup Checklist

### Step 1: Identify Custom Frameworks

**Questions to ask yourself:**
- [ ] Do I use custom ORMs or database layers?
- [ ] Do I have framework-specific patterns that differ from standard libraries?
- [ ] Are there architectural patterns unique to this project?
- [ ] Do I have type system features with special rules (e.g., extensible records)?

**For each custom framework, document:**
```
Framework Name: _________________
What it does: _________________
Key differences from standard frameworks: _________________
Critical rules to remember: _________________
```

### Step 2: Document Critical Rules

**Especially important for:**
- [ ] **Order-sensitive types** (e.g., record field order matters)
- [ ] **Non-obvious constraints** (e.g., must call X before Y)
- [ ] **Type system quirks** (e.g., phantom types, GADTs)
- [ ] **Performance gotchas** (e.g., avoid pattern X in hot paths)

**Template:**
```
Rule: _________________
Why it matters: _________________
Common mistake: _________________
Correct pattern: _________________
```

### Step 3: Collect Code Examples

**For each pattern, find:**
- [ ] Working example in the codebase
- [ ] File path and line numbers
- [ ] Context about when to use it
- [ ] Alternative approaches and why they're wrong

### Step 4: Store in Memory

Use Claude Code to store your findings in the knowledge graph:

```
Create memory entities for:
1. Framework: [Framework Name]
   - How it works
   - Key patterns
   - Critical rules

2. Type System Pattern: [Pattern Name]
   - Order sensitivity
   - Type constraints
   - Field access patterns

3. Coding Standards: Project Code Conventions
   - Custom framework usage
   - Example-first approach
   - Common mistakes to avoid
```

## Example: Opaleye with Extensible Records

Here's an example of what to collect:

### Framework: Opaleye
- **What**: Custom Haskell ORM for type-safe SQL queries
- **Key difference**: Uses extensible records, not standard row types
- **Critical rule**: Field order in records is type-significant

### Pattern: Extensible Records
- **Order sensitivity**: `'["id" := Int, "name" := Text]` ≠ `'["name" := Text, "id" := Int]`
- **Field access**: Use `#!` operator: `user #! #name`
- **Common mistake**: Assuming field order doesn't matter
- **Correct pattern**: Always maintain consistent field order

### Code Example
```haskell
-- File: src/Database/Users.hs:15-20
type UserRecord = Record '["id" := Column PGInt4, "name" := Column PGText]

queryUsers :: Query UserRecord
queryUsers = selectTable userTable
```

## Commands to Add to Memory

Once you've collected the information, use these commands:

### Add a Framework
```bash
Ask Claude:
"Create a memory entity for [Framework Name] as type 'Framework' with these observations:
- How it works
- Key patterns
- Critical rules"
```

### Add a Type Pattern
```bash
Ask Claude:
"Create a memory entity for [Pattern Name] as type 'Type System Pattern' with these observations:
- Type constraints
- Order sensitivity
- Field access patterns
- Common mistakes"
```

### Add Project Conventions
```bash
Ask Claude:
"Create a memory entity for 'Project Code Conventions' with these observations:
- Custom framework usage patterns
- Example-first development approach
- Common gotchas to avoid"
```

## Verification

After adding to memory, verify:

```bash
Ask Claude:
"Search memory for Opaleye"
"Open memory nodes: Opaleye, Extensible Records, Project Code Conventions"
```

## Updating Context

As you discover new patterns:

```bash
Ask Claude:
"Add observation to Opaleye entity: [new pattern or rule]"
"Create relation between Opaleye and [Another Entity]"
```

## Next Steps

1. **Fill out the checklist** above for your custom frameworks
2. **Ask Claude to create memory entities** with your findings
3. **Test Gemini commands** to verify it uses the project context
4. **Keep updating** as you discover new patterns

## Quick Start Template

Copy this template and fill it in:

```markdown
## My Custom Framework: __________

### What It Does
[Brief description]

### Key Differences from Standard
- Different from [Standard Framework] because...
- Uses [Custom Pattern] instead of [Standard Pattern]

### Critical Rules
1. [CRITICAL Rule #1]
   - Why: [Explanation]
   - Example: [Code]

2. [Important Rule #2]
   - Why: [Explanation]
   - Example: [Code]

### Common Mistakes
❌ **Wrong**: [What not to do]
✅ **Correct**: [What to do instead]

### Code Examples
**Location**: `path/to/file.ext:lines`
```[language]
[Working code example]
```
```

---

**Need help?** Ask Claude:
"Help me set up project-specific context in memory for [your framework/pattern]"
