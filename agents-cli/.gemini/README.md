# Claude → Gemini Agent Delegation System

This system enables Claude Code to delegate tasks to Gemini CLI with intelligent context management and file-based output to save tokens.

## Architecture

```
┌─────────────┐                    ┌──────────────┐
│ Claude Code │ ──(delegates)───→  │  Gemini CLI  │
└─────────────┘                    └──────────────┘
       │                                   │
       │ reads result                      │ writes
       ↓                                   ↓
┌─────────────────────────────────────────────────┐
│       .gemini/output/                           │
│   (timestamped result files)                    │
└─────────────────────────────────────────────────┘
```

## Features

### 1. File-Based Output (Token Efficient)
Instead of transferring results through the API, Gemini saves analysis to files:
- **Saves tokens**: Only confirmation message returned via API
- **Preserves full output**: Complete analysis saved to disk
- **Timestamped files**: Each run creates unique output file

### 2. Category-Based Context Control
Prevent context window overflow by specifying which code categories to include:
- **Focused analysis**: Only relevant files loaded into Gemini's context
- **Configurable categories**: Define custom mappings in `context-categories.json`
- **Elastic context**: Choose categories dynamically per query

### 3. Four Specialized Agents
- `codebase-pattern-finder` - Find similar implementations and code patterns
- `codebase-locator` - Locate files and directories by feature/topic
- `codebase-analyzer` - Analyze implementation details with file:line references
- `web-search-researcher` - Research information from the web

## File Structure

```
.gemini/
├── context-categories.json     # Category-to-path mappings
├── resolve-categories.sh       # Helper script to resolve categories
├── output/                     # Gemini results (timestamped)
├── commands/                   # Gemini CLI custom commands
│   ├── codebase-pattern-finder.toml
│   ├── codebase-locator.toml
│   ├── codebase-analyzer.toml
│   └── web-search-researcher.toml
└── README.md                   # This file

.claude/
├── commands/                   # Claude Code slash commands
│   ├── ask-gemini-patterns.md
│   ├── ask-gemini-locate.md
│   ├── ask-gemini-analyze.md
│   └── ask-gemini-research.md
└── delegate-to-gemini.sh       # Delegation wrapper script
```

## Configuration

### Category Definitions

Edit `.gemini/context-categories.json` to define your categories:

```json
{
  "categories": {
    "api-layer": {
      "description": "API endpoints, routes, and handlers",
      "files": [
        "src/api/**/*.{ts,js}",
        "src/routes/**/*.{ts,js}"
      ]
    },
    "database": {
      "description": "Database models and migrations",
      "files": [
        "src/models/**/*.{ts,js}",
        "src/migrations/**/*.sql"
      ]
    }
  }
}
```

### Available Categories

Default categories (customize as needed):
- `project-context` - **Project-specific patterns and rules** (RECOMMENDED for custom frameworks)
- `opaleye` - Opaleye database layer with extensible records
- `api-layer` - API endpoints, routes, handlers
- `database` - Models, migrations, repositories
- `frontend` - Components, pages, views
- `services` - Business logic and service layer
- `tests` - Test files (unit, integration, e2e)
- `config` - Configuration files
- `types` - TypeScript type definitions
- `docs` - Documentation files
- `haskell-core` - Haskell source files
- `haskell-tests` - Haskell test files

#### Special Category: `project-context`

The `project-context` category includes `PROJECT_CONTEXT.md`, which contains project-specific patterns and rules that differ from standard frameworks. **Always include this category when working with custom code!**

**What goes in PROJECT_CONTEXT.md:**
- Custom ORM patterns (e.g., Opaleye with extensible records)
- Field order sensitivity and constraints
- Framework-specific conventions
- Common mistakes to avoid
- Non-standard architectural patterns

**Example PROJECT_CONTEXT.md content:**
```markdown
## Database Layer: Opaleye with Extensible Records

### Critical Rule: Field Order Sensitivity
Extensible records are ORDER-SENSITIVE. Field order matters:
- `'["id" := Int, "name" := Text]` ≠ `'["name" := Text, "id" := Int]`
- Always maintain consistent field order
- Check existing record definitions before adding new ones
```

## Usage

### From Claude Code

#### Basic Usage (No Category Filtering)
```
/ask-gemini-patterns Find pagination patterns
/ask-gemini-locate Find authentication files
/ask-gemini-analyze How does the webhook handler work?
/ask-gemini-research Latest Next.js 15 features
```

#### With Category Filtering
```
/ask-gemini-patterns CATEGORIES: api-layer,services Find error handling patterns
/ask-gemini-locate CATEGORIES: frontend,types Find user profile components
/ask-gemini-analyze CATEGORIES: database,services Analyze the user repository
```

### From Gemini CLI Directly

```bash
# Without categories
gemini /codebase-pattern-finder "Find pagination patterns"

# With categories
gemini /codebase-pattern-finder "CATEGORIES: api-layer,services | Find error handling patterns"

# With output file
gemini /codebase-pattern-finder "OUTPUT_FILE: result.md | Find patterns"

# With both
gemini /codebase-pattern-finder "OUTPUT_FILE: result.md | CATEGORIES: api-layer | Find patterns"
```

### List Available Categories

```bash
.gemini/resolve-categories.sh
```

## How It Works

### 1. Claude Invokes Gemini
```bash
OUTPUT_FILE=".gemini/output/patterns-1730329800.md"
gemini "/codebase-pattern-finder" "OUTPUT_FILE: $OUTPUT_FILE | CATEGORIES: api-layer | Find error patterns"
```

### 2. Gemini Processes Request
- Parses `OUTPUT_FILE` and `CATEGORIES` parameters
- Loads relevant files based on categories (if specified)
- Performs analysis
- Saves complete results to output file
- Returns: `"Analysis complete. Saved to <path>"`

### 3. Claude Presents Results
- Receives short confirmation message (saves tokens!)
- Reads the output file from disk
- Presents findings to the user

## Benefits

1. **Token Efficiency**: File-based output saves thousands of tokens per delegation
2. **Context Control**: Category filtering prevents context window overflow
3. **Specialization**: Each agent optimized for specific task types
4. **Flexibility**: Works with or without category filtering
5. **Persistence**: All results saved with timestamps for later reference
6. **Auditability**: Complete analysis history in `.gemini/output/`

## Advanced Usage

### Custom Categories

Add project-specific categories to `context-categories.json`:

```json
{
  "categories": {
    "payment-system": {
      "description": "Payment processing components",
      "files": [
        "src/payment/**/*.ts",
        "src/billing/**/*.ts",
        "lib/stripe/**/*.ts"
      ]
    }
  }
}
```

### Multiple Category Resolution

The `resolve-categories.sh` script handles multiple categories:

```bash
.gemini/resolve-categories.sh api-layer database services
# Outputs all file patterns from these three categories
```

### Combining with Gemini's File Inclusion

Gemini CLI supports `@{file}` syntax to include file contents. You can combine this with categories for very specific context:

```bash
gemini /codebase-analyzer "@{src/api/users.ts} | CATEGORIES: database,types | Analyze user creation flow"
```

## Troubleshooting

### Category Not Found
```bash
# List available categories
.gemini/resolve-categories.sh

# Check configuration
cat .gemini/context-categories.json | jq '.categories | keys'
```

### Output File Not Created
- Check Gemini has write permissions to `.gemini/output/`
- Verify Gemini CLI is properly installed
- Check if Gemini successfully parsed the OUTPUT_FILE parameter

### Context Still Too Large
- Use more specific categories
- Split query into multiple smaller delegations
- Create narrower custom categories

## Future Enhancements

- [ ] Automatic category suggestion based on query
- [ ] Category usage statistics
- [ ] Smart context sizing based on remaining token budget
- [ ] Result caching and reuse
- [ ] Multi-agent workflows (chain delegations)
