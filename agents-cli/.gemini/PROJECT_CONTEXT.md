# Project-Specific Patterns and Rules

This document contains critical project-specific patterns, rules, and conventions that differ from standard frameworks. AI assistants should pay special attention to these when analyzing or working with this codebase.

---

## Database Layer: Opaleye with Extensible Records

### Critical Rule: Field Order Sensitivity

**Extensible records are ORDER-SENSITIVE**. The order of fields in record types matters:

```haskell
-- These are NOT the same type:
type User1 = Record '["id" := Int, "name" := Text]
type User2 = Record '["name" := Text, "id" := Int]

-- ❌ WRONG - Will cause type errors
user1 :: User1
user2 :: User2 = user1  -- TYPE ERROR!
```

**When working with records:**
- Always maintain consistent field order across the codebase
- Check existing record definitions before adding new ones
- Field order in database queries must match table definition order
- Reordering fields requires updating ALL references

### Opaleye Query Patterns

**Standard patterns to follow:**

```haskell
-- Pattern 1: Query with extensible records
queryUsers :: Query (UserRecord Read)
queryUsers = selectTable userTable

-- Pattern 2: Field projection must preserve order
selectUserFields :: Query (Record '["id" := Column PGInt4, "name" := Column PGText])
selectUserFields = proc () -> do
  user <- queryUsers -< ()
  returnA -< user #! #id #: user #! #name #: RNil  -- Order matters!
```

**Common mistakes to avoid:**
- ❌ Changing field order in SELECT projections
- ❌ Mixing field orders between table definitions and queries
- ❌ Assuming fields can be accessed in any order

### Table Definitions

**Convention for table definitions:**

```haskell
-- Always define fields in the same order as database schema
userTable :: Table
  (Record '["id" := Column PGInt4, "name" := Column PGText, "email" := Column PGText])
  (Record '["id" := Column PGInt4, "name" := Column PGText, "email" := Column PGText])
```

---

## Custom ORM Patterns

### Record Field Access

**Use the `#!` operator for field access:**

```haskell
-- Correct pattern
getName :: UserRecord Read -> Text
getName user = user #! #name

-- When composing multiple field accesses
getFullInfo :: UserRecord Read -> (Int, Text)
getFullInfo user = (user #! #id, user #! #name)
```

---

## Adding New Patterns

When you discover new project-specific patterns or rules:

1. Document them in this file
2. Include working code examples
3. Show common mistakes to avoid
4. Reference files where the pattern is used

---

## Quick Reference

### Always Check These:
- [ ] Field order matches existing definitions
- [ ] Extensible record fields are in correct order
- [ ] Opaleye queries preserve field order
- [ ] Record field access uses `#!` operator
- [ ] New patterns are documented here
