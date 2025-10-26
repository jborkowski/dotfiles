Analyze the current conversation and session context, then proactively update the Mem0 knowledge graph with any important learnings, patterns, decisions, or user preferences discovered.

Follow these steps:

1. **Review the current session** - Look at recent conversation context for:
   - New technical patterns or conventions discovered
   - User preferences or workflow decisions
   - Architectural decisions made
   - Important code patterns or best practices
   - Project-specific knowledge
   - Technology stack information

2. **Use memory tools to update** - Based on findings:
   - Create new entities for new concepts: `memory-create_entities`
   - Add observations to existing entities: `memory-add_observations`
   - Create relations between concepts: `memory-create_relations`
   - Remove outdated information: `memory-delete_observations`

3. **Be selective and meaningful** - Only save information that:
   - Is likely to be useful in future sessions
   - Represents actual learning or discovery
   - Clarifies user preferences or project patterns
   - Would help maintain consistency across sessions

4. **Summarize what was saved** - Report back:
   - How many entities were created/updated
   - What key concepts were captured
   - Any relations established

Focus on quality over quantity. Save meaningful knowledge that will help maintain context across sessions.
