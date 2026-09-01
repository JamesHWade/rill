---
status: accepted
---

# Persist conversations and accepted reading artifacts

Rill keeps two durable product records from agent interaction. A Conversation
is the canonical, reader-visible history provided by shinychat. A Reading
Artifact is an explicitly accepted Carry-forward outcome stored through Graft.
Neither record makes Deputy's runtime transcript, turns, tool calls, or run
state into a separate product artifact.

Conversations may be renamed or permanently deleted. Deleting a Conversation
does not delete a separately accepted Reading Artifact or Reader Memory.
Reading Artifacts use one stable identity with immutable Graft revisions and
retain their acceptance event, Source Evidence anchors, and relevant Deputy run
and trace identifiers. They support Archive and restore. Actionable questions
and follow-ups additionally support open, completed, and cancelled states.

When the Reader has confirmed Logfire as a Data Destination, Rill may export an
OpenTelemetry trace as a non-authoritative diagnostic copy of a Conversation.
The trace contains reader-visible messages and sanitized structural run and tool
spans. It excludes hidden instructions, credentials, raw Documents, tool
arguments and results, and unsanitized exception content. This requires a
narrow Rill-owned export path rather than enabling ellmer's full message-content
capture switch.

The confirmation names Logfire and discloses that Conversation content is sent.
Tracing remains enabled until the Reader opts out; opting out stops all future
Logfire export. Rill does not back up traces or depend on their completeness.
Deleting a Conversation removes Rill's canonical copy immediately but does not
promise deletion before Logfire's disclosed retention period ends.

## Consequences

- Conversation, Deputy run, and OpenTelemetry trace identifiers are correlated.
  Any Reader identifier exported for correlation is pseudonymous and excludes
  names and email addresses.
- Rill exports every enabled Conversation during v1, but accepted Reading
  Artifacts remain meaningful if a trace is sampled, dropped, or unavailable.
- Corrections create new immutable Reading Artifact revisions rather than
  overwriting accepted history.
- Orientation keeps only its current maintained state. Drafts and rejected
  Graft plans disappear after their decision event is recorded in Reading
  History.
- A recommendation or briefing becomes durable only through explicit
  Carry-forward. It does not persist merely because an agent generated it.
- Permanent Forget remains separate from Archive and requires the Graft purge
  capability established as an MVP gate.
