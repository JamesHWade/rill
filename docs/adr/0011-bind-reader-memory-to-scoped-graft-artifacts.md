---
status: accepted
---

# Bind Reader Memory to scoped Graft artifacts

## Decision

The first accepted-memory slice covers explicit preferences and interpretations
in Ask Rill. The feature is opt-in with `RILL_READER_MEMORY_ENABLED=true` until
permanent Forget and restored-backup admission are implemented. It does not
change Orientation or implement all Carry-forward outcomes.

Rill resolves the Reader from trusted session identity and checks current host
authority on every memory operation. A PostgreSQL transaction checks the active
Reader with a shared row lock, binds the Graft scope `rill:reader:<reader_id>`, and
uses Graft's artifact, selection, decision, and exact reuse APIs. Agent tools
have no Reader identity argument and receive neither database handles nor write
authority. Workers must resolve current authority and bind a new access closure;
serialized handles are not credentials.

Rill retains only a Reader-to-memory catalog and Reading History receipts.
Graft owns immutable content and the decision journal in its scoped object table.
Acceptance, catalog insertion, and the host receipt commit in the same database
transaction. Concurrent operations on a scope serialize through Graft's lock;
expected-head and request-key checks prevent stale changes and duplicate events.
PostgreSQL backup configuration remains an operator responsibility.

A proposal is server-side session state until the Reader reviews and explicitly
accepts it. Editing any field invalidates the pending approval. A preference is
Reader Context without a citation. An interpretation has a retained exact quote,
character offset, Document identity, content hash, record hash, and source label.
Rill verifies current Document access and the exact passage again at acceptance.
The quote anchors the interpretation; it does not prove it.

Correction creates another immutable revision and acceptance event. Archive
records withdrawal, preserving inspection; restore is a new acceptance. Ask Rill
pins the exact accepted decisions when its agent is built and rechecks them on
every tool call. It cannot silently substitute corrected or archived memory.
An already delivered answer is historical output; Archive cannot retract model
context that was already consumed. The dialog lists the latest 100 memories;
exact historical records remain addressable internally.

## Consequences

- Graft contains shared persistence and verification, not Reader product policy.
- Rill cannot equate an artifact reference, scope string, or retained actor label
  with authentication or current access.
- Local demo stores use temporary files and provide no durable or cross-record
  transaction guarantee; the UI discloses their lifetime.
- No permanent Forget or backup admission claim is made by this slice. Archive
  must not be presented as erasure, and the rollout flag stays off by default.
- ADR 0002's native plan/snapshot integration is superseded. No compatibility
  layer or copied Graft serialization is retained in Rill.
