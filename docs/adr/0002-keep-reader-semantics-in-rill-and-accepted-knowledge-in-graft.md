---
status: accepted
---

# Keep reader semantics in Rill and accepted knowledge in Graft

Rill owns the Reading Loop, context and evidence policy, tool authority,
approval meaning, product records, and product-specific presentation. It
composes Deputy for governed execution and shinychat for embedded conversation;
Tempest remains deferred until Rill needs a genuine multi-source research
workflow.

Graft is a required v1 dependency and the authoritative store for Reader Memory
and accepted Carry-forward outcomes. Rill authorizes and maps each write, Graft
plans and commits it, and later Reading Loops consume bounded context from a
pinned Graft snapshot.

## Consequences

- Rill depends directly on Graft through one Rill-owned knowledge module rather
  than a speculative provider-neutral interface.
- Documents, Reading History, Session Context, Orientation, and unaccepted
  proposals remain in Rill. Deputy owns execution records, and shinychat owns
  reader-visible Conversation presentation and durable history.
- Rill and Graft keep their `main` branches compatible through real-object
  integration tests rather than a commit pin.
- A first vertical slice uses Graft's current schema, plan, commit, and snapshot
  interfaces. Generally useful improvements belong upstream in Graft.
- Permanent Forget and durable Graft backup and restore are MVP gates. Richer
  receipts, scoped query helpers, and supersession conveniences may follow the
  working integration.
