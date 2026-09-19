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
and accepted Carry-forward outcomes. Rill authorizes and maps each write. Graft retains immutable artifacts, exact
selections, and decision streams; later Reading Loops consult an exact accepted
decision only after Rill rechecks current Reader access. ADR 0011 supersedes the
original native graph plan and snapshot integration.

## Consequences

- Rill depends directly on Graft through one Rill-owned knowledge module rather
  than a speculative provider-neutral interface.
- Documents, Reading History, Session Context, Orientation, and unaccepted
  proposals remain in Rill. Deputy owns execution records, and shinychat owns
  reader-visible Conversation presentation and durable history.
- Rill tests against the exact Graft revision recorded in `DESCRIPTION`. A
  reviewed dependency update must pass the real-object PostgreSQL tests.
- The first vertical slice uses Graft artifacts, selections, and decisions in
  PostgreSQL. Generally useful persistence mechanisms belong upstream in Graft.
- Permanent Forget and durable Graft backup and restore are MVP gates. Richer
  receipts, scoped query helpers, and supersession conveniences may follow the
  working integration.
