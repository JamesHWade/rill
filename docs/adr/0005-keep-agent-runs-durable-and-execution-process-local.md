---
status: accepted
---

# Keep Agent Runs durable and execution process-local

Rill owns a PostgreSQL Agent Run ledger independent of any Shiny session, while
Deputy executes asynchronously inside the single web process. A reconnecting
session attaches to the existing Agent Run; process loss marks leased work
interrupted rather than pretending an in-flight provider stream can resume.
This meets the private single-Reader requirement without introducing a worker
service and preserves a stable ledger a later worker could claim.

## Considered options

Session-owned execution was rejected because closing or reconnecting the browser
would lose work and make lifecycle state incoherent. A separate durable worker
was rejected for v1 because the accepted hosting shape uses one web process and
does not yet justify another deployed service.

## Consequences

- V1 admits one active Agent Run per Reader. User Engagement takes priority;
  automatic Orientation waits rather than creating a hidden queue.
- A Run moves from pending to running and then to completed, failed, cancelled,
  or interrupted. Cancelling is a visible intermediate state, and Rill claims
  cancellation only after execution confirms it stopped.
- Rill durably records the request, pinned input identities and policy,
  structural lifecycle events, and latest partial response. Only a completed
  response enters canonical Conversation history.
- An Action Proposal completes its Run. Approval is a separate bounded
  operation, so no Run remains suspended waiting for the Reader.
- Repeating an idempotent start returns the existing Run. Explicit Retry creates
  a linked Run over the same pinned inputs; reevaluation against current Library
  state is a distinct request.
- Every Run has bounded time, model requests, tool calls, context, output, and
  cost. Exact defaults and retention periods remain tunable through
  implementation and v1 acceptance testing.
