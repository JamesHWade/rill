---
status: accepted
---

# Authorize agent outcomes at the Rill boundary

Rill grants authority to a bounded, reader-visible outcome rather than each
underlying tool call. Read-only work and configured maintenance may run without
repeated approval. A clear Reader request authorizes a reversible action;
otherwise the agent presents an inert Action Proposal for acceptance. Computed
bulk changes require a plain-language preview, and permanent deletion always
requires action-specific confirmation.

Rill owns and enforces Approval at every mutation boundary. Deputy permissions
remain a defense-in-depth ceiling, and shinychat elements present proposals,
results, failures, and reversal controls without becoming the source of
authority.

## Consequences

- Agent inspection never becomes Reading History or changes reading state.
- Automatic source maintenance stays within existing Subscriptions, Research
  Scope, and confirmed Data Destinations.
- Every durable agent change produces an Action Receipt. A change is called
  reversible only when Rill can perform an exact inverse without overwriting
  newer Reader intent.
- Accepted Graft knowledge uses Archive or Revise rather than generic Undo.
  Permanent Forget remains a separate destructive workflow.
- Dismissed proposals create no preference or Reader Memory. Changed scope
  invalidates Approval; unchanged retries reuse it.
- V1 begins conservatively. Approval friction may justify loosening specific
  boundaries later without adding a general permission-rules interface now.
