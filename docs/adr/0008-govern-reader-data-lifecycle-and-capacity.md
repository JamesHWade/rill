---
status: accepted
---

# Govern Reader data lifecycle and capacity

Rill retains Reader-owned data deliberately, exports it in an inspectable form,
and permanently deletes it through a staged lifecycle. Resource limits begin as
observable capacity signals rather than product restrictions. Shared source
material follows its own retention rules and never becomes Reader-owned merely
because a Reader subscribed to it.

Identity admission requests and identity audit events are installation-owned
operational records. When they can be linked to a Reader, their relevant events
are included in that Reader's export. They do not become Library records.
Unlinked admission-request personal details expire 30 days after their last
operational use.

## Reader lifecycle

The conceptual lifecycle is:

```text
active -> disabled -> deletion pending -> deleted
```

A Reader may also move directly from active to deletion pending. Disabling is
reversible and preserves the Library. It immediately ends authenticated
sessions, revokes capture credentials, stops new Orientation and Agent Run
work, cancels active work, and excludes the Reader's Subscriptions from Feed
polling. It does not silently deactivate or rewrite those Subscriptions. During
the invited beta, only an operator may disable or restore a Reader.

Permanent deletion always requires action-specific confirmation. During the
invited beta, an operator acts on an authenticated Reader request. A later
self-service flow must require recent authentication and explicit
confirmation. Deletion remains pending and reversible for seven days. When the
window ends, Rill begins the permanent purge automatically and completes it in
the live system within 24 hours.

The purge removes the Reader, external identity bindings, capture credentials,
personal admission data, Subscriptions, organization, Entry state, Reading
History, private Capture Entries and Documents, reading-copy selections,
Orientations, Agent Runs, Data Destination settings, deferred questions, and
future Reader-owned Conversations, Reading Artifacts, Reader Memory,
Approvals, and Action Receipts. A private Capture Entry is Reader-owned even
when an implementation stores it in a shared table.

Shared Feeds, Feed Entries, and public Documents survive only according to the
shared-source policy below. Deleting one Reader never removes source material
still required by another Reader.

Rill deletes the associated Auth0 profile and requests deletion from external
Data Destinations when they support it. The deletion result names any external
retention that Rill cannot synchronously erase; it never claims stronger
deletion than a provider can verify.

Backups may retain deleted data for at most 30 days. Until that window closes,
a restricted deletion ledger retains only the identifiers required to reapply
completed purges after restoration. No restored system may serve traffic until
those purges succeed. The ledger is destroyed when the backup window closes.

Rill retains a non-identifying deletion receipt for one year. It contains an
opaque request identifier, timestamps, record counts, destination outcomes,
and completion status. It contains no email, issuer, subject, content, stable
Reader identifier, or reusable identity hash. Other identity audit records are
removed or minimized so they cannot identify the deleted Reader.

## Export

A Reader export is a versioned, checksummed archive containing:

- active and inactive Subscriptions as OPML;
- Reader-owned structured records as JSON Lines;
- private captured Documents as Markdown with acquisition provenance;
- exact shared Documents required to inspect Reading History or Reading
  Artifacts, identified as export dependencies rather than Reader-owned data;
- a manifest describing schemas, omissions, checksums, and Data Destinations.

The archive excludes secrets, credential hashes, and provider tokens. Export
remains available when a Reader reaches a capacity signal. A requested export
must finish or fail visibly before the associated permanent purge begins.

## Shared-source retention

Feed polling stops immediately when no active Reader has an active Subscription
to a Feed. A disabled Reader's Subscription remains intact but does not keep
polling active. Inactive Subscriptions remain restorable for as long as their
Reader exists.

After the last Library reference disappears, Rill retains otherwise unneeded
shared Feed material for 90 days. It may then prune unpinned Feed Entries and
public Documents. Material required by Reading History, source provenance, or
a Reading Artifact remains available for as long as that Reader-owned record
exists. Shared-source cleanup must not cascade into Reader-owned history.

## Capacity policy

The invited beta measures and alerts on these per-Reader soft caps:

- 500 active Subscriptions;
- 5,000 Captures or 2 GiB of private Capture storage;
- 100 Captures and 200 new public extractions per day;
- 50 interactive Agent Runs per day and 20 US dollars of model spend per
  month.

These signals do not reject work while Rill has 50 or fewer active Readers.
Existing per-request safety bounds, Agent Run budgets, authentication checks,
and isolation constraints remain enforced. Disposable heartbeat events may be
rate-limited, but Reader decisions, state changes, reading, export, security
actions, and deletion may not be blocked by a capacity signal.

Crossing 50 active Readers triggers a capacity review. It does not
automatically turn soft caps into hard limits. Enforcement requires an explicit
operator decision, evidence from observed usage, and advance notice to affected
Readers. A hard limit rejects only the new resource and never silently deletes
existing data.

## Self-service admission

Self-service sign-up requires a separate launch decision. Before that decision,
Rill must have:

- passed cross-Reader isolation and recovery verification;
- automated and exercised export and deletion;
- settled account linking and recovery;
- operated usage measurement and abuse alerts;
- proved that backup restoration reapplies completed deletions;
- assigned ownership for privacy terms and Reader support;
- completed the invited beta evidence period.

Crossing a Reader-count threshold does not itself enable self-service.

## Consequences

- Lifecycle, export, cleanup, and capacity code must implement this contract in
  later delivery work. The current `disabled` status and OPML-only export are
  not sufficient.
- Reader removal is an orchestrated purge rather than a database cascade.
- Retention preserves inspectability without treating shared source material as
  private Library data.
- Capacity decisions remain evidence-based during the invited beta while hard
  safety and privacy boundaries continue to apply.
