---
status: accepted
---

# Separate shared sources from Reader-owned Libraries

Rill stores syndicated source material once while keeping each Reader's
Library, activity, private captures, and reading choices isolated. A Reader is
the internal domain identity. A verified external identity is keyed by its
issuer and subject and resolves to exactly one Reader; provider attributes such
as email do not identify or own Library data.

The ownership and key boundaries are:

- Feeds are shared and retain a stable internal identity. URL aliases absorb
  redirects without replacing that identity.
- Feed Entries are shared and keyed within a Feed by external identifier. The
  same article published in two Feeds remains two Entries with separate
  provenance and Reader state. Future story grouping may relate those Entries
  but may not collapse them.
- A Subscription is a durable Reader-to-Feed membership keyed by Reader and
  Feed. Its folder, displayed title, preferences, active state, and Entry state
  belong to that Reader.
- Public Documents are shared immutable captures. A materially changed source
  creates a new Document even when a publisher reuses an Entry identifier.
- A Capture belongs to one Reader and creates a distinct private logical
  Document, even when another Capture has the same URL or bytes. Physical blob
  deduplication may occur below the logical record without merging ownership,
  existence, or provenance.
- Reading Copy Selection belongs to one Reader and one reading item. Reading
  History, events, Conversations, Reader Memory, Reading Artifacts,
  Orientations, Agent Runs, and their approvals and receipts also belong to the
  Reader who created or authorized them.

## Access and lifecycle

An active Subscription makes every retained Entry in its Feed available to the
Reader. Deactivating the Subscription hides those Entries while preserving the
Reader's organization, state, and history so reactivation restores them. A
guessed Entry identifier does not grant access.

A Reader may also access their own Capture and an exact Document pinned by one
of their historical records. A historical pin remains inspectable after an
unsubscribe, but grants no access to the Feed, other Entries, or later
Documents. Reading History retains stable identifiers and event facts when
source material becomes unavailable; it does not duplicate the source content.

A Capture associates with a Feed Entry only when that Entry is already in the
Reader's Library. Otherwise it remains a standalone private reading item and
never subscribes the Reader implicitly. Capturing selects the private Document
for its owner only.

Without an explicit Reading Copy Selection, a Reader follows the current
public Document. A captured or manually selected Document remains selected when
a newer public Document appears. Explicit deletion previews and atomically
falls back to the newest accessible public Document, or makes the item
unavailable when none exists. Unexpected loss or corruption fails closed and
does not silently change the selection.

When the last active Subscription to a Feed is deactivated, polling stops
immediately. The Feed, its Entries, and their provenance remain as an inactive
orphan until the retention policy decides their disposition. Unsubscribing or
source cleanup never cascades into Reader-owned history.

## Legacy migration

The single-Reader database moves to this model in a brief maintenance cutover
using the immutable, forward-only migration process and a verified backup.
Before a second Reader is admitted, the migration must:

1. Create an explicit legacy Reader and make every existing subscription-kind
   Feed an active Subscription for that Reader.
2. Copy each folder and displayed title into Reader-owned Subscription fields,
   and assign existing Entry state, events, and other global Reader records to
   the legacy Reader.
3. Classify public extraction methods as shared Documents. A browser Capture
   becomes private only when its Reader is attributable.
4. Give only the legacy Reader the existing global reading-copy selection.
   Later Readers derive the public default and never inherit a private head.
5. Preserve existing identifiers, hashes, timestamps, and source provenance,
   then verify record counts, references, and ownership constraints.

Unknown or contradictory provenance aborts the migration. It must not guess an
owner, collapse unexpected actors, or admit another Reader before backfill and
verification succeed.

## Consequences

- Every Reader-owned database record uses `reader_id`, references an existing
  Reader, and rejects cross-Reader relationships at the database boundary.
  Store methods must still authorize reads and writes against that Reader.
- Historical audit identifiers remain after related source cleanup rather than
  cascading away. Access to unavailable content still fails closed.
- A shared Feed and its Entries may outlive all active Subscriptions. This
  favors provenance and restoration over immediate reclamation.
- Retention periods, capacity signals, export behavior, permanent Reader
  deletion, and orphan cleanup follow the dedicated
  [data lifecycle decision](0008-govern-reader-data-lifecycle-and-capacity.md).
