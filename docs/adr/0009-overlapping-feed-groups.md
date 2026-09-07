---
status: accepted
---

# Organize Subscriptions in overlapping Groups

Readers can place a Subscription in several named Groups, create empty Groups,
and rename or delete a Group without changing its Feeds or reading state.
Ungrouped is a derived view of active Subscriptions without memberships.
Captures stay outside Groups and retain a separate navigation link.

Groups and memberships belong to one Reader. Queries match memberships before
pagination; any selected Group forms a union, while all selected Groups matches
Feeds present in every selected Group. Entries appear once in a queue. Bulk read
actions use the same membership scope and update the existing Reader Entry state.

Migration 013 adds separate Group and membership tables and backfills existing
Subscription folders. The old folder field projects the first Group name in
alphabetical order, or Unsorted. A rollout trigger preserves folder writes from
the preceding release as additive memberships. That release cannot represent
multiple memberships or remove them reliably; Group management requires the new
release. Previously applied migrations remain unchanged.

OPML exports repeat standard feed outlines for each membership and include Rill
metadata in the https://rill.run/opml namespace. The metadata preserves exact
Group names, including slashes and Unsorted, and an empty-Group catalog. Imports
combine repeated feeds and add memberships to existing Subscriptions. Ordinary
nested OPML folders become slash-separated Group names. Other readers may keep
only one outline per Feed or discard the extension, so their round trips cannot
guarantee all memberships or empty Groups.

The OPML import limit counts distinct feed URLs, so repeated membership outlines
do not consume additional Subscription capacity. The file-size limit remains.
