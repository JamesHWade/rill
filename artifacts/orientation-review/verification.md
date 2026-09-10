# Orientation review verification

The unread total now uses the existing uncapped, Reader-scoped feed aggregates. Memory and PostgreSQL regressions verify 507 unread entries beyond the retrieval limit and verify that another Reader's reading state cannot alter this total.

Selecting a Group, combined Groups, Ungrouped, or a folder clears the old Orientation theme. Theme membership is part of the queue context, so switching themes also invalidates the previous batch context. Server regressions cover all four navigation routes.

Inline Source Evidence retains an exact source substring without generated punctuation. The long-passage regression verifies the 200-character lead against its original text. Browser checks also compare every visible lead with its full evidence disclosure.

Desktop (1440 pixels) and phone (390 pixels) checks show three picks and the correct six-story demo total. Opening the two-story Digests theme and then the R Group restores the full four-story Group queue. Both widths pass axe with no violations, no horizontal overflow, and no browser errors. The review also corrected insufficient contrast in Why now text and the Orientation rating button.

The focused R tests passed 426 assertions, the navigation regression passed 16 assertions, and the deployment manifest test passed 23 assertions after refreshing checksums. The full source suite passed 3,518 assertions with its only failure being the subsequently corrected stale manifest check. See `scripts/browser/orientation-review.mjs` for browser reproduction and `results.json` for results. Package-level and GitHub CI checks are recorded separately on the PR.

The next review pass added the full unread count to memory and PostgreSQL poll fingerprints. Backend regressions keep the evaluated boundary unchanged while reading an older entry, verify that the poll token changes, and verify another Reader's activity cannot change it. Candidate admission also reserves source-text space before fixing the evaluated boundary: unusually large metadata reduces the number of candidates while preserving their provenance fields and the full unread total. A 36-Document metadata stress case stays below 60 KB. All 788 Orientation assertions pass with PostgreSQL enabled.
