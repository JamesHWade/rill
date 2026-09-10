# Orientation review verification

The unread total now uses the existing uncapped, Reader-scoped feed aggregates. Memory and PostgreSQL regressions verify 507 unread entries beyond the retrieval limit and verify that another Reader's reading state cannot alter this total.

Selecting a Group, combined Groups, Ungrouped, or a folder clears the old Orientation theme. Theme membership is part of the queue context, so switching themes also invalidates the previous batch context. Server regressions cover all four navigation routes.

Inline Source Evidence retains an exact source substring without generated punctuation. The long-passage regression verifies the 200-character lead against its original text. Browser checks also compare every visible lead with its full evidence disclosure.

Desktop (1440 pixels) and phone (390 pixels) checks show three picks and the correct six-story demo total. Opening the two-story Digests theme and then the R Group restores the full four-story Group queue. Both widths pass axe with no violations, no horizontal overflow, and no browser errors. The review also corrected insufficient contrast in Why now text and the Orientation rating button.

The focused R tests passed 426 assertions, the navigation regression passed 16 assertions, and the deployment manifest test passed 23 assertions after refreshing checksums. The full source suite passed 3,518 assertions with its only failure being the subsequently corrected stale manifest check. See `scripts/browser/orientation-review.mjs` for browser reproduction and `results.json` for results. Package-level and GitHub CI checks are recorded separately on the PR.

The next review pass added the full unread count to memory and PostgreSQL poll fingerprints. Backend regressions keep the evaluated boundary unchanged while reading an older entry, verify that the poll token changes, and verify another Reader's activity cannot change it. Candidate admission also reserves source-text space before fixing the evaluated boundary: unusually large metadata reduces the number of candidates while preserving their provenance fields and the full unread total. A 36-Document metadata stress case stays below 60 KB. All 788 Orientation assertions pass with PostgreSQL enabled.

The theme follow-up preserves visible theme names, notes, entry membership, and source provenance in feedback snapshots and previews, including theme-only Orientations. The preview omits legacy introductions and hidden framing questions. Theme-only output now participates in mobile Orientation navigation and exposes an accessible rating button. Memory and PostgreSQL tests verify frozen snapshots after the visible output changes. Maintenance receives prior theme wording and membership inside its existing untrusted editorial-data envelope.

New model submissions enforce the advertised 30-word interpretation, 12-word why-now tag, and six-word theme-name caps, plus an explicit 30-word theme-note cap. Typed rejection permits correction before a single accepted submission. Stored legacy Orientations retain their existing validation contract. The package check passed with zero errors, warnings, or notes; subsequent feedback wording and mobile marker adjustments receive focused tests and the fresh PR CI run.

Final UI and feedback tests pass 469 assertions with PostgreSQL enabled. Desktop and phone browser runs both verify mixed and theme-only ratings, saving and reopening the unchanged preview, three picks, six total unread stories, two-story theme navigation, and restoration of the four-story Group queue. Both widths report zero axe violations and zero browser errors.
