# Orientation entry and fresh visits

This report records September 10 local validation of the Orientation entry
changes committed as `5753681`, before the branch was pushed.

## Reproduction and cause

`node scripts/browser/orientation-entry.mjs` reproduced a fresh visit opening an
article without a selection: the synthetic fixture held a completed answer
pinned to that article. The initial browser assertion failed with one open
article where none was expected. A matching server regression also failed.

The startup restoration path fetched the latest question, including completed
ones, and selected its pinned Document unconditionally. That restored the old
article on every new session. The completed answer remains retained; selecting
its article is now an explicit action.

Orientation's old queue button was dynamically created and hidden while an
article was selected. Its click handler only changed presentation, so it could
not open Orientation from an article even if the button were exposed.

## Resulting behavior

- The queue and article toolbar have a visible, labeled Orientation button with
  a compass icon and a minimum 44-pixel target.
- Opening Orientation clears the current article selection and refreshes its
  current state while preserving the queue filter. The empty Orientation state
  can also be opened explicitly.
- Fresh sessions retain completed answers without automatically opening their
  articles. The Library offers **Reopen last answer**, which restores the
  existing answer, its pinned reading copy, and the Ask Rill pane. It does not
  run the question again.
- New completed answers update that recovery action. Recovery still validates
  access to the reading copy. Existing unfinished-question recovery remains in
  place, and opening Orientation cannot discard an in-flight response.
- A late destination acknowledgement does not override a newer queue, Library,
  or article selection in the browser.

## Verification

The new browser regression passes at 390 and 1440 pixels: fresh visit, queue to
Orientation, article to Orientation, explicit answer recovery, and reload.
Queue, article, and Orientation views have no axe WCAG A/AA violations or
horizontal overflow. Screenshots and results are in
`artifacts/orientation-entry/`.

The server regression checks selection clearing without changing the queue,
opening an empty Orientation, and explicit recovery of a completed answer even
after its Feed is unsubscribed. The existing answer-rating browser regression
passes at 320, 390, and 1440 pixels using the explicit reopening action, including
saved ratings, export, current-response rating, and enlarged answer text.

The September 10 local source run passed 3,692 assertions; its only failure was
outdated manifest checksums. After refreshing them, the ten-assertion deployment
test passed. The installed-package `R CMD check --no-manual` finished with zero errors,
warnings, or notes. Changed-file Jarl checks, Air formatting, and pkgdown checks
also pass. The responsive timeline regression passes through 320–1440 pixels,
dark mode, and doubled text without accessibility violations or overflow.

Later PR review added per-request IDs to Orientation and saved-answer
acknowledgements. Repeated requests to the same destination can no longer consume
one another's replies. The browser regression holds replies and delivers an old
failure before the newer success for each destination at phone and desktop
widths; server tests check the echoed ID on success and rejection paths.
The September 11 server and deployment test run passed 702 assertions with no
warnings or skips.

A subsequent review regression checks that a later failed or cancelled question,
or an active Orientation, cannot hide an earlier completed answer. Startup now
retrieves the latest completed question independently of active-work recovery.
The store regression covers status ordering and Reader isolation in memory and
PostgreSQL. Server checks preserve an unfinished question when reopening is
blocked; browser checks reopen the earlier answer after each of the other three
states at phone and desktop widths.

These checks establish local behavior. Production verification requires
publishing this revision and repeating the flow in the authenticated Reader.
