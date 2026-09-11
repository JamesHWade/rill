# Reader responsiveness and gestures

The changes are locally verified against deployed baseline `6beb5be`. They have
not yet been pushed or deployed, so the local improvements below are not claims
about current hosted performance.

## Production diagnosis

Connect Cloud publication 45 showed a successful deployment of `6beb5be` on
September 10, 2026, at 6:17 PM EDT, using R 4.6.0 and one CPU with 4 GB memory.
Two article actions were reproduced in the authenticated Reader and matched to
Logfire traces that evening. No article content or URLs were copied into this
report.

- Opening a story: `reading.first_text_ms = 1292.1`,
  `reading.dom_ready_ms = 974.1`, and `reading.server_flush_ms = 814.6`.
  The visible copy was a full copy; `article.render` took 6.6 ms.
- Next article: trace `75ab9ad9f79272e138437616f7f87ed6`, starting
  `2026-09-11T00:00:08.542863Z`, recorded `reading.first_text_ms = 1360.1`,
  `reading.dom_ready_ms = 1237.6`, `reading.server_flush_ms = 767.0`, and
  `reading.paint_delay_ms = 122.5`. The visible copy was a feed copy.
  `article.selection` took 48.6 ms and `article.render` took 4.5 ms.

The root span duration is not interchangeable with first visible text. Subsequent
production records showed queue rendering around 149–178 ms and library
navigation around 110–134 ms. Local profiling identified broad reactive
invalidation, repeated reading-copy rendering, and queue-card timestamp parsing
as avoidable work on the response path. The full-copy preparation dispatch also
ran before the first response had been flushed.

The existing version had no end-to-end mark-read timing. The new `queue.action`
trace measures request-to-visible feedback, with a `queue.action.persist` child
span and separate server-flush, DOM-ready, and two-frame visibility timings.
Invalid reports are rejected, and outstanding traces have bounded lifetimes.
These traces contain no story content or Reader identifiers.

## Implementation

- Article selection and individual queue actions refresh the affected reader
  state and queue immediately. Library navigation and Orientation state refresh
  after the first response, with a short coalescing delay.
- The selected reading copy is cached without invalidating its own reactive
  calculation. Full-copy acquisition is queued after the initial response,
  retaining the originating trace context.
- Queue-card rendering reuses parsed publication times and unchanged HTML;
  relative time labels still update.
- Queue swipes follow the finger instead of stopping at 108 pixels. Releasing
  past the threshold slides the card away, and surviving rows move into place.
  Short, vertical, cancelled, or multi-touch gestures do not commit an action.
- In an open phone article, left advances and right returns to the queue.
  Horizontal code scrolling, selection, interactive content, and vertical
  reading gestures remain separate. Returning to the queue takes precedence
  over a late article response.
- Success notices expire after eight seconds. Hover, keyboard focus, and a
  hidden page pause the available Undo time; errors remain available for
  recovery. Touch-generated hover does not hold a notice open indefinitely.

Action acknowledgements remain after the queue flush. An early-acknowledgement
experiment exposed stale-row selection at a batch boundary and was removed.
The retained benchmark waits for both the confirmation and the changed row.

## Matched local browser measurements

The original baseline and modified reader ran in separate local R processes,
using the same synthetic fixture of 150 articles and 116 feeds, a 30-card first
batch, native Chrome, and a 1280 × 900 viewport. Browser benchmarks ran
sequentially, with 400 ms between actions. The fixture uses an in-memory store;
it does not model production database or network latency.

| Visible outcome | Baseline median | Updated median | Reduction |
| --- | ---: | ---: | ---: |
| Next article, six actions | 280.1 ms | 163.1 ms | 41.8% |
| First open plus six Next actions | 282.5 ms | 179.7 ms | 36.4% |
| Mark read, seven actions | 229.2 ms | 114.4 ms | 50.1% |

The timing begins at the browser click and ends two animation frames after the
new article appears, or after both the read confirmation and updated row are
present. These small samples demonstrate a local improvement, not a hosted
latency distribution. Raw samples are retained in
`artifacts/reader-swipes/{baseline,improved}-final-latency.json`.

## Verification

- Full source suite with PostgreSQL: 3,679 assertions passed, no warnings or
  skips.
- Installed-package `R CMD check --no-manual`: zero errors, warnings, or notes.
- `pkgdown::check_pkgdown()`: no problems found.
- Air formatting and changed-file Jarl checks pass. The full Jarl run still has
  39 pre-existing findings, verified against the baseline.
- Native Chrome swipe checks cover finger tracking, short/cancelled gestures,
  article next/back, vertical and code scrolling, a late selection response,
  eight consecutive next/back cycles, notice expiry with focused Undo, and
  rejected/disconnected action recovery. The reading copy moves 52 pixels for
  a 130-pixel drag; the queue card follows a 180-pixel drag without the old stop.
- Existing queue review checks pass for batch boundaries, keyboard focus, Undo,
  native next swipes, unavailable crypto, and acknowledgement timeouts.
- Responsive timeline checks pass at 320, 390, 430, 768, and 1440 pixels, including
  dark mode, doubled text, reduced motion, and zero axe WCAG A/AA violations.

Reproduction commands are documented in `scripts/browser/README.md`. Screenshots
and browser results are in `artifacts/reader-swipes/`.

## PR review regression

The September 11 PR review identified an unavailable-navigation edge case:
reopening an answer from an unsubscribed Feed leaves no selected queue card.
Although a left drag showed **End of queue**, release selected the first queue
story. A new native-touch regression reproduced that selection request. Swipe
release now requires the gesture's available state and an enabled Next button;
the regression checks that the recovered answer stays selected and a right swipe
still returns to the queue.

## Hosted verification still required

After publication, verify the deployed SHA and repeat article opening, Next,
mark-read, and Undo in the real Reader. Pair visible behavior with
`reading.first_text_ms` and `queue_action.visible_ms`, and inspect the child
spans if either remains slow. A successful publication or telemetry ingestion
alone does not close this check.
