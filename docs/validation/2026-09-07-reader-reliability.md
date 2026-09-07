# Reader reliability and responsive validation

This work follows issues #45, #50, and #51. The results below distinguish
production observations from local fixture checks. The remaining VoiceOver
limitation is documented at the user's request; this is not a conformance claim
and does not close the complete accessibility acceptance work in #50.

## Production polling and source repair

[Run 34073140323](https://github.com/JamesHWade/rill/actions/runs/34073140323)
ran main revision `7ea5201a2540d17f7e48e746f4c31519d74a563e` and recorded
91 successful feeds and 25 failures out of 116. Failure classes were 11 HTTP
403, 11 HTTP 404, one hostname/request failure, one discovery error, and one XML
error. Four previously failing RSS 1.0 sources successfully updated. No
idle-transaction timeout occurred.

Five replacement endpoints from the same publishers were parsed and checked
against production feed IDs and URL collisions. With explicit approval, one
transaction updated those five URLs and cleared their polling validators and
last-poll timestamps. Feed IDs, existing entries, and subscriptions were retained.

[Verification run 34115100892](https://github.com/JamesHWade/rill/actions/runs/34115100892)
checked the five due feeds and all succeeded. Durable outcomes recorded:

| Publisher | New entries |
| --- | ---: |
| Data Imaginist | 20 |
| OpenAI News | 1,173 |
| The Functional Art | 25 |
| Works in Progress | 233 |
| fast.ai | 20 |
| Total | 1,471 |

That targeted run does not prove recovery of the other 20 failed sources.
Subscriptions were not deleted and access denials were not bypassed.

Scheduled article preparation still reported HTTP 403 for all 100 attempts
through the hosted extractor. The branch selects the shipped bundled extractor
with Node.js 22 and reports prepared/failed counts. This workflow change needs
post-merge production verification; it has not been deployed by this work.

## Hosted Reader observation

The authenticated hosted Reader displayed all five repaired feeds. Opening the
latest fast.ai story first displayed its feed copy, then offered a prepared full
article. Loading it showed a stored reading copy prepared by `defuddle-local`.
Ask Rill completed a bounded question about that public article, with the native
Stop generating control visible during generation. The response addressed the
selected source. This verifies an actual hosted reading and model-request path,
not just fixture markup. An existing authenticated session was used; a fresh
credential-entry flow was not tested.

## Product changes

- Queue swipes reveal Read and Save. A named actions button and keyboard
  activation expose the same controls. Saving preserves focus after rerender;
  Escape closes the actions and restores the trigger.
- Reading swipes use existing story navigation. Previous/Next buttons expose
  the same actions and communicate queue boundaries.
- Gestures ignore vertical movement, multiple touches, text selection, edge
  gestures, links, editable controls, code, and tables.
- Selecting the current story can reopen compact Reading. Opening Library from
  an empty queue retains Library instead of immediately returning to the queue.
- The document declares English; the reading surface is a named region;
  sidebar resize handles expose their width; code blocks support keyboard
  focus for horizontal scrolling. Source text retains contrast during refresh.
- Short phone viewports scroll the entire queue or Library so fixed controls
  cannot consume the available content area. Native dialogs restore focus.
- Startup waits for rendered Library content before announcing readiness and
  offers reload recovery after a delay. Access decisions suppress startup text.
- Ask Rill's running and retry output renders even when its empty container was
  hidden. Native Shiny/bslib status, notification, and dialog controls remain.

The gesture layer delegates selection and saving to existing product operations.
It does not introduce a separate navigation framework.

## Browser evidence

Run the isolated demo fixture and checks using
[the browser verification instructions](../../scripts/browser/README.md).
The [audit results](../../artifacts/responsive-audit/results.json) record each
viewport, surface, and accessibility outcome. Fixtures use synthetic data,
including 116 feeds, 150 entries, long titles, and long reading copies.

Coverage includes queue and Reading at 320, 390, 430, 768, 1024, and 1440 CSS
pixels; phone Library; Orientation; feed management; an empty queue; Ask Rill;
dark mode; reduced motion; long content; native errors; an actual closed Shiny
session; offline status; reload recovery; agent running/completed/interrupted
states; pending/denied access; and delayed/interrupted startup.

Chromium touch input exercises the horizontal queue gesture. Additional
input-event checks exercise exclusions and reading navigation. Keyboard checks
cover disclosure, Save focus, Escape, selected-story reopening, and modal
dismissal. Native Chrome at an actual 400% zoom also supported Orientation,
Queue, opening a story with Enter, returning to Queue, opening Library and
Manage feeds, and dismissal. Browser zoom was restored to its initial 90%.

[Before](../../artifacts/responsive-comparison/before/results.json) and
[after](../../artifacts/responsive-comparison/after/results.json) captures compare
15 states at phone and desktop widths. The baseline uses main revision
`7ea5201` with the same synthetic fixture; agent-status markup is copied from
the original inline renderer solely to let that fixture run. Baseline captures
preserve the missing-status behavior. These are fixture comparisons, not
screenshots of private production data.

## Check results

- 44 browser audits passed with zero automated WCAG A/AA violations,
  horizontal overflow, or browser errors.
- The full PostgreSQL-backed source suite passed 2,933 assertions with no
  failures, warnings, or skips.
- R CMD check passed with zero errors, warnings, or notes, including
  PostgreSQL-backed tests.
- Air formatting, pkgdown reference checks, JavaScript syntax, and
  `git diff --check` passed.

## Remaining verification boundaries

- VoiceOver was enabled, but spoken announcements and VoiceOver traversal were
  not reliably observable through the available controls. It was restored to
  its original off state. A complete screen-reader pass remains outstanding;
  the user requested that this limitation be documented and the PR finished.
- Physical iOS/Safari touch behavior has not been tested. Chromium touch and
  native Chrome zoom checks do not certify those platforms.
- Local agent and access error states are synthetic. Hosted success was tested,
  but forced hosted model errors and a fresh Auth0 sign-in were not exercised.
- The scheduled bundled-extractor change requires a post-merge production run.
  The successful hosted interactive extraction is a separate execution path.
