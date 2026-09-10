# Article timeline validation

Issue: #88. Base: `76816b44edf37d2650856343ba17298aff66b0c8`.

The implementation uses Rill's real Shiny Queue and storage interfaces. It follows the approved warm-paper timeline hierarchy: source initials and byline, serif headline, short source excerpt, optional wide image, explicit Save and Mark read actions, teal swipe tray, and an Undo notice. Existing Orientation, Group filters, sorting, Library, Reading, and Ask Rill remain part of the app.

## Visual comparison

The approved illustration supplied the hierarchy and palette. The runnable implementation retains Rill's existing top bar, Orientation return, queue count, and sorting controls. Source initials are used instead of invented publisher logos. Content in these screenshots is fictional browser-fixture material; the river image is generated exclusively for that fixture and is never used for a real feed entry.

Phone, tablet, and desktop screenshots cover 320, 390, 430, 768, and 1440 CSS pixels. The timeline has no horizontal overflow and no automated WCAG A/AA violations in those states. Dark mode, reduced motion, long content, doubled root text size, and a 320 by 225 reflow-equivalent viewport are covered by the timeline and responsive scripts. These checks do not certify screen-reader or browser-zoom conformance.

## Behavior

- Save and Mark read operate without opening the article. Keyboard focus returns to the applicable action after a row refresh.
- Real Chromium touch input verifies a short drag revealing the action and a longer drag committing it. Undo restores unread state. Vertical scrolling does not mark an article read.
- The server acknowledges completed mutations. Failed mutations show recovery text; a failed Undo retains its receipt for retry.
- Undo is conditional on the acknowledged state. Opening the article invalidates that queue read receipt even if repeated opens have the same timestamp. Saving does not prevent Undo.
- Memory and PostgreSQL tests cover idempotent actions, authorization, repeated requests, conditional Undo, image metadata persistence, lightweight queue reads, legacy entry shapes, and schema upgrades.
- Image selection uses publisher metadata or article HTML, rejecting unsafe URLs, obvious tracking/decorative images, and known tiny images. Images are lazy-loaded without a referrer; broken images collapse. Existing persisted entries acquire image metadata on their next feed refresh.

## Boundaries

No merge or deployment is included. Browser checks use isolated local demo sessions, with no production model calls or Reader data. Hosted identity, production WebSocket behavior, and actual publisher-image availability are outside this local validation.

## Final validation results

- Full suite with isolated PostgreSQL: 3,384 passing assertions, zero failures, warnings, or skips.
- R CMD check: zero errors, warnings, or notes.
- `pkgdown::check_pkgdown()`: no problems.
- Responsive browser suite: 46 captured states, zero browser errors and zero automated accessibility violations.
- Timeline suite: five viewport sizes plus dark mode with actual doubled root text; zero browser errors and zero automated accessibility violations.
- Independent standards and spec reviews of `8e20c33`: no remaining actionable findings. The review fixes have regression coverage for legacy entry field ordering, identical-timestamp reopening, and retry after a transient Undo failure.

The final CSS pass makes bottom navigation and sort controls scale with enlarged text. The deployment manifest includes both new R files, the queue JavaScript, and the additive image metadata migration.
