# Reader feedback validation

This implements issue #74 with private helpful/not-helpful ratings, optional
reasons and comments, and a retained snapshot of the output presented for review.
The form supports current Orientation, the current finished Ask Rill attempt,
and the last 50 finished attempts. Saved ratings can be revised, withdrawn, or
downloaded. Missing ratings remain unknown. No rating changes live behavior or
becomes Reader Memory.

## Persistence and isolation

The feedback regression suite passed 72 expectations with PostgreSQL enabled,
without failures, warnings, or skipped tests. It covers output replacement,
JSONB field-order normalization, Reader ownership, disabled Readers, revision,
withdrawal, prior attempts, available provenance, partial or missing answers,
server-side freezing, private downloads, and operator connection cleanup.

The full R CMD check, including the PostgreSQL-backed tests, passed with zero
errors, warnings, and notes. Air, Jarl on changed R code, pkgdown reference
checks, JavaScript syntax, and the Git whitespace check also passed.

## Browser evidence

The repeatable [feedback audit](../../scripts/browser/feedback.mjs) uses synthetic
data and real Shiny handlers. It exercises Orientation rating and revision,
earlier-response selection, output preview, saving, Reader-scoped JSON downloads,
withdrawal, and keyboard focus after chained dialogs. The [results](../../artifacts/reader-feedback-audit/results.json)
and screenshots cover 320, 390, and 1440 CSS pixels.

Manual connected-browser checks also exercised the dark-theme rating form,
optional explanations, saved values, withdrawal, Escape, and focus restoration.
The new controls use native Shiny inputs and dialogs. The existing modal focus
handler accepts a stable return target so replacing a picker with a rating form
does not strand keyboard focus on the page body.

## Limits

Browser fixtures do not establish production identity, physical iOS/Safari
behavior, or a complete screen-reader pass. Those remain part of #51 and #24.
Unfinished attempts show retained partial text or explicitly state that no answer
was retained; a saved checkpoint may precede the last streamed token. Historical
provenance fields that were not recorded remain unknown. Feedback and its output
snapshot persist only after Save; routine backup retention is a separate concern.

The [weekly review workflow](../reader-feedback-review.md) defines private review,
reason categories, consent for sharing, and comparison of proposed changes against
rated examples. These checks do not constitute evidence that a model or prompt
change improved outcomes.
