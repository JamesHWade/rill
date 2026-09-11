# Reader Feedback vitals verification

Verified on September 11, 2026 with R 4.6.1, vitals 0.3.0, and ellmer
0.4.2.9000.

## Delivered behavior

- `reader_feedback_samples()` prepares current ratings from one Reader for
  analysis, preserving output identities, reasons, comments, and historical
  provenance.
- `evaluate_reader_feedback()` creates an offline vitals Task with human
  helpfulness scores, or agreement scores for independently supplied judgments.
- Revisions and withdrawal are reflected when rebuilding from current feedback.
  Existing exported copies remain the owner's responsibility.
- Changed outputs, mixed Readers, ambiguous duplicate targets, and mismatched
  independent judgments are rejected. Repeated epochs cannot multiply a human
  observation.
- Full metadata survives vitals logging. Distinct feedback snapshots receive
  distinct log names, including when saved within the same second.

## Verification results

- Focused evaluation tests: 76 passing assertions, no warnings or skips.
- Evaluation, application API, and deployment tests together: 121 passing
  assertions, no warnings or skips.
- R CMD check: zero errors, zero warnings, zero notes. Its installed-package
  tests ran with an isolated PostgreSQL database enabled.
- Air formatting and whitespace checks passed. Jarl passed on all changed R
  files. A repository-wide lint run also reported 39 pre-existing diagnostics
  in untouched code; those are outside this change.
- pkgdown reference indexing and a full site build passed; the new reference
  examples executed during R CMD check and site generation.
- The actual vitals Inspect viewer loaded all three demo evaluations. The
  overview showed helpfulness 0.5 and 1.0 for the separately rated examples and
  calibration agreement 0.667. The disagreement sample exposed its retained
  output, original human label, independent label, and explanation. No browser
  errors were reported. See [overview](viewer-overview.png) and
  [disagreement](viewer-disagreement.png).

The first full source test run identified the expected API-export allowlist and
deployment-checksum updates. Both were corrected, their affected tests passed,
and the subsequent installed-package check passed in full.

## Demonstration and limits

[Report](report.md) and [machine-readable summary](summary.json) were produced by
`scripts/evaluate-reader-feedback.R`. All examples and judge labels are
synthetic. The demonstration imports four human ratings, compares one separately
rated proposed output, and calibrates three independent judgments against four
human ratings. The missing judgment remains unknown.

No real Reader feedback was read, no agent or judge was called, and no change
was deployed. The paired example demonstrates the comparison workflow, not a
measured improvement to live Orientation. Real regression runs require the
original authorized source and execution inputs; source identities alone do not
establish replay availability.

The temporary PostgreSQL server was stopped after validation.
