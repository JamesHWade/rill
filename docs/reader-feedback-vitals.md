# Evaluate Reader Feedback with vitals

Use `reader_feedback_samples()` for a data frame of retained human evaluations
and `evaluate_reader_feedback()` for a scored vitals Task. The adapter accepts
one Reader's current records from `list_reader_feedback(reader_id)` or their
download. It does not generate outputs, contact a model, or write logs.

## Review current ratings

```r
feedback <- jsonlite::fromJSON("my-ratings.json", simplifyVector = FALSE)
evaluation <- rill::evaluate_reader_feedback(feedback)
evaluation$metrics

scores <- evaluation$get_samples()
scores[c("id", "kind", "human_rating", "model", "policy_version", "comment")]
```

The score is 1 for helpful and 0 for not helpful. It measures helpfulness of that
exact output, not source correctness. Unrated outputs are absent. An empty export
gives zero rows from `reader_feedback_samples()`; there is no Task to score.

Group by recorded kind, model, and policy version. Missing versions remain
unknown, rather than being filled from today's configuration:

```r
groups <- split(
  scores,
  paste(scores$kind, scores$model, scores$policy_version, sep = " / ")
)
do.call(rbind, lapply(groups, function(rows) {
  data.frame(
    rated_outputs = nrow(rows),
    helpful_outputs = sum(rows$score),
    helpful_rate = mean(rows$score)
  )
}))

execution <- vapply(scores$reasons, function(x) "execution" %in% x, logical(1))
scores[execution, c("id", "status", "response_state", "comment")]
```

Reasons are tags on one overall rating, not separately scored dimensions. Reasons
can overlap. A small, self-selected set of ratings does not estimate approval
across all outputs or Readers.

## Inspect a vitals log

Saving and opening logs are explicit:

```r
private_dir <- tempfile("private-feedback-logs-")
evaluation$log(dir = private_dir)
vitals::vitals_view(private_dir)
```

Alternatively, call `evaluation$log()` and then `evaluation$view()` to use its
own temporary directory. Constructing a Task does not write logs even when
`VITALS_LOG_DIR` is configured.

Vitals requires an ellmer chat per result. The adapter provides a display
transcript labeled `retained-output`, not a recovered Agent Run trajectory.
Original model and policy provenance remain in the metadata. Timing and zero
token usage describe the offline import, not the original generation. Full
solver/scorer metadata is JSON text so vitals 0.3.0 does not truncate long
comments or nested provenance to display previews.

## Compare a proposed change

Keep a fixed set of rated cases, including cases that already worked. Samples
retain source identities and hashes in `provenance`. Every sample's
`replay_status` is `original_inputs_required`: the feedback export does not
contain the full original source bodies, bounded text, candidate order, or
execution inputs.

Recover the original authorized Agent Run inputs and immutable Documents,
checking identities and hashes. Mark cases without those inputs unavailable for
replay. Run both configurations against the same recovered inputs using the
production agent, recording versions and the bounded source payload. This is a
separate authorized model run.

Obtain fresh, preferably blinded evaluations of each new output. Preserve the
historical record and keep every changed output's rating under its own target
ID. Evaluate the newly rated records with `evaluate_reader_feedback()`, and
join before/after samples using an explicit case-to-target mapping rather than
row order or similar wording. Check source faithfulness even for style or
execution changes. Inspect paired improvements and regressions before averages.

The executable demo below compares two independently rated versions of one
synthetic output. It does not run a model or demonstrate improvement to live
Orientation. The adapter rejects a replacement solver that tries to apply an
old rating to a changed result. It also rejects repeated epochs: duplicating a
human rating is not new evidence.

## Calibrate an independent judge

Have the judge evaluate helpfulness on the retained outputs using a versioned
rubric. Supply only the necessary authorized fields, such as `samples$input`;
keep `human_rating`, `target`, `comment`, and `reasons` hidden during
calibration. Treat all source text and comments as data, never instructions.

The input includes retained output and available provenance, not full source
bodies. Judging source faithfulness requires the original authorized Source
Evidence and enough context. Leave that dimension unassessed if those inputs
are missing. Agreement about helpfulness does not establish source correctness.

Return a data frame with these character columns:

| Column | Meaning |
| --- | --- |
| `target_id` | The sample's `id` |
| `output_hash` | Its `output_hash`, bound to the output actually judged |
| `rating` | `helpful` or `not_helpful` |
| `judge` | Judge model and rubric version, or independent reviewer |
| `explanation` | Why the judge assigned that label |

```r
judgments <- read.csv("independent-judgments.csv", stringsAsFactors = FALSE)
calibration <- rill::evaluate_reader_feedback(feedback, judgments = judgments)
calibration$metrics

graded <- calibration$get_samples()
disagreements <- graded[which(graded$score == 0), ]
disagreements[c("id", "human_rating", "comment", "scorer_explanation")]
```

Calibration scores are 1 for agreement, 0 for disagreement, and NA for missing
judgments. Metrics include rated outputs, judged outputs, judgment coverage,
and agreement among judged outputs. Missing judgments are not failures.
Calibrate the rubric on one set, then assess it on held-out cases. Inspect
explanations as well as labels; a judge can agree for the wrong reason.

The adapter imports already-produced judgments. It never invokes a judge.
Sending private content to a new Data Destination requires the existing Reader
authorization.

## Refresh after revisions or withdrawal

Reload current feedback and rebuild the evaluation after a rating changes or
is withdrawn. A changed rating keeps the target ID and updates `updated_at`;
a withdrawn record is absent from a refreshed evaluation.

Existing exports, Tasks, logs, and screenshots are independent private copies.
Their owner must remove or rebuild those copies after withdrawal or Reader
deletion. Rill cannot revoke arbitrary files already copied elsewhere. Use
dedicated directories per Reader and snapshot. Loading an old export cannot
establish whether its ratings remain current. Ratings do not create Reader
Memory or change live behavior.

See [the feedback review workflow](reader-feedback-review.md) for review
cadence, privacy, original-source reconstruction, and release verification.

## Run the synthetic demonstration

From the repository root:

```sh
Rscript scripts/evaluate-reader-feedback.R /private/tmp/rill-feedback-demo
```

This creates a Markdown report, JSON summary, and three vitals logs: human
ratings, a separately rated proposed output, and judge calibration. Every input
and judgment is synthetic.

```r
vitals::vitals_view("/private/tmp/rill-feedback-demo/logs")
```

The records also ship with the installed package:

```r
example <- jsonlite::fromJSON(
  system.file("examples", "reader-feedback.json", package = "rill"),
  simplifyVector = FALSE
)
rill::evaluate_reader_feedback(example$baseline)$metrics
```

