# Run from the repository root. All inputs and judgments are synthetic.
pkgload::load_all(".", quiet = TRUE)
arguments <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(arguments)) {
  arguments[[1L]]
} else {
  tempfile("rill-feedback-demo-")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
log_dir <- file.path(output_dir, "logs")
example <- jsonlite::fromJSON(
  system.file("examples", "reader-feedback.json", package = "rill"),
  simplifyVector = FALSE
)
baseline <- evaluate_reader_feedback(example$baseline)
candidate <- evaluate_reader_feedback(example$candidate)
judgments <- do.call(rbind, lapply(example$judgments, as.data.frame))
calibration <- evaluate_reader_feedback(example$baseline, judgments)
logs <- c(
  baseline = baseline$log(dir = log_dir),
  candidate = candidate$log(dir = log_dir),
  calibration = calibration$log(dir = log_dir)
)
scores <- baseline$get_samples()
groups <- split(
  scores,
  paste(scores$kind, scores$model, scores$policy_version, sep = " / ")
)
by_configuration <- do.call(
  rbind,
  lapply(names(groups), function(name) {
    rows <- groups[[name]]
    data.frame(
      configuration = name,
      rated = nrow(rows),
      helpful = sum(rows$score),
      helpful_rate = mean(rows$score)
    )
  })
)
by_reason <- do.call(
  rbind,
  lapply(sort(unique(unlist(scores$reasons))), function(reason) {
    selected <- vapply(scores$reasons, function(x) reason %in% x, logical(1))
    data.frame(
      reason = reason,
      rated = sum(selected),
      helpful = sum(scores$score[selected])
    )
  })
)
pair <- example$pairs[[1L]]
before <- scores[match(pair$baseline_target_id, scores$id), ]
after_scores <- candidate$get_samples()
after <- after_scores[match(pair$candidate_target_id, after_scores$id), ]
stopifnot(
  before$id != after$id,
  before$output_hash != after$output_hash,
  identical(
    before$provenance[[1]]$document_id,
    after$provenance[[1]]$document_id
  ),
  identical(
    before$provenance[[1]]$content_hash,
    after$provenance[[1]]$content_hash
  )
)
comparison <- data.frame(
  case_id = pair$case_id,
  baseline_target_id = before$id,
  candidate_target_id = after$id,
  original_rating = before$human_rating,
  new_rating = after$human_rating,
  input_status = pair$input_status
)
graded <- calibration$get_samples()
disagreements <- graded[
  which(graded$score == 0),
  c(
    "id",
    "human_rating",
    "comment",
    "scorer_explanation"
  )
]
stopifnot(
  baseline$metrics[["rated_outputs"]] == 4,
  baseline$metrics[["helpful_rate"]] == 0.5,
  calibration$metrics[["judged_outputs"]] == 3,
  calibration$metrics[["agreement"]] == 2 / 3,
  nrow(disagreements) == 1L
)
summary <- list(
  description = example$description,
  baseline = as.list(baseline$metrics),
  calibration = as.list(calibration$metrics),
  by_configuration = by_configuration,
  by_reason = by_reason,
  comparison = comparison,
  disagreements = disagreements,
  logs = as.list(stats::setNames(
    file.path("logs", basename(logs)),
    names(logs)
  ))
)
writeLines(
  jsonlite::toJSON(
    summary,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null",
    digits = NA
  ),
  file.path(output_dir, "summary.json")
)
report <- c(
  "# Reader Feedback in vitals",
  "",
  example$description,
  "",
  "## Human evaluations",
  "",
  "Four retained outputs were reviewed: two helpful and two not helpful.",
  "These are explicit ratings of saved outputs. No agent was rerun.",
  "",
  "| Configuration | Rated | Helpful | Helpful rate |",
  "| --- | ---: | ---: | ---: |",
  apply(by_configuration, 1, function(row) {
    paste0("| ", paste(row, collapse = " | "), " |")
  }),
  "",
  "## Reported problems",
  "",
  "Reasons tag an overall rating and can overlap.",
  "",
  "| Reason | Rated | Helpful |",
  "| --- | ---: | ---: |",
  apply(by_reason, 1, function(row) {
    paste0("| ", paste(row, collapse = " | "), " |")
  }),
  "",
  "## Paired comparison",
  "",
  "The small-pilot example has distinct output IDs and separate synthetic ratings.",
  "The original overstates the pilot result and is rated not helpful. The more cautious",
  "version is independently rated helpful. Both identify the same synthetic source.",
  "This demonstrates the workflow, not a measured improvement to live Orientation.",
  "Original inputs must be recovered and checked before a real replay.",
  "",
  "## Judge calibration",
  "",
  "Three of four examples have independent synthetic judgments. Two agree with the",
  "Reader and one disagrees. Coverage is 75%; agreement among judged examples is 66.7%.",
  "The fourth judgment is missing and remains unknown.",
  "",
  "The disagreement concerns the overconfident pilot interpretation. The calibration",
  "log shows it alongside the human comment and judge explanation.",
  "",
  "## Inspect the outputs",
  "",
  "Run in R:",
  "",
  "```r",
  paste0("vitals::vitals_view(", deparse(log_dir), ")"),
  "```",
  "",
  "Logs retain the exact outputs, provenance, human feedback, and labels identifying",
  "display transcripts. Timing and token usage concern the import. Rebuild from",
  "current feedback after rating revisions or withdrawal."
)
writeLines(report, file.path(output_dir, "report.md"))
cat(normalizePath(file.path(output_dir, "report.md")), "\n")
