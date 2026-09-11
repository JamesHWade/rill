feedback_eval_record <- function(
  key = "one",
  kind = "orientation",
  rating = "helpful",
  reader_id = "reader",
  reasons = character(),
  comment = "",
  model = "fixture-model"
) {
  output <- if (identical(kind, "orientation")) {
    list(
      question = "Which findings deserve a closer look?",
      status = "Current",
      cards = list(list(
        document_id = paste0("document-", key),
        interpretation = "This result merits a larger replication.",
        why_now = "New evidence changes the next experiment.",
        evidence = "The pilot included twelve samples.",
        source = list(title = "A small pilot", site = "Example Journal")
      )),
      themes = list(list(
        name = "Replication",
        note = "Two other studies explore reproducibility.",
        entry_ids = list("entry-a", "entry-b")
      ))
    )
  } else {
    list(
      question = "What are the limitations?",
      response = "The pilot included only twelve samples.",
      response_state = "partial",
      status = "interrupted"
    )
  }
  snapshot <- list(
    reader_id = reader_id,
    kind = kind,
    source_id = paste0("revision-", key),
    run_id = paste0("run-", key),
    output = output,
    provenance = list(
      model = model,
      policy_version = "fixture-v1",
      document_id = paste0("document-", key),
      content_hash = paste0("content-", key)
    )
  )
  list(
    target_id = rill_id("feedback", canonical_json(snapshot)),
    snapshot = snapshot,
    rating = rating,
    reasons = reasons,
    comment = comment,
    created_at = "2026-09-11T12:00:00Z",
    updated_at = "2026-09-11T12:00:00Z"
  )
}

feedback_eval_judgment_fixture <- function(
  samples,
  ratings = samples$human_rating
) {
  data.frame(
    target_id = samples$id,
    output_hash = samples$output_hash,
    rating = ratings,
    judge = rep("synthetic-judge/rubric-v1", nrow(samples)),
    explanation = rep("Synthetic independent judgment.", nrow(samples))
  )
}
