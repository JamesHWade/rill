#' Evaluate retained Reader Feedback with vitals
#'
#' Review explicit human ratings of the exact outputs retained by Rill. These
#' functions do not generate outputs, contact a model, read the Library, or write
#' files. They accept one Reader's current feedback records, obtained through
#' [list_reader_feedback()] or their JSON download.
#'
#' @param feedback A list of current feedback records for one Reader. To read a
#'   downloaded file, use `jsonlite::fromJSON(path, simplifyVector = FALSE)`.
#' @param judgments Optional data frame of independently produced helpfulness
#'   judgments. Required character columns are `target_id`, `output_hash`,
#'   `rating` (`"helpful"` or `"not_helpful"`), `judge` (model and rubric version,
#'   or reviewer), and `explanation`. Copy identities from
#'   `reader_feedback_samples()`. Each row must describe that exact retained
#'   output. Omitted judgments remain unknown. Supplying this argument evaluates
#'   agreement with the Reader; it does not invoke a judge.
#'
#' @details
#' `reader_feedback_samples()` prepares a data frame for analysis, with stable
#' target IDs, human ratings, reasons, comments, source provenance, and retained
#' output JSON. `input` contains output and provenance but excludes the human
#' rating, reasons, and comment, so it can be used for independent judging.
#' `target` is the human helpfulness label, not a reference answer. Missing
#' historical provenance stays missing. An empty list produces zero rows.
#'
#' `evaluate_reader_feedback()` requires the optional vitals package. It returns
#' a solved, scored, and measured [vitals::Task]. Human helpfulness scores are
#' numeric: 1 for helpful and 0 for not helpful. They are not correctness scores.
#' With `judgments`, scores instead measure agreement with the human rating;
#' unmatched samples receive `NA`. Metrics include coverage and denominators.
#' A changed result cannot be scored using the original human rating.
#' Repeated epochs are rejected because a rating is one human observation.
#'
#' Vitals requires an ellmer chat for each result. The adapter supplies a clearly
#' labeled display transcript containing the retained output. This is not the
#' original Agent Run trajectory. Its model name is `retained-output`; original
#' model provenance is recorded separately. Any reported timing or token usage
#' describes this offline import, not the original generation.
#'
#' Use the returned task's `$get_samples()` for analysis, `$log(dir = ...)` to
#' explicitly save a private evaluation log, then `vitals::vitals_view(dir)` to
#' inspect that directory. Alternatively, use `$log()` followed by `$view()` to
#' use the task's own temporary log directory. Rebuild tasks after revisions or
#' withdrawals. Previously exported tasks, logs, or files are independent private
#' copies and must be removed or rebuilt by their owner. These functions cannot
#' authenticate a Reader or establish permission to share their data.
#'
#' Feedback retains output snapshots and source identities, not complete replay
#' inputs. Every sample is marked `original_inputs_required` for replay. Recover
#' the authorized original Documents and execution inputs before comparing a new
#' configuration. Preserve the original human rating and obtain a fresh
#' evaluation for every changed output. Do not substitute current Documents for
#' historical source versions.
#'
#' @returns `reader_feedback_samples()` returns a data frame, one row per rated
#'   output. `evaluate_reader_feedback()` returns a vitals Task and requires at
#'   least one rated output.
#' @name evaluate_reader_feedback
#' @export
#' @examples
#' example <- jsonlite::fromJSON(
#'   system.file("examples", "reader-feedback.json", package = "rill"),
#'   simplifyVector = FALSE
#' )
#' samples <- reader_feedback_samples(example$baseline)
#' samples[c("kind", "human_rating", "model")]
#' if (requireNamespace("vitals", quietly = TRUE)) {
#'   evaluation <- evaluate_reader_feedback(example$baseline)
#'   evaluation$metrics
#' }
reader_feedback_samples <- function(feedback) {
  feedback <- feedback_eval_validate(feedback)
  snapshots <- lapply(feedback, `[[`, "snapshot")
  outputs <- lapply(snapshots, `[[`, "output")
  provenance <- lapply(snapshots, `[[`, "provenance")
  field <- function(records, name) {
    vapply(
      records,
      function(record) {
        value <- record[[name]]
        if (store_scalar_string(value)) value else NA_character_
      },
      character(1)
    )
  }
  result <- vapply(outputs, feedback_eval_json, character(1))
  rating <- field(feedback, "rating")
  samples <- data.frame(
    id = field(feedback, "target_id"),
    input = vapply(
      seq_along(snapshots),
      function(i) {
        feedback_eval_json(c(
          list(target_id = feedback[[i]]$target_id),
          snapshots[[i]][c("kind", "output", "provenance")]
        ))
      },
      character(1)
    ),
    target = rating,
    retained_output = result,
    output_hash = vapply(
      result,
      function(output) {
        rill_id("feedback-output", output)
      },
      character(1)
    ),
    human_rating = rating,
    reader_id = field(snapshots, "reader_id"),
    kind = field(snapshots, "kind"),
    source_id = field(snapshots, "source_id"),
    run_id = field(snapshots, "run_id"),
    model = field(provenance, "model"),
    policy_version = field(provenance, "policy_version"),
    response_state = field(outputs, "response_state"),
    status = field(outputs, "status"),
    comment = vapply(feedback, `[[`, character(1), "comment"),
    created_at = field(feedback, "created_at"),
    updated_at = field(feedback, "updated_at"),
    replay_status = rep("original_inputs_required", length(feedback)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  samples$reasons <- unname(lapply(feedback, `[[`, "reasons"))
  samples$provenance <- unname(provenance)
  samples
}

#' @rdname evaluate_reader_feedback
#' @export
evaluate_reader_feedback <- function(feedback, judgments = NULL) {
  samples <- reader_feedback_samples(feedback)
  if (!nrow(samples)) {
    cli::cli_abort(
      "There are no current Reader ratings to evaluate.",
      class = "rill_feedback_empty"
    )
  }
  if (!is.null(judgments)) {
    judgments <- feedback_eval_judgments(judgments, samples)
  }
  if (
    !requireNamespace("vitals", quietly = TRUE) ||
      utils::packageVersion("vitals") < "0.3.0"
  ) {
    cli::cli_abort(
      "Install {.pkg vitals} 0.3.0 or later to evaluate Reader Feedback.",
      class = "rill_feedback_vitals_required"
    )
  }
  retained <- samples
  solver <- function(inputs) {
    index <- match(inputs, retained$input)
    list(
      result = retained$retained_output[index],
      solver_chat = lapply(index, function(i) {
        feedback_eval_chat(retained$input[[i]], retained$retained_output[[i]])
      }),
      solver_metadata = vapply(
        index,
        function(i) {
          feedback_eval_json(c(
            as.list(retained[
              i,
              setdiff(
                names(retained),
                c(
                  "input",
                  "target",
                  "retained_output",
                  "reasons",
                  "provenance"
                )
              ),
              drop = FALSE
            ]),
            list(
              reasons = retained$reasons[[i]],
              provenance = retained$provenance[[i]],
              transcript_origin = "retained_output_display",
              original_trajectory_available = FALSE,
              original_usage_available = FALSE
            )
          ))
        },
        character(1)
      )
    )
  }
  scorer <- function(samples) {
    if (anyDuplicated(samples$id)) {
      cli::cli_abort(
        "Use one epoch: repeated copies of a rating are not new observations.",
        class = "rill_feedback_invalid"
      )
    }
    index <- match(as.character(samples$id), retained$id)
    if (
      anyNA(index) ||
        !identical(unname(samples$result), retained$retained_output[index])
    ) {
      cli::cli_abort(
        "A human rating can only evaluate its original retained output.",
        class = "rill_feedback_output_mismatch"
      )
    }
    human <- retained$human_rating[index]
    if (is.null(judgments)) {
      score <- as.numeric(human == "helpful")
      metadata <- lapply(index, function(i) {
        list(
          evaluated_dimension = "helpfulness",
          evaluator = "reader",
          rating = retained$human_rating[[i]],
          reasons = retained$reasons[[i]],
          comment = retained$comment[[i]],
          updated_at = retained$updated_at[[i]]
        )
      })
      explanation <- retained$comment[index]
    } else {
      matched <- judgments[match(retained$id[index], judgments$target_id), ]
      score <- as.numeric(matched$rating == human)
      metadata <- lapply(seq_along(index), function(i) {
        list(
          evaluated_dimension = "agreement_with_reader_helpfulness",
          human_rating = human[[i]],
          judge_rating = matched$rating[[i]],
          judge = matched$judge[[i]],
          judgment_available = !is.na(matched$rating[[i]])
        )
      })
      explanation <- matched$explanation
      explanation[is.na(explanation)] <- "No independent judgment supplied."
    }
    list(
      score = score,
      scorer_metadata = vapply(metadata, feedback_eval_json, character(1)),
      explanation = explanation
    )
  }
  metrics <- if (is.null(judgments)) {
    list(helpful_rate = mean, rated_outputs = length, helpful_outputs = sum)
  } else {
    list(
      agreement = function(score) {
        if (all(is.na(score))) NA_real_ else mean(score, na.rm = TRUE)
      },
      judgment_coverage = \(score) mean(!is.na(score)),
      rated_outputs = length,
      judged_outputs = \(score) sum(!is.na(score))
    )
  }
  snapshot_id <- rill_id(
    "feedback-eval-v1",
    feedback_eval_json(samples),
    feedback_eval_json(judgments)
  )
  task <- vitals::Task$new(
    dataset = samples,
    solver = solver,
    scorer = scorer,
    metrics = metrics,
    epochs = 1L,
    name = paste0(
      if (is.null(judgments)) {
        "reader-feedback-helpfulness-"
      } else {
        "reader-feedback-calibration-"
      },
      substr(snapshot_id, 1L, 16L)
    ),
    dir = tempfile("rill-feedback-")
  )
  task$solve()
  task$score()
  task$measure()
  task
}

feedback_eval_json <- function(value) {
  as.character(canonical_json(value))
}

feedback_eval_validate <- function(feedback) {
  invalid <- function(message) {
    cli::cli_abort(message, class = "rill_feedback_invalid")
  }
  if (!is.list(feedback) || is.data.frame(feedback)) {
    invalid("{.arg feedback} must be a list of Reader Feedback records.")
  }
  feedback <- lapply(feedback, function(record) {
    if (!is.list(record) || !is.list(record$snapshot)) {
      invalid("Each feedback record must contain its retained snapshot.")
    }
    snapshot <- record$snapshot
    if (
      !store_scalar_string(record$target_id) ||
        !store_scalar_string(snapshot$reader_id) ||
        !store_scalar_string(snapshot$source_id) ||
        !store_scalar_string(snapshot$kind) ||
        !snapshot$kind %in% c("orientation", "question") ||
        !is.list(snapshot$output) ||
        !is.list(snapshot$provenance) ||
        !store_scalar_string(record$rating) ||
        !record$rating %in% c("helpful", "not_helpful")
    ) {
      invalid(
        "Each record needs an output identity, snapshot, and explicit rating."
      )
    }
    if (
      !identical(
        record$target_id,
        rill_id("feedback", canonical_json(snapshot))
      )
    ) {
      invalid("A feedback target does not match its retained output snapshot.")
    }
    reasons <- record$reasons
    if (
      is.list(reasons) && all(vapply(reasons, store_scalar_string, logical(1)))
    ) {
      reasons <- unlist(reasons, use.names = FALSE)
    }
    if (is.null(reasons)) {
      reasons <- character()
    }
    if (
      !is.character(reasons) ||
        anyNA(reasons) ||
        !all(reasons %in% unname(feedback_reasons())) ||
        !is.character(record$comment) ||
        length(record$comment) != 1L ||
        is.na(record$comment) ||
        nchar(record$comment) > 2000L ||
        !store_scalar_string(record$created_at) ||
        !store_scalar_string(record$updated_at)
    ) {
      invalid(
        "Feedback reasons, comment, and timestamps must match saved records."
      )
    }
    record$reasons <- unique(reasons)
    record
  })
  ids <- vapply(feedback, `[[`, character(1), "target_id")
  if (anyDuplicated(ids)) {
    invalid(
      "Supply one current rating per output; duplicate targets are ambiguous."
    )
  }
  readers <- vapply(
    feedback,
    function(record) {
      record$snapshot$reader_id
    },
    character(1)
  )
  if (length(unique(readers)) > 1L) {
    invalid("Evaluate one Reader's private feedback at a time.")
  }
  unname(feedback[order(ids)])
}

feedback_eval_judgments <- function(judgments, samples) {
  required <- c("target_id", "output_hash", "rating", "judge", "explanation")
  if (
    !is.data.frame(judgments) ||
      !all(required %in% names(judgments)) ||
      !all(vapply(judgments[required], is.character, logical(1))) ||
      anyNA(judgments[required]) ||
      anyDuplicated(judgments$target_id) ||
      !all(nzchar(judgments$judge)) ||
      !all(judgments$rating %in% c("helpful", "not_helpful"))
  ) {
    cli::cli_abort(
      "Supply unique judgments with {.field {required}} character columns.",
      class = "rill_feedback_judgments_invalid"
    )
  }
  index <- match(judgments$target_id, samples$id)
  if (
    anyNA(index) || any(judgments$output_hash != samples$output_hash[index])
  ) {
    cli::cli_abort(
      "Each judgment must identify the exact retained output being evaluated.",
      class = "rill_feedback_output_mismatch"
    )
  }
  judgments[order(judgments$target_id), required, drop = FALSE]
}

feedback_eval_chat <- function(input, result) {
  constructing <- TRUE
  chat <- ellmer::chat_openai_compatible(
    base_url = "http://127.0.0.1",
    name = "Rill feedback display",
    model = "retained-output",
    credentials = function() {
      if (!constructing) {
        cli::cli_abort(
          "Retained feedback transcripts cannot make model requests.",
          class = "rill_feedback_offline"
        )
      }
      "offline-display-only"
    },
    system_prompt = paste(
      "Offline display of a retained Rill output for human evaluation.",
      "No model was called. This is not the original Agent Run transcript."
    )
  )
  constructing <- FALSE
  chat$set_turns(list(
    ellmer::UserTurn(list(ellmer::ContentText(input))),
    ellmer::AssistantTurn(list(ellmer::ContentText(result)))
  ))
  chat
}
