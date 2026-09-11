testthat::test_that("feedback samples preserve identity and exclude human labels from judge input", {
  first <- feedback_eval_record(
    reasons = "clarity",
    comment = "Private judgment"
  )
  second <- feedback_eval_record(
    "two",
    kind = "question",
    rating = "not_helpful"
  )
  samples <- reader_feedback_samples(list(second, first))
  testthat::expect_identical(
    samples$id,
    sort(c(first$target_id, second$target_id))
  )
  testthat::expect_identical(samples$target, samples$human_rating)
  testthat::expect_identical(
    samples$replay_status,
    rep("original_inputs_required", 2)
  )
  i <- match(first$target_id, samples$id)
  testthat::expect_identical(samples$reasons[[i]], "clarity")
  testthat::expect_identical(samples$provenance[[i]], first$snapshot$provenance)
  input <- jsonlite::fromJSON(samples$input[[i]], simplifyVector = FALSE)
  testthat::expect_setequal(
    names(input),
    c("target_id", "kind", "output", "provenance")
  )
  testthat::expect_identical(input$target_id, first$target_id)
  testthat::expect_no_match(samples$input[[i]], "Private judgment")
  output <- jsonlite::fromJSON(
    samples$retained_output[[i]],
    simplifyVector = FALSE
  )
  testthat::expect_identical(output$themes[[1]]$name, "Replication")
  testthat::expect_identical(
    output$cards[[1]]$evidence,
    first$snapshot$output$cards[[1]]$evidence
  )
  testthat::expect_identical(nrow(reader_feedback_samples(list())), 0L)
})

testthat::test_that("downloaded JSON and missing provenance remain faithful", {
  record <- feedback_eval_record(reasons = c("clarity", "execution"))
  record$snapshot$provenance <- list()
  record$target_id <- rill_id("feedback", canonical_json(record$snapshot))
  feedback <- stats::setNames(list(record), record$target_id)
  downloaded <- jsonlite::fromJSON(
    orientation_json(feedback),
    simplifyVector = FALSE
  )
  samples <- reader_feedback_samples(downloaded)
  testthat::expect_identical(samples$id, record$target_id)
  testthat::expect_identical(samples$reasons[[1]], c("clarity", "execution"))
  testthat::expect_identical(samples$model, NA_character_)
  testthat::expect_identical(samples$policy_version, NA_character_)
  testthat::expect_length(samples$provenance[[1]], 0L)
})

testthat::test_that("invalid, mixed, duplicated, and reassigned feedback is rejected", {
  one <- feedback_eval_record()
  tampered <- one
  tampered$snapshot$output$question <- "Different output"
  bad_rating <- one
  bad_rating$rating <- NA_character_
  bad_reasons <- one
  bad_reasons$reasons <- list(list("clarity"))
  bad_comment <- one
  bad_comment$comment <- NA_character_
  for (feedback in list(
    "not a list",
    data.frame(),
    list(list()),
    list(tampered),
    list(one, one),
    list(one, feedback_eval_record(reader_id = "another")),
    list(bad_rating),
    list(bad_reasons),
    list(bad_comment)
  )) {
    testthat::expect_error(
      reader_feedback_samples(feedback),
      class = "rill_feedback_invalid"
    )
  }
  testthat::expect_error(
    evaluate_reader_feedback(list()),
    class = "rill_feedback_empty"
  )
})

testthat::test_that("imported feedback requires its retained run identity", {
  for (kind in c("orientation", "question")) {
    for (run_id in list(NULL, "", NA_character_, c("one", "two"), 1)) {
      record <- feedback_eval_record(kind = kind)
      record$snapshot$run_identity <- "A similar key cannot replace run_id"
      record$snapshot$run_id <- run_id
      record$target_id <- rill_id("feedback", canonical_json(record$snapshot))
      testthat::expect_error(
        reader_feedback_samples(list(record)),
        class = "rill_feedback_invalid"
      )
    }
  }
})

testthat::test_that("imported records require exact field names", {
  for (field in c(
    "reader_id",
    "source_id",
    "run_id",
    "kind",
    "output",
    "provenance"
  )) {
    record <- feedback_eval_record()
    names(record$snapshot)[names(record$snapshot) == field] <- paste0(
      field,
      "_other"
    )
    record$target_id <- rill_id("feedback", canonical_json(record$snapshot))
    testthat::expect_error(
      reader_feedback_samples(list(record)),
      class = "rill_feedback_invalid"
    )
  }
  for (field in c(
    "target_id",
    "snapshot",
    "rating",
    "comment",
    "created_at",
    "updated_at"
  )) {
    record <- feedback_eval_record()
    names(record)[names(record) == field] <- paste0(field, "_other")
    testthat::expect_error(
      reader_feedback_samples(list(record)),
      class = "rill_feedback_invalid"
    )
  }
})

testthat::test_that("recomputed identities cannot legitimize malformed retained outputs", {
  for (kind in c("orientation", "question")) {
    record <- feedback_eval_record(kind = kind)
    output <- record$snapshot$output
    patch_output <- function(patch) {
      value <- output
      value[names(patch)] <- patch
      value
    }
    invalid <- list(
      list(),
      data.frame(status = "completed"),
      feedback_eval_record(
        kind = if (kind == "question") "orientation" else "question"
      )$snapshot$output,
      patch_output(list(question = list("Nested question"))),
      patch_output(list(status = NA_character_)),
      patch_output(list(status = c("one", "two")))
    )
    renamed <- output
    names(renamed)[names(renamed) == "status"] <- "statuses"
    invalid[[length(invalid) + 1L]] <- renamed
    if (kind == "orientation") {
      for (cards in list(NULL, "not cards", list("not a card"), list(list()))) {
        invalid[[length(invalid) + 1L]] <- patch_output(list(cards = cards))
      }
      for (field in c("document_id", "interpretation", "why_now", "evidence")) {
        broken <- output
        broken$cards[[1]][[field]] <- NULL
        invalid[[length(invalid) + 1L]] <- broken
      }
      for (themes in list(
        "not themes",
        list(list()),
        list(list(
          name = "Theme",
          note = "Note",
          entry_ids = list(list("entry"))
        ))
      )) {
        invalid[[length(invalid) + 1L]] <- patch_output(list(themes = themes))
      }
      broken <- output
      broken$cards[[1]]$source <- list(title = c("one", "two"))
      invalid[[length(invalid) + 1L]] <- broken
      broken <- output
      broken$themes[[1]]$sources <- list("not a source")
      invalid[[length(invalid) + 1L]] <- broken
      invalid[[length(invalid) + 1L]] <- patch_output(list(introduction = 1))
    } else {
      for (patch in list(
        list(question = NULL),
        list(status = "running"),
        list(response_state = "unknown"),
        list(response_state = NULL),
        list(response = list("Nested answer")),
        list(response = NULL),
        list(response = ""),
        list(response_state = "unavailable", response = "Contradictory text")
      )) {
        invalid[[length(invalid) + 1L]] <- patch_output(patch)
      }
    }
    for (value in invalid) {
      record$snapshot$output <- value
      record$target_id <- rill_id("feedback", canonical_json(record$snapshot))
      testthat::expect_error(
        reader_feedback_samples(list(record)),
        class = "rill_feedback_invalid"
      )
    }
  }
})

testthat::test_that("saved output shapes retain optional historical fields and response states", {
  store <- local_orientation_backend_store("memory", "reader")
  for (state in c("complete", "partial", "unavailable")) {
    run <- list(
      reader_id = "reader",
      kind = "question",
      run_id = state,
      status = if (state == "complete") "completed" else "failed",
      pinned_inputs = list(question = "What are the limitations?")
    )
    if (state == "complete") {
      run$response_text <- "A complete answer."
    }
    if (state == "partial") {
      run$partial_response <- "An unfinished answer."
    }
    target <- feedback_target(store, "reader", "question", run)
    feedback_save(store, "reader", target, "not_helpful")
  }
  samples <- reader_feedback_samples(store_list_reader_feedback(
    store,
    "reader"
  ))
  testthat::expect_setequal(
    samples$response_state,
    c("complete", "partial", "unavailable")
  )

  source <- feedback_eval_record()$snapshot$output
  source$reader_id <- "reader"
  source$revision_id <- "theme-only"
  source$agent_run_id <- "missing-run"
  source$cards <- list()
  source$question <- NULL
  target <- feedback_target(store, "reader", "orientation", source)
  record <- feedback_save(store, "reader", target, "helpful")
  samples <- reader_feedback_samples(list(record))
  testthat::expect_identical(samples$run_id, "missing-run")
  testthat::expect_identical(samples$model, NA_character_)
  testthat::expect_identical(
    samples$retained_output,
    feedback_eval_json(target$snapshot$output)
  )

  record <- feedback_eval_record()
  record$snapshot$output$themes <- NULL
  record$snapshot$output$cards[[1]]$source <- NULL
  record$target_id <- rill_id("feedback", canonical_json(record$snapshot))
  testthat::expect_identical(
    reader_feedback_samples(list(record))$id,
    record$target_id
  )
})

testthat::test_that("current saved ratings produce offline human evaluations", {
  testthat::skip_if_not_installed("vitals", "0.3.0")
  store <- local_orientation_backend_store("memory", "reader")
  record <- feedback_eval_record()
  target <- record[c("target_id", "snapshot")]
  feedback_save(
    store,
    "reader",
    target,
    "helpful",
    "usefulness",
    "A useful pick."
  )
  question <- list(
    reader_id = "reader",
    kind = "question",
    run_id = "attempt",
    status = "failed",
    partial_response = "An unfinished answer",
    pinned_inputs = list(question = "Why?", model = "model-b")
  )
  partial <- feedback_target(store, "reader", "question", question)
  feedback_save(store, "reader", partial, "not_helpful", "execution")
  log_dir <- withr::local_tempdir()
  withr::local_envvar(VITALS_LOG_DIR = log_dir)
  task <- evaluate_reader_feedback(store_list_reader_feedback(store, "reader"))
  testthat::expect_r6_class(task, "Task")
  testthat::expect_equal(
    task$metrics,
    c(helpful_rate = 0.5, rated_outputs = 2, helpful_outputs = 1)
  )
  samples <- task$get_samples()
  testthat::expect_equal(
    samples$score,
    as.numeric(samples$human_rating == "helpful")
  )
  testthat::expect_identical(
    list.files(log_dir, all.files = TRUE, no.. = TRUE),
    character()
  )
  testthat::expect_contains(samples$response_state, "partial")
  testthat::expect_error(
    samples$solver_chat[[1]]$chat("Make a request"),
    class = "rill_feedback_offline"
  )
  metadata <- jsonlite::fromJSON(samples$solver_metadata[[1]])
  testthat::expect_identical(metadata$original_trajectory_available, FALSE)
  testthat::expect_identical(metadata$original_usage_available, FALSE)
})

testthat::test_that("refreshing evaluations reflects revision and withdrawal", {
  testthat::skip_if_not_installed("vitals", "0.3.0")
  store <- local_orientation_backend_store("memory", "reader")
  record <- feedback_eval_record()
  target <- record[c("target_id", "snapshot")]
  feedback_save(store, "reader", target, "not_helpful")
  before <- evaluate_reader_feedback(store_list_reader_feedback(
    store,
    "reader"
  ))
  feedback_save(store, "reader", target, "helpful", "clarity")
  after <- evaluate_reader_feedback(store_list_reader_feedback(store, "reader"))
  testthat::expect_identical(before$get_samples()$id, after$get_samples()$id)
  testthat::expect_identical(before$get_samples()$score, 0)
  testthat::expect_identical(after$get_samples()$score, 1)
  feedback_withdraw(store, "reader", record$target_id)
  current <- store_list_reader_feedback(store, "reader")
  testthat::expect_identical(nrow(reader_feedback_samples(current)), 0L)
  testthat::expect_error(
    evaluate_reader_feedback(current),
    class = "rill_feedback_empty"
  )
})

testthat::test_that("a replacement solver cannot transfer historical scores to changed output", {
  testthat::skip_if_not_installed("vitals", "0.3.0")
  task <- evaluate_reader_feedback(list(feedback_eval_record()))
  chat <- task$get_samples()$solver_chat[[1]]
  task$set_solver(function(inputs) {
    list(result = "A newly generated output", solver_chat = list(chat))
  })
  task$solve()
  testthat::expect_error(task$score(), class = "rill_feedback_output_mismatch")
})

testthat::test_that("identical output text in distinct attempts preserves each sample identity", {
  testthat::skip_if_not_installed("vitals", "0.3.0")
  first <- feedback_eval_record()
  second <- first
  second$snapshot$source_id <- "another-revision"
  second$snapshot$run_id <- "another-run"
  second$target_id <- rill_id("feedback", canonical_json(second$snapshot))
  second$rating <- "not_helpful"
  task <- evaluate_reader_feedback(list(first, second))
  samples <- task$get_samples()
  testthat::expect_equal(length(unique(samples$input)), 2L)
  testthat::expect_identical(
    vapply(
      samples$solver_metadata,
      function(value) jsonlite::fromJSON(value)$id,
      character(1),
      USE.NAMES = FALSE
    ),
    samples$id
  )
  testthat::expect_equal(task$metrics[["helpful_rate"]], 0.5)
  task$solve(epochs = 2)
  testthat::expect_error(task$score(), class = "rill_feedback_invalid")
})

testthat::test_that("calibration joins exact outputs and reports unknown judgments separately", {
  testthat::skip_if_not_installed("vitals", "0.3.0")
  feedback <- list(
    feedback_eval_record(),
    feedback_eval_record("two", rating = "not_helpful"),
    feedback_eval_record("three")
  )
  samples <- reader_feedback_samples(feedback)
  judgments <- feedback_eval_judgment_fixture(samples)
  judgments$rating[[2]] <- if (samples$human_rating[[2]] == "helpful") {
    "not_helpful"
  } else {
    "helpful"
  }
  judgments <- judgments[c(2, 1), ]
  task <- evaluate_reader_feedback(feedback, judgments)
  result <- task$get_samples()
  testthat::expect_equal(result$score, c(1, 0, NA_real_))
  testthat::expect_equal(
    task$metrics,
    c(
      agreement = 0.5,
      judgment_coverage = 2 / 3,
      rated_outputs = 3,
      judged_outputs = 2
    )
  )
  testthat::expect_identical(result$retained_output, result$result)
  testthat::expect_identical(
    jsonlite::fromJSON(result$scorer_metadata[[2]])$human_rating,
    samples$human_rating[[2]]
  )
  testthat::expect_identical(
    jsonlite::fromJSON(result$scorer_metadata[[3]])$judgment_available,
    FALSE
  )
  none <- evaluate_reader_feedback(feedback, judgments[FALSE, ])
  testthat::expect_equal(none$metrics[["judgment_coverage"]], 0)
  testthat::expect_identical(none$metrics[["agreement"]], NA_real_)
})

testthat::test_that("judgments require valid labels, evaluator provenance, and exact output identities", {
  feedback <- list(feedback_eval_record())
  samples <- reader_feedback_samples(feedback)
  judgment <- feedback_eval_judgment_fixture(samples)
  wrong_output <- judgment
  wrong_output$output_hash <- "different"
  wrong_target <- judgment
  wrong_target$target_id <- "unknown"
  for (value in list(wrong_output, wrong_target)) {
    testthat::expect_error(
      evaluate_reader_feedback(feedback, value),
      class = "rill_feedback_output_mismatch"
    )
  }
  wrong_rating <- judgment
  wrong_rating$rating <- "correct"
  missing_judge <- judgment
  missing_judge$judge <- ""
  for (value in list(
    list(),
    judgment["rating"],
    rbind(judgment, judgment),
    wrong_rating,
    missing_judge
  )) {
    testthat::expect_error(
      evaluate_reader_feedback(feedback, value),
      class = "rill_feedback_judgments_invalid"
    )
  }
})

testthat::test_that("vitals logs preserve ratings, source provenance, and retained displays", {
  testthat::skip_if_not_installed("vitals", "0.3.0")
  record <- feedback_eval_record(
    reasons = "source_faithfulness",
    comment = "Inspect the passage."
  )
  missing <- feedback_eval_record(
    "missing",
    kind = "question",
    rating = "not_helpful"
  )
  missing$snapshot$output$response <- NULL
  missing$snapshot$output$response_state <- "unavailable"
  missing$target_id <- rill_id("feedback", canonical_json(missing$snapshot))
  task <- evaluate_reader_feedback(list(record, missing))
  path <- task$log(dir = withr::local_tempdir())
  log <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  testthat::expect_length(log$samples, 2L)
  ids <- vapply(log$samples, `[[`, character(1), "id")
  sample <- log$samples[[match(record$target_id, ids)]]
  testthat::expect_identical(sample$target, "helpful")
  testthat::expect_identical(sample$scores[[1]]$value, 1L)
  testthat::expect_identical(
    sample$scores[[1]]$explanation,
    "Inspect the passage."
  )
  testthat::expect_match(
    sample$output$choices[[1]]$message$content[[1]]$text,
    "Replication",
    fixed = TRUE
  )
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  testthat::expect_match(text, "retained_output_display", fixed = TRUE)
  testthat::expect_match(text, "fixture-model", fixed = TRUE)
  testthat::expect_match(text, "source_faithfulness", fixed = TRUE)
  testthat::expect_no_match(text, "offline-display-only")
  metadata <- jsonlite::fromJSON(
    sample$metadata$solver_metadata,
    simplifyVector = FALSE
  )
  testthat::expect_identical(metadata$provenance$content_hash, "content-one")
  testthat::expect_identical(metadata$reasons, "source_faithfulness")
  task2 <- evaluate_reader_feedback(
    list(record, missing),
    feedback_eval_judgment_fixture(reader_feedback_samples(list(record)))
  )
  testthat::expect_type(task2$log(dir = withr::local_tempdir()), "character")
})

testthat::test_that("logs retain complete comments instead of a truncated metadata preview", {
  testthat::skip_if_not_installed("vitals", "0.3.0")
  comment <- paste(rep("A detailed Reader judgment.", 50), collapse = " ")
  record <- feedback_eval_record(comment = comment)
  task <- evaluate_reader_feedback(list(record))
  log <- jsonlite::fromJSON(
    task$log(dir = withr::local_tempdir()),
    simplifyVector = FALSE
  )
  metadata <- jsonlite::fromJSON(log$samples[[1]]$metadata$solver_metadata)
  testthat::expect_identical(metadata$comment, comment)
  testthat::expect_identical(log$samples[[1]]$scores[[1]]$explanation, comment)
})

testthat::test_that("different feedback revisions receive distinct evaluation log identities", {
  testthat::skip_if_not_installed("vitals", "0.3.0")
  original <- feedback_eval_record()
  revised <- original
  revised$rating <- "not_helpful"
  revised$comment <- "A revised assessment."
  before <- evaluate_reader_feedback(list(original))
  after <- evaluate_reader_feedback(list(revised))
  directory <- withr::local_tempdir()
  paths <- c(before$log(dir = directory), after$log(dir = directory))
  logs <- lapply(paths, jsonlite::fromJSON, simplifyVector = FALSE)
  testthat::expect_equal(length(unique(paths)), 2L)
  testthat::expect_equal(
    length(unique(vapply(logs, function(log) log$eval$task_id, character(1)))),
    2L
  )
  testthat::expect_identical(names(before$metrics)[[1]], "helpful_rate")
  testthat::expect_identical(
    names(logs[[1]]$results$scores[[1]]$metrics)[[1]],
    "helpful_rate"
  )
})
