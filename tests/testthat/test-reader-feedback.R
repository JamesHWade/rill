testthat::test_that("feedback freezes outputs and supports private revision and withdrawal", {
  for (backend in c("memory", "postgres")) {
    store <- local_orientation_backend_store(backend, "reader")
    store_ensure_reader(store, "other")
    source <- list(
      reader_id = "reader",
      kind = "question",
      run_id = "attempt-1",
      status = "completed",
      response_text = "The original answer.",
      pinned_inputs = list(
        question = "Why?",
        model = "test-model",
        policy_version = "v1",
        document_id = "doc-1",
        secret = "do-not-retain"
      )
    )
    target <- feedback_target(store, "reader", "question", source)
    testthat::expect_length(store_list_reader_feedback(store, "reader"), 0L)
    feedback_save(
      store,
      "reader",
      target,
      "not_helpful",
      "source_faithfulness",
      "Check the claim."
    )
    source$response_text <- "A different answer."
    replacement <- feedback_target(store, "reader", "question", source)
    testthat::expect_equal(
      identical(target$target_id, replacement$target_id),
      FALSE
    )
    records <- store_list_reader_feedback(store, "reader")
    saved <- records[[target$target_id]]
    testthat::expect_identical(
      saved$snapshot$output$response,
      "The original answer."
    )
    testthat::expect_null(saved$snapshot$provenance$secret)
    testthat::expect_length(store_list_reader_feedback(store, "other"), 0L)
    testthat::expect_error(
      feedback_save(store, "other", target, "helpful"),
      class = "rill_feedback_invalid"
    )
    feedback_withdraw(store, "other", target$target_id)
    testthat::expect_length(store_list_reader_feedback(store, "reader"), 1L)
    feedback_save(store, "reader", saved[c("target_id", "snapshot")], "helpful")
    revised <- store_list_reader_feedback(store, "reader")[[target$target_id]]
    testthat::expect_identical(revised$rating, "helpful")
    testthat::expect_identical(revised$created_at, saved$created_at)
    testthat::expect_identical(revised$snapshot, saved$snapshot)
    feedback_withdraw(store, "reader", target$target_id)
    testthat::expect_length(store_list_reader_feedback(store, "reader"), 0L)
  }
})

testthat::test_that("Orientation feedback survives replacement of the current revision", {
  for (backend in c("memory", "postgres")) {
    store <- local_orientation_backend_store(backend, "reader")
    source <- list(
      reader_id = "reader",
      revision_id = "revision-1",
      agent_run_id = "missing",
      question = "First question",
      introduction = "First interpretation",
      status = "Current",
      cards = list(list(
        document_id = "doc",
        interpretation = "Evidence-bound claim"
      ))
    )
    target <- feedback_target(store, "reader", "orientation", source)
    source$revision_id <- "revision-2"
    source$question <- "Second question"
    feedback_save(store, "reader", target, "helpful")
    saved <- store_list_reader_feedback(store, "reader")[[target$target_id]]
    testthat::expect_identical(saved$snapshot$source_id, "revision-1")
    testthat::expect_identical(saved$snapshot$output$question, "First question")
    testthat::expect_identical(saved$snapshot$run_id, "missing")
    testthat::expect_length(saved$snapshot$provenance, 0L)
    feedback_save(
      store,
      "reader",
      saved[c("target_id", "snapshot")],
      "not_helpful",
      "clarity"
    )
    testthat::expect_length(store_list_reader_feedback(store, "reader"), 1L)
  }
})

testthat::test_that("feedback rejects invalid input and does not preselect approval", {
  store <- local_orientation_backend_store("memory", "reader")
  source <- list(
    reader_id = "reader",
    kind = "question",
    run_id = "run",
    status = "failed",
    response_text = "<script>untrusted</script>",
    pinned_inputs = list()
  )
  target <- feedback_target(store, "reader", "question", source)
  testthat::expect_error(
    feedback_save(store, "reader", target, "helpful", "invented"),
    class = "rill_feedback_invalid"
  )
  testthat::expect_error(
    feedback_save(
      store,
      "reader",
      target,
      "helpful",
      comment = strrep("x", 2001)
    ),
    class = "rill_feedback_invalid"
  )
  testthat::expect_error(
    store_list_reader_feedback(store, "missing"),
    class = "rill_feedback_invalid"
  )
  source$status <- "running"
  testthat::expect_error(
    feedback_target(store, "reader", "question", source),
    class = "rill_feedback_invalid"
  )
  html <- htmltools::renderTags(feedback_dialog(target))$html
  testthat::expect_no_match(html, "<script>untrusted|checked=")
  testthat::expect_match(html, "&lt;script&gt;", fixed = TRUE)
  testthat::expect_match(html, "Slow or failed execution", fixed = TRUE)
  review_html <- htmltools::renderTags(feedback_dialog(
    target,
    return_focus = "review_feedback"
  ))$html
  testthat::expect_match(
    review_html,
    'data-rill-return-focus="review_feedback"',
    fixed = TRUE
  )
})

testthat::test_that("failed attempt feedback distinguishes retained partial text from missing answers", {
  store <- local_orientation_backend_store("memory", "reader")
  source <- list(
    reader_id = "reader",
    kind = "question",
    run_id = "run",
    status = "failed",
    partial_response = "A retained partial",
    pinned_inputs = list(question = "Why?")
  )
  partial <- feedback_target(store, "reader", "question", source)
  testthat::expect_identical(
    partial$snapshot$output$response,
    "A retained partial"
  )
  testthat::expect_identical(partial$snapshot$output$response_state, "partial")
  source$partial_response <- NULL
  unavailable <- feedback_target(store, "reader", "question", source)
  testthat::expect_identical(
    unavailable$snapshot$output$response_state,
    "unavailable"
  )
  testthat::expect_match(
    htmltools::renderTags(feedback_output_ui(unavailable$snapshot$output))$html,
    "No answer text was retained",
    fixed = TRUE
  )
  feedback_save(store, "reader", partial, "not_helpful", "execution")
  testthat::expect_identical(
    store_list_reader_feedback(store, "reader")[[
      partial$target_id
    ]]$snapshot$output$response,
    "A retained partial"
  )
})

testthat::test_that("the server saves the response frozen when its dialog opened", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  shiny::testServer(rill_server(config, store), {
    source <- list(
      reader_id = config$actor_id,
      kind = "question",
      run_id = "run",
      status = "completed",
      response_text = "First answer",
      pinned_inputs = list(question = "Why?")
    )
    active_agent_run(source)
    session$setInputs(rate_response = 1)
    frozen <- feedback_controller$pending()
    source$response_text <- "Changed answer"
    active_agent_run(source)
    session$setInputs(
      feedback_rating = "not_helpful",
      feedback_reasons = "clarity",
      feedback_comment = "Needs explanation",
      feedback_save = 1
    )
    saved <- store_list_reader_feedback(store, config$actor_id)[[
      frozen$target_id
    ]]
    testthat::expect_identical(saved$snapshot$output$response, "First answer")
    exported <- jsonlite::read_json(output$download_feedback)
    testthat::expect_named(exported, frozen$target_id)
    testthat::expect_identical(
      exported[[frozen$target_id]]$snapshot$output$response,
      "First answer"
    )
    testthat::expect_null(feedback_controller$pending())
    session$setInputs(review_feedback = 1, saved_feedback_id = frozen$target_id)
    session$setInputs(edit_feedback = 1)
    session$setInputs(feedback_withdraw = 1)
    testthat::expect_length(
      store_list_reader_feedback(store, config$actor_id),
      0L
    )
  })
})

testthat::test_that("earlier response choices remain Reader scoped and retain available provenance", {
  for (backend in c("memory", "postgres")) {
    store <- local_orientation_backend_store(backend, "reader")
    store_ensure_reader(store, "other")
    now <- Sys.time()
    first <- feedback_test_run(store, "reader", "first", now)
    second <- feedback_test_run(store, "reader", "second", now + 10)
    feedback_test_run(store, "other", "private-other", now + 20)
    runs <- feedback_question_runs(store, "reader")
    testthat::expect_identical(
      unname(vapply(runs, `[[`, character(1), "run_id")),
      c(second$run_id, first$run_id)
    )
    target <- feedback_target(store, "reader", "question", runs[[2]])
    feedback_save(store, "reader", target, "helpful")
    saved <- store_list_reader_feedback(store, "reader")[[target$target_id]]
    testthat::expect_identical(saved$snapshot$output$response, "Answer first")
    testthat::expect_identical(
      saved$snapshot$provenance$document_content_hash,
      "content-hash"
    )
    testthat::expect_identical(
      saved$snapshot$provenance$document_record_hash,
      "record-hash"
    )
    store_disable_reader(store, "reader", "operator", "test suspension")
    testthat::expect_error(
      feedback_question_runs(store, "reader"),
      class = "rill_feedback_invalid"
    )
    testthat::expect_error(
      feedback_save(store, "reader", target, "not_helpful"),
      class = "rill_feedback_invalid"
    )
    testthat::expect_error(
      store_list_reader_feedback(store, "reader"),
      class = "rill_feedback_invalid"
    )
    testthat::expect_error(
      feedback_withdraw(store, "reader", target$target_id),
      class = "rill_feedback_invalid"
    )
  }
})

testthat::test_that("operator feedback review requires a durable store and closes its connection", {
  store <- local_orientation_backend_store("memory", "reader")
  closed <- 0L
  testthat::local_mocked_bindings(
    rill_config = function() list(demo_mode = FALSE),
    rill_store = function(config) store,
    rill_store_close = function(store) {
      closed <<- closed + 1L
    }
  )
  testthat::expect_length(list_reader_feedback("reader"), 0L)
  testthat::expect_identical(closed, 1L)
  testthat::expect_error(
    list_reader_feedback("missing"),
    class = "rill_feedback_invalid"
  )
  testthat::expect_identical(closed, 2L)
  testthat::local_mocked_bindings(rill_config = function() {
    list(demo_mode = TRUE)
  })
  testthat::expect_error(
    list_reader_feedback("reader"),
    class = "rill_feedback_store_required"
  )
  testthat::expect_error(
    list_reader_feedback(character()),
    class = "rill_feedback_invalid"
  )
})
