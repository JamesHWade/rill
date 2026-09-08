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
  testthat::expect_null(unavailable$snapshot$output$response)
  preview <- xml2::read_html(as.character(feedback_output_ui(
    list(cards = list(list(interpretation = "A claim")))
  )))
  testthat::expect_length(
    xml2::xml_find_all(preview, ".//pre | .//p[not(normalize-space())]"),
    0L
  )
  testthat::expect_identical(
    xml2::xml_find_chr(preview, "normalize-space(.//p)"),
    "Rill interpretation: A claim"
  )
  missing_preview <- xml2::read_html(as.character(feedback_output_ui(
    unavailable$snapshot$output
  )))
  testthat::expect_length(xml2::xml_find_all(missing_preview, ".//pre"), 0L)
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


testthat::test_that("empty Orientation selections do not offer or create ratings", {
  store <- local_orientation_backend_store("memory", "reader")
  source <- list(
    reader_id = "reader",
    revision_id = "revision",
    agent_run_id = "missing",
    question = "Hidden question",
    introduction = "Hidden introduction",
    status = "No current selection",
    cards = list(list(document_id = "gone", interpretation = "Hidden claim"))
  )
  html <- as.character(orientation_ui(source, list()))
  testthat::expect_match(html, "No current Orientation selection", fixed = TRUE)
  testthat::expect_length(
    xml2::xml_find_all(xml2::read_html(html), "//*[@id='rate_orientation']"),
    0L
  )
  shiny::testServer(
    function(input, output, session) {
      controller <- reader_feedback_server(
        store,
        "reader",
        shiny::reactiveVal(NULL),
        session
      )
    },
    {
      controller$set_orientation(source, list())
      session$setInputs(rate_orientation = 1)
      testthat::expect_null(controller$pending())
      controller$set_orientation(
        source,
        list(list(document = list(document_id = "gone")))
      )
      session$setInputs(rate_orientation = 2)
      testthat::expect_identical(
        controller$pending()$snapshot$output$question,
        "Hidden question"
      )
    }
  )
})


testthat::test_that("feedback previews label source evidence separately from interpretation", {
  preview <- xml2::read_html(as.character(feedback_output_ui(list(
    introduction = "Generated overview",
    cards = list(list(
      interpretation = "Generated claim",
      why_now = "Generated rationale",
      evidence = "Source words"
    ))
  ))))
  testthat::expect_identical(
    xml2::xml_text(xml2::xml_find_all(preview, ".//strong")),
    c(
      "Rill introduction: ",
      "Rill interpretation: ",
      "Why now (Rill): ",
      "Source evidence"
    )
  )
  testthat::expect_identical(
    xml2::xml_text(xml2::xml_find_all(preview, ".//blockquote")),
    "Source words"
  )
})

testthat::test_that("failed streamed text is available for rating only in its session until saved", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  for (backend in c("memory", "postgres")) {
    store <- local_orientation_backend_store(backend, config$actor_id)
    shiny::testServer(rill_server(config, store), {
      for (status in c("failed", "cancelled", "interrupted")) {
        run <- store_start_agent_run(
          store,
          config$actor_id,
          "question",
          status,
          pinned_inputs = list(question = "Why?"),
          worker_id = session_id
        )
        run <- store_claim_agent_run(
          store,
          config$actor_id,
          run$run_id,
          session_id,
          lease_expires_at = Sys.time() + 60
        )
        record_partial <- record_agent_run_partials(run, Sys.time() + 60)
        record_partial("Initial text")
        record_partial("The last text before failure")
        if (identical(status, "cancelled")) {
          store_request_agent_run_cancel(store, config$actor_id, run$run_id)
        }
        run <- if (identical(status, "interrupted")) {
          store_interrupt_agent_run(
            store,
            config$actor_id,
            run$run_id,
            session_id,
            "test_interrupt"
          )
        } else {
          store_finish_agent_run(
            store,
            config$actor_id,
            run$run_id,
            session_id,
            status
          )
        }
        testthat::expect_null(run$partial_response)
        active_agent_run(run)
        session$setInputs(rate_response = status)
        target <- feedback_controller$pending()
        testthat::expect_identical(
          target$snapshot$output$response_state,
          "partial"
        )
        testthat::expect_identical(
          target$snapshot$output$response,
          "The last text before failure"
        )
        session$setInputs(
          feedback_rating = "not_helpful",
          feedback_save = status
        )
        saved <- store_list_reader_feedback(store, config$actor_id)[[
          target$target_id
        ]]
        testthat::expect_identical(
          saved$snapshot$output$response,
          "The last text before failure"
        )
      }
    })
    latest <- feedback_question_runs(store, config$actor_id)[[1L]]
    shiny::testServer(
      function(input, output, session) {
        controller <- reader_feedback_server(
          store,
          config$actor_id,
          shiny::reactiveVal(latest),
          session
        )
      },
      {
        session$setInputs(rate_response = 1)
        testthat::expect_identical(
          controller$pending()$snapshot$output$response_state,
          "unavailable"
        )
      }
    )
  }
})


testthat::test_that("Orientation ratings freeze each displayed source identity with its excerpt", {
  for (backend in c("memory", "postgres")) {
    store <- local_orientation_backend_store(backend, "reader")
    source <- list(
      reader_id = "reader",
      revision_id = "revision",
      agent_run_id = "missing",
      question = "How do these sources differ?",
      cards = list(
        list(
          document_id = "a",
          interpretation = "Claim A",
          evidence = "Excerpt A"
        ),
        list(
          document_id = "b",
          interpretation = "Claim B",
          evidence = "Excerpt B"
        )
      )
    )
    candidates <- list(
      list(
        document = list(
          document_id = "b",
          source_url = "https://example.org/b"
        ),
        entry = list(
          title = "Source B",
          feed_title = "Site B",
          published_at = "2000-01-02 12:00:00"
        )
      ),
      list(
        document = list(
          document_id = "a",
          title = "Source A",
          site = "Site A",
          canonical_url = "https://example.org/a",
          markdown = "Unquoted private body",
          acquisition_method = "web_extraction",
          producer = "test-extractor",
          captured_at = "2000-01-03 12:00:00",
          content_hash = "hash-a"
        ),
        entry = list(published_at = "2000-01-01 12:00:00")
      )
    )
    shiny::testServer(
      function(input, output, session) {
        controller <- reader_feedback_server(
          store,
          "reader",
          shiny::reactiveVal(NULL),
          session
        )
      },
      {
        controller$set_orientation(source, candidates)
        session$setInputs(rate_orientation = 1)
        frozen <- controller$pending()
        candidates[[2]]$document$title <- "Changed title"
        controller$set_orientation(source, candidates)
        session$setInputs(feedback_rating = "helpful", feedback_save = 1)
        saved <- store_list_reader_feedback(store, "reader")[[frozen$target_id]]
        cards <- saved$snapshot$output$cards
        testthat::expect_identical(
          vapply(cards, function(card) card$source$title, character(1)),
          c("Source A", "Source B")
        )
        testthat::expect_identical(
          vapply(cards, function(card) card$source$site, character(1)),
          c("Site A", "Site B")
        )
        testthat::expect_identical(
          cards[[1]]$source$original_url,
          "https://example.org/a"
        )
        testthat::expect_identical(
          cards[[1]]$source$published_at,
          "2000-01-01 12:00:00"
        )
        testthat::expect_identical(cards[[1]]$source$content_hash, "hash-a")
        testthat::expect_null(cards[[1]]$source$markdown)
        preview <- xml2::read_html(as.character(feedback_output_ui(
          saved$snapshot$output
        )))
        testthat::expect_identical(
          xml2::xml_find_chr(
            preview,
            "string((//dl)[1]/dd[preceding-sibling::dt[1]='Title'])"
          ),
          "Source A"
        )
        testthat::expect_identical(
          xml2::xml_find_chr(
            preview,
            "string((//dl)[2]/dd[preceding-sibling::dt[1]='Title'])"
          ),
          "Source B"
        )
        testthat::expect_identical(
          xml2::xml_text(xml2::xml_find_all(preview, ".//blockquote")),
          c("Excerpt A", "Excerpt B")
        )
      }
    )
  }
})
