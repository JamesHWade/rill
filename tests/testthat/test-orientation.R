testthat::test_that("the memory store keeps one current Orientation per Reader", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader-1"))
  document <- store$memory$documents[[1]]
  evaluated_at <- as.POSIXct("2026-09-02 16:00:00", tz = "UTC")
  orientation <- new_rill_orientation(
    reader_id = "reader-1",
    boundary = list(
      hash = "boundary-1",
      document_ids = document$document_id,
      candidate_count = 1L
    ),
    question = "What deserves attention?",
    introduction = "Start with the clearest source boundary.",
    cards = list(list(
      role = "anchor",
      document_id = document$document_id,
      entry_id = document$entry_id,
      interpretation = "This story establishes the boundary.",
      why_now = "It is the clearest unread account.",
      evidence = "Rill keeps the source feed"
    )),
    agent_run_id = "run-1",
    evaluated_at = evaluated_at
  )
  register_orientation_test_run(store, "reader-1", "run-1")

  saved <- store_save_orientation(store, orientation)
  restored <- store_get_orientation(store, "reader-1")

  testthat::expect_identical(saved, orientation)
  testthat::expect_identical(restored, orientation)
  testthat::expect_null(store_get_orientation(store, "reader-2"))
})

testthat::test_that("an Orientation is bound to its Reader's producing run", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader-1"))
  document <- store$memory$documents[[1L]]
  orientation <- new_rill_orientation(
    reader_id = "reader-1",
    boundary = list(
      hash = "boundary-1",
      document_ids = document$document_id,
      candidate_count = 1L
    ),
    question = "What deserves attention?",
    introduction = "Start here.",
    cards = list(list(
      role = "anchor",
      document_id = document$document_id,
      entry_id = document$entry_id,
      interpretation = "This is the anchor.",
      why_now = "It establishes the boundary.",
      evidence = "Rill keeps the source feed"
    )),
    agent_run_id = "missing-run"
  )

  testthat::expect_error(
    store_save_orientation(store, orientation),
    class = "rill_orientation_invalid"
  )

  register_orientation_test_run(store, "reader-2", "cross-reader-run")
  orientation$agent_run_id <- "cross-reader-run"
  testthat::expect_error(
    store_save_orientation(store, orientation),
    class = "rill_orientation_invalid"
  )

  register_orientation_test_run(store, "reader-1", "wrong-kind-run")
  store$memory$agent_runs[["wrong-kind-run"]]$kind <- "question"
  orientation$agent_run_id <- "wrong-kind-run"
  testthat::expect_error(
    store_save_orientation(store, orientation),
    class = "rill_orientation_invalid"
  )
})

testthat::test_that("Orientation JSON preserves one-card record collections", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)
  candidate <- orientation_candidates(store, reader_id, limit = 1L)[[1L]]
  boundary <- orientation_boundary(list(candidate))
  orientation <- new_rill_orientation(
    reader_id = reader_id,
    boundary = boundary,
    question = "What deserves attention?",
    introduction = "Start here.",
    cards = list(list(
      role = "anchor",
      document_id = candidate$document$document_id,
      entry_id = candidate$entry$entry_id,
      interpretation = "This is the anchor.",
      why_now = "It establishes the boundary.",
      evidence = "Rill keeps the source feed"
    )),
    agent_run_id = "run-1",
    dismissals = list(list(
      card_id = "dismissed-card",
      basis_hash = "dismissed-basis",
      entry_id = "dismissed-entry",
      document_id = "dismissed-document",
      why_now = "It was previously relevant.",
      rationale_hash = "dismissed-rationale",
      dismissed_at = as.POSIXct("2026-09-02 16:00:00", tz = "UTC")
    ))
  )
  row <- data.frame(
    orientation_id = orientation$orientation_id,
    revision_id = orientation$revision_id,
    reader_id = orientation$reader_id,
    evaluation_run_id = orientation$agent_run_id,
    evaluated_at = orientation$evaluated_at,
    updated_at = orientation$updated_at,
    payload = orientation_json(orientation_payload(orientation)),
    stringsAsFactors = FALSE
  )

  restored <- orientation_from_row(row)

  testthat::expect_length(restored$cards, 1L)
  testthat::expect_identical(
    restored$cards[[1L]]$document_id,
    candidate$document$document_id
  )
  testthat::expect_length(restored$boundary$candidates, 1L)
  testthat::expect_identical(
    restored$boundary$candidates[[1L]]$entry_id,
    candidate$entry$entry_id
  )
  testthat::expect_length(restored$dismissals, 1L)
  testthat::expect_s3_class(restored$dismissals[[1L]]$dismissed_at, "POSIXct")
})

testthat::test_that("the Orientation boundary changes only with eligible reading state", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)

  candidates <- orientation_candidates(store, reader_id, limit = 4L)
  first <- orientation_boundary(candidates)
  unchanged <- orientation_boundary(
    orientation_candidates(store, reader_id, limit = 4L)
  )

  testthat::expect_length(candidates, 4L)
  testthat::expect_identical(first, unchanged)
  testthat::expect_identical(first$candidate_count, 4L)
  testthat::expect_length(first$document_ids, 4L)
  testthat::expect_length(first$candidates, 4L)

  store_mark_opened(store, reader_id, candidates[[1]]$entry$entry_id)
  changed <- orientation_boundary(
    orientation_candidates(store, reader_id, limit = 4L)
  )

  testthat::expect_identical(identical(changed$hash, first$hash), FALSE)
  testthat::expect_identical(
    candidates[[1]]$document$document_id %in% changed$document_ids,
    FALSE
  )
})

testthat::test_that("entry copy changes do not move an immutable Document boundary", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)
  candidates <- orientation_candidates(store, reader_id, limit = 4L)
  before <- orientation_boundary(candidates)
  entry_id <- candidates[[1L]]$entry$entry_id
  index <- match(entry_id, store$memory$entries$entry_id)

  store$memory$entries$content_hash[[index]] <- "changed-entry-copy"
  after <- orientation_boundary(
    orientation_candidates(store, reader_id, limit = 4L)
  )

  testthat::expect_identical(after, before)
})

testthat::test_that("missing Documents do not consume the Orientation bound", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)
  newest_entry_id <- store$memory$entries$entry_id[[1L]]
  newest_document_id <- store$memory$document_heads[[newest_entry_id]]
  store$memory$document_heads <- store$memory$document_heads[
    names(store$memory$document_heads) != newest_entry_id
  ]
  store$memory$documents[[newest_document_id]] <- NULL

  candidates <- orientation_candidates(store, reader_id, limit = 2L)

  testthat::expect_length(candidates, 2L)
  testthat::expect_no_match(
    vapply(candidates, \(candidate) candidate$entry$entry_id, character(1)),
    newest_entry_id,
    fixed = TRUE
  )
  testthat::expect_all_true(vapply(
    candidates,
    \(candidate) inherits(candidate$document, "rill_document"),
    logical(1)
  ))
})

testthat::test_that("Orientation prepares bounded feed copies without extraction", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)
  entry_ids <- store$memory$entries$entry_id[1:2]
  document_ids <- unname(store$memory$document_heads[entry_ids])
  store$memory$document_heads <- store$memory$document_heads[
    !names(store$memory$document_heads) %in% entry_ids
  ]
  store$memory$documents[document_ids] <- NULL
  extracted <- FALSE
  testthat::local_mocked_bindings(
    document_from_defuddle = function(...) {
      extracted <<- TRUE
      stop("Orientation must not extract externally.")
    }
  )

  prepared <- prepare_orientation_documents(store, reader_id, limit = 2L)

  testthat::expect_identical(prepared$prepared, 2L)
  testthat::expect_identical(extracted, FALSE)
  documents <- store_list_documents(store, reader_id, entry_ids)
  testthat::expect_length(documents, 2L)
  testthat::expect_all_equal(
    vapply(documents, `[[`, character(1), "acquisition_method"),
    "feed_fallback"
  )
  testthat::expect_all_equal(
    vapply(documents, `[[`, character(1), "producer"),
    "orientation-feed-copy"
  )
})

testthat::test_that("Orientation skips unusable feed copies within its bound", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)
  entry_ids <- store$memory$entries$entry_id[1:2]
  document_ids <- unname(store$memory$document_heads[entry_ids])
  store$memory$document_heads <- store$memory$document_heads[
    !names(store$memory$document_heads) %in% entry_ids
  ]
  store$memory$documents[document_ids] <- NULL
  store$memory$entries$feed_content[[1L]] <- "<img src='cover.jpg'>"

  prepared <- prepare_orientation_documents(store, reader_id, limit = 1L)

  testthat::expect_identical(prepared$prepared, 1L)
  testthat::expect_null(
    store_get_document(store, reader_id, entry_ids[[1L]])
  )
  testthat::expect_s3_class(
    store_get_document(store, reader_id, entry_ids[[2L]]),
    "rill_document"
  )
})

testthat::test_that("Orientation fallback cannot replace an existing head", {
  store <- rill_store(list(demo_mode = TRUE))
  entry <- as.list(store$memory$entries[1L, , drop = FALSE])
  current <- store_get_document(store, "reader", entry$entry_id)
  fallback <- document_fallback(entry, reason = "orientation-feed-copy")

  saved <- store_save_document_if_missing_head(store, "reader", fallback)

  testthat::expect_identical(saved$created, FALSE)
  testthat::expect_identical(saved$document$document_id, current$document_id)
  testthat::expect_identical(
    store_get_document(store, "reader", entry$entry_id)$document_id,
    current$document_id
  )
})

testthat::test_that("dismissed bases do not consume the candidate bound", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)
  candidates <- orientation_candidates(store, reader_id, limit = 2L)
  first <- candidates[[1L]]
  orientation <- new_rill_orientation(
    reader_id = reader_id,
    boundary = orientation_boundary(candidates),
    question = "What deserves attention?",
    introduction = "Begin here.",
    cards = list(list(
      role = "anchor",
      document_id = first$document$document_id,
      entry_id = first$entry$entry_id,
      interpretation = "This is the first candidate.",
      why_now = "It is currently unread.",
      evidence = "Rill keeps the source feed"
    )),
    agent_run_id = "run-1"
  )
  register_orientation_test_run(store, reader_id, "run-1")
  store_save_orientation(store, orientation)
  store_dismiss_orientation_card(
    store,
    reader_id,
    orientation$cards[[1L]]$card_id,
    orientation$revision_id,
    orientation$cards[[1L]]$rationale_hash
  )

  active <- orientation_candidates(store, reader_id, limit = 2L)

  testthat::expect_length(active, 2L)
  testthat::expect_no_match(
    vapply(active, `[[`, character(1), "basis_hash"),
    first$basis_hash,
    fixed = TRUE
  )
})

testthat::test_that("invalid Orientation cards disappear without discarding valid cards", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)
  candidates <- orientation_candidates(store, reader_id, limit = 4L)
  boundary <- orientation_boundary(candidates)
  cards <- lapply(seq_len(2L), function(index) {
    candidate <- candidates[[index]]
    list(
      role = c("anchor", "contrast")[[index]],
      document_id = candidate$document$document_id,
      entry_id = candidate$entry$entry_id,
      interpretation = paste("Interpretation", index),
      why_now = paste("Why now", index),
      evidence = "Rill keeps the source feed"
    )
  })
  orientation <- new_rill_orientation(
    reader_id = reader_id,
    boundary = boundary,
    question = "What deserves attention?",
    introduction = "Read these together.",
    cards = cards,
    agent_run_id = "run-1"
  )
  register_orientation_test_run(store, reader_id, "run-1")
  store_save_orientation(store, orientation)

  current <- orientation_status(store, reader_id, limit = 4L)

  testthat::expect_identical(current$due, FALSE)
  testthat::expect_length(current$orientation$cards, 2L)

  store_mark_opened(store, reader_id, cards[[1]]$entry_id)
  changed <- orientation_status(store, reader_id, limit = 4L)

  testthat::expect_identical(changed$due, TRUE)
  testthat::expect_identical(
    vapply(changed$orientation$cards, `[[`, character(1), "document_id"),
    cards[[2]]$document_id
  )
  testthat::expect_identical(
    changed$invalid_document_ids,
    cards[[1]]$document_id
  )
  testthat::expect_identical(
    changed$orientation$question,
    "What still deserves attention?"
  )
  testthat::expect_identical(
    changed$orientation$cards[[1L]]$role,
    "anchor"
  )
  testthat::expect_identical(
    changed$orientation$cards[[1L]]$frame,
    "connection"
  )
})

testthat::test_that("active processing names the current model destination", {
  store <- rill_store(list(demo_mode = TRUE))
  orientation <- store_get_orientation(store, "reader")
  store$memory$agent_runs[[orientation$agent_run_id]] <- list(
    run_id = orientation$agent_run_id,
    reader_id = "reader",
    pinned_inputs = list(data_destination = "OpenAI")
  )
  config <- list(
    demo_mode = FALSE,
    orientation_enabled = TRUE,
    agent_model = "anthropic/claude-sonnet-4-5"
  )

  note <- orientation_processing_note(
    store,
    orientation,
    config,
    preparing = TRUE
  )

  testthat::expect_match(note, "Anthropic", fixed = TRUE)
  testthat::expect_no_match(note, "OpenAI", fixed = TRUE)
})

testthat::test_that("Orientation cards require exact evidence from their Document", {
  store <- rill_store(list(demo_mode = TRUE))
  document <- store$memory$documents[[1]]
  orientation <- new_rill_orientation(
    reader_id = "reader-1",
    boundary = list(
      hash = "boundary-1",
      document_ids = document$document_id,
      candidate_count = 1L
    ),
    question = "What deserves attention?",
    introduction = "Start here.",
    cards = list(list(
      role = "anchor",
      document_id = document$document_id,
      entry_id = document$entry_id,
      interpretation = "This story establishes the boundary.",
      why_now = "It is the clearest unread account.",
      evidence = "This passage does not exist in the Document."
    )),
    agent_run_id = "run-1"
  )
  register_orientation_test_run(store, "reader-1", "run-1")

  testthat::expect_error(
    store_save_orientation(store, orientation),
    class = "rill_orientation_invalid"
  )
  testthat::expect_null(store_get_orientation(store, "reader-1"))
})

testthat::test_that("Orientation keeps stable Reader identity across evaluations", {
  first <- new_rill_orientation(
    reader_id = "reader-1",
    boundary = list(hash = "boundary-1"),
    question = "First question",
    introduction = "First introduction",
    cards = list(),
    agent_run_id = "run-1"
  )
  second <- new_rill_orientation(
    reader_id = "reader-1",
    boundary = list(hash = "boundary-2"),
    question = "Second question",
    introduction = "Second introduction",
    cards = list(),
    agent_run_id = "run-2"
  )

  testthat::expect_identical(first$orientation_id, second$orientation_id)
  testthat::expect_identical(
    identical(first$revision_id, second$revision_id),
    FALSE
  )
})

testthat::test_that("the Orientation schema keeps one current aggregate per Reader", {
  schema <- paste(
    readLines(
      rill_package_file("sql", "004_orientations.sql"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  testthat::expect_match(
    schema,
    "CREATE TABLE orientations",
    fixed = TRUE
  )
  testthat::expect_match(schema, "reader_id text PRIMARY KEY", fixed = TRUE)
  testthat::expect_match(schema, "payload jsonb NOT NULL", fixed = TRUE)
  testthat::expect_match(schema, "revision_id text NOT NULL", fixed = TRUE)
  testthat::expect_match(
    schema,
    "evaluation_run_id text NOT NULL UNIQUE",
    fixed = TRUE
  )
  testthat::expect_match(
    schema,
    "FOREIGN KEY (reader_id, evaluation_run_id)",
    fixed = TRUE
  )
  testthat::expect_match(
    schema,
    "REFERENCES agent_runs(reader_id, run_id)",
    fixed = TRUE
  )
  testthat::expect_match(
    schema,
    "agent_runs_reader_run_key UNIQUE (reader_id, run_id)",
    fixed = TRUE
  )
})

testthat::test_that("dismissing an Orientation card suppresses its unchanged basis", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)
  candidates <- orientation_candidates(store, reader_id, limit = 4L)
  boundary <- orientation_boundary(candidates)
  cards <- lapply(seq_len(2L), function(index) {
    candidate <- candidates[[index]]
    list(
      role = c("anchor", "contrast")[[index]],
      document_id = candidate$document$document_id,
      entry_id = candidate$entry$entry_id,
      interpretation = paste("Interpretation", index),
      why_now = paste("Why now", index),
      evidence = "Rill keeps the source feed"
    )
  })
  orientation <- new_rill_orientation(
    reader_id = reader_id,
    boundary = boundary,
    question = "What deserves attention?",
    introduction = "Read these together.",
    cards = cards,
    agent_run_id = "run-1"
  )
  register_orientation_test_run(store, reader_id, "run-1")
  store_save_orientation(store, orientation)
  dismissed_at <- as.POSIXct("2026-09-02 17:00:00", tz = "UTC")

  dismissed <- store_dismiss_orientation_card(
    store,
    reader_id,
    orientation$cards[[1]]$card_id,
    orientation$revision_id,
    orientation$cards[[1]]$rationale_hash,
    dismissed_at = dismissed_at
  )
  current <- orientation_status(store, reader_id, limit = 4L)

  testthat::expect_identical(
    dismissed$basis_hash,
    orientation$cards[[1]]$basis_hash
  )
  testthat::expect_identical(dismissed$why_now, "Why now 1")
  testthat::expect_identical(dismissed$dismissed_at, dismissed_at)
  testthat::expect_identical(current$due, TRUE)
  testthat::expect_identical(
    vapply(current$orientation$cards, `[[`, character(1), "card_id"),
    orientation$cards[[2]]$card_id
  )
})

testthat::test_that("dismissal is pinned to the visible Orientation revision", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)
  candidate <- orientation_candidates(store, reader_id, limit = 1L)[[1L]]
  boundary <- orientation_boundary(list(candidate))
  card <- list(
    role = "anchor",
    document_id = candidate$document$document_id,
    entry_id = candidate$entry$entry_id,
    interpretation = "The earlier interpretation.",
    why_now = "It was the earlier rationale.",
    evidence = "Rill keeps the source feed"
  )
  earlier <- new_rill_orientation(
    reader_id = reader_id,
    boundary = boundary,
    question = "What deserved attention?",
    introduction = "Begin here.",
    cards = list(card),
    agent_run_id = "run-earlier"
  )
  card$interpretation <- "The current interpretation."
  current <- new_rill_orientation(
    reader_id = reader_id,
    boundary = boundary,
    question = "What deserves attention now?",
    introduction = "Begin with the current rationale.",
    cards = list(card),
    agent_run_id = "run-current"
  )
  register_orientation_test_run(store, reader_id, "run-current")
  store_save_orientation(store, current)

  testthat::expect_error(
    store_dismiss_orientation_card(
      store,
      reader_id,
      earlier$cards[[1L]]$card_id,
      earlier$revision_id,
      earlier$cards[[1L]]$rationale_hash
    ),
    class = "rill_orientation_invalid"
  )
  testthat::expect_error(
    store_dismiss_orientation_card(
      store,
      reader_id,
      current$cards[[1L]]$card_id,
      current$revision_id,
      earlier$cards[[1L]]$rationale_hash
    ),
    class = "rill_orientation_invalid"
  )
  testthat::expect_length(
    store_get_orientation(store, reader_id)$dismissals,
    0L
  )
})

testthat::test_that("a newly read card cannot be dismissed from stale UI", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)
  candidate <- orientation_candidates(store, reader_id, limit = 1L)[[1L]]
  orientation <- new_rill_orientation(
    reader_id = reader_id,
    boundary = orientation_boundary(list(candidate)),
    question = "What deserves attention?",
    introduction = "Begin here.",
    cards = list(list(
      role = "anchor",
      document_id = candidate$document$document_id,
      entry_id = candidate$entry$entry_id,
      interpretation = "This is the current candidate.",
      why_now = "It is currently unread.",
      evidence = "Rill keeps the source feed"
    )),
    agent_run_id = "run-1"
  )
  register_orientation_test_run(store, reader_id, "run-1")
  store_save_orientation(store, orientation)
  store_mark_opened(store, reader_id, candidate$entry$entry_id)

  testthat::expect_error(
    store_dismiss_orientation_card(
      store,
      reader_id,
      orientation$cards[[1L]]$card_id,
      orientation$revision_id,
      orientation$cards[[1L]]$rationale_hash
    ),
    class = "rill_orientation_invalid"
  )
  testthat::expect_length(
    store_get_orientation(store, reader_id)$dismissals,
    0L
  )
})

testthat::test_that("a dismissal and its Reading History event are atomic", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)
  candidate <- orientation_candidates(store, reader_id, limit = 1L)[[1L]]
  orientation <- new_rill_orientation(
    reader_id = reader_id,
    boundary = orientation_boundary(list(candidate)),
    question = "What deserves attention?",
    introduction = "Begin here.",
    cards = list(list(
      role = "anchor",
      document_id = candidate$document$document_id,
      entry_id = candidate$entry$entry_id,
      interpretation = "This is the current candidate.",
      why_now = "It is currently unread.",
      evidence = "Rill keeps the source feed"
    )),
    agent_run_id = "run-1"
  )
  register_orientation_test_run(store, reader_id, "run-1")
  store_save_orientation(store, orientation)
  event <- list(
    event_id = "dismissal-event-1",
    reader_id = reader_id,
    entry_id = NULL,
    session_id = "session-1",
    event_type = "orientation_card_dismissed",
    happened_at = Sys.time(),
    surface = "orientation",
    position = 1L,
    payload = list()
  )
  testthat::local_mocked_bindings(
    store_record_event = function(...) {
      cli::cli_abort(
        "history unavailable",
        class = "rill_test_history_unavailable"
      )
    }
  )

  testthat::expect_error(
    store_dismiss_orientation_card(
      store,
      reader_id,
      orientation$cards[[1L]]$card_id,
      orientation$revision_id,
      orientation$cards[[1L]]$rationale_hash,
      event = event
    ),
    class = "rill_test_history_unavailable"
  )
  restored <- store_get_orientation(store, reader_id)
  testthat::expect_length(restored$dismissals, 0L)
  testthat::expect_equal(nrow(store$memory$events), 0L)
})

testthat::test_that("publishing Orientation completes its owned Agent Run", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)
  worker_id <- "worker-1"
  candidates <- orientation_candidates(store, reader_id, limit = 4L)
  boundary <- orientation_boundary(candidates)
  run <- store_start_agent_run(
    store,
    reader_id = reader_id,
    kind = "orientation",
    request_key = paste0("orientation:", boundary$hash),
    pinned_inputs = list(boundary_hash = boundary$hash),
    worker_id = worker_id
  )
  run <- store_claim_agent_run(
    store,
    reader_id = reader_id,
    run_id = run$run_id,
    worker_id = worker_id,
    lease_expires_at = Sys.time() + 120
  )
  document <- candidates[[1]]$document
  orientation <- new_rill_orientation(
    reader_id = reader_id,
    boundary = boundary,
    question = "What deserves attention?",
    introduction = "Start here.",
    cards = list(list(
      role = "anchor",
      document_id = document$document_id,
      entry_id = document$entry_id,
      interpretation = "This story establishes the boundary.",
      why_now = "It is the clearest unread account.",
      evidence = "Rill keeps the source feed"
    )),
    agent_run_id = run$run_id
  )
  finished_at <- as.POSIXct("2026-09-02 18:00:00", tz = "UTC")

  completed <- store_complete_orientation_run(
    store,
    orientation,
    worker_id = worker_id,
    usage = list(requests = 1L),
    terminal_reason = "complete",
    deputy_run_id = "deputy-orientation-1",
    finished_at = finished_at
  )

  testthat::expect_identical(completed$run$status, "completed")
  testthat::expect_identical(completed$run$usage$requests, 1L)
  testthat::expect_identical(completed$orientation$evaluated_at, finished_at)
  testthat::expect_identical(
    store_get_orientation(store, reader_id)$revision_id,
    orientation$revision_id
  )
})

testthat::test_that("a worker cannot publish another worker's Orientation", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)
  candidates <- orientation_candidates(store, reader_id, limit = 4L)
  boundary <- orientation_boundary(candidates)
  run <- store_start_agent_run(
    store,
    reader_id = reader_id,
    kind = "orientation",
    request_key = paste0("orientation:", boundary$hash),
    pinned_inputs = list(boundary_hash = boundary$hash),
    worker_id = "worker-1"
  )
  run <- store_claim_agent_run(
    store,
    reader_id = reader_id,
    run_id = run$run_id,
    worker_id = "worker-1",
    lease_expires_at = Sys.time() + 120
  )
  orientation <- new_rill_orientation(
    reader_id = reader_id,
    boundary = boundary,
    question = "What deserves attention?",
    introduction = "Start here.",
    cards = list(),
    agent_run_id = run$run_id
  )

  rejected <- store_complete_orientation_run(
    store,
    orientation,
    worker_id = "worker-2"
  )

  testthat::expect_null(rejected)
  testthat::expect_null(store_get_orientation(store, reader_id))
  testthat::expect_identical(
    store_get_agent_run(store, reader_id, run$run_id)$status,
    "running"
  )
})

testthat::test_that("publication rechecks its boundary at the store seam", {
  reader_id <- "reader-1"
  store <- local_orientation_backend_store("memory", reader_id)
  worker_id <- "worker-1"
  candidates <- orientation_candidates(store, reader_id, limit = 4L)
  boundary <- orientation_boundary(candidates)
  run <- store_start_agent_run(
    store,
    reader_id = reader_id,
    kind = "orientation",
    request_key = paste0("orientation:", boundary$hash),
    pinned_inputs = list(
      boundary_hash = boundary$hash,
      candidate_limit = 4L
    ),
    worker_id = worker_id
  )
  run <- store_claim_agent_run(
    store,
    reader_id = reader_id,
    run_id = run$run_id,
    worker_id = worker_id,
    lease_expires_at = Sys.time() + 120
  )
  orientation <- new_rill_orientation(
    reader_id = reader_id,
    boundary = boundary,
    question = NULL,
    introduction = NULL,
    cards = list(),
    agent_run_id = run$run_id
  )
  store_mark_opened(store, reader_id, candidates[[1L]]$entry$entry_id)

  rejected <- store_complete_orientation_run(
    store,
    orientation,
    worker_id = worker_id
  )

  testthat::expect_null(rejected)
  testthat::expect_null(store_get_orientation(store, reader_id))
  testthat::expect_identical(
    store_get_agent_run(store, reader_id, run$run_id)$status,
    "running"
  )
})
