register_orientation_test_run <- function(
  store,
  reader_id,
  run_id,
  status = "completed"
) {
  store$memory$agent_runs[[run_id]] <- list(
    run_id = run_id,
    reader_id = reader_id,
    kind = "orientation",
    request_key = paste0("test-orientation:", run_id),
    retry_of_run_id = NULL,
    status = status,
    pinned_inputs = list(),
    requested_at = as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  )
  invisible(store$memory$agent_runs[[run_id]])
}

orientation_destination_test_config <- function(
  model = "openai/gpt-test",
  enabled = TRUE,
  demo_mode = FALSE,
  base_url = "",
  policy_url = "https://provider.example/privacy"
) {
  list(
    agent_model = model,
    agent_base_url = base_url,
    agent_policy_url = policy_url,
    orientation_enabled = enabled,
    demo_mode = demo_mode
  )
}

confirm_test_orientation_destination <- function(store, config) {
  confirm_orientation_destination(store, config$actor_id, config)
}

orientation_test_destination_check <- function(
  store,
  reader_id,
  model = "openai/gpt-test",
  base_url = ""
) {
  config <- orientation_destination_test_config(
    model = model,
    base_url = base_url
  )
  state <- orientation_destination_state(store, reader_id, config)
  if (!isTRUE(state$enabled)) {
    confirm_orientation_destination(store, reader_id, config)
  }
  \() orientation_destination_state(store, reader_id, config)
}

expect_orientation_policy_consent_contract <- function(store, reader_id) {
  config <- orientation_destination_test_config(
    policy_url = "https://provider.example/privacy"
  )
  confirm_orientation_destination(store, reader_id, config)
  revised_config <- orientation_destination_test_config(
    policy_url = "https://provider.example/revised-privacy"
  )

  revised <- orientation_destination_state(store, reader_id, revised_config)

  testthat::expect_identical(revised$enabled, FALSE)
  testthat::expect_identical(revised$confirmed, FALSE)
  testthat::expect_identical(revised$needs_confirmation, TRUE)
  testthat::expect_error(
    set_orientation_enabled(
      store,
      reader_id,
      enabled = TRUE,
      config = revised_config
    ),
    class = "rill_orientation_confirmation_required"
  )

  reconfirmed <- confirm_orientation_destination(
    store,
    reader_id,
    revised_config
  )
  record <- orientation_destination_record(store, reader_id)
  testthat::expect_identical(reconfirmed$enabled, TRUE)
  testthat::expect_identical(
    record$policy_url,
    "https://provider.example/revised-privacy"
  )

  record$policy_url <- "https://reader:secret@provider.example/privacy"
  testthat::expect_error(
    store_save_orientation_destination(store, record),
    class = "rill_agent_url_invalid"
  )
}

expect_deferred_reader_question_contract <- function(store, reader_id) {
  started_at <- as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  orientation <- store_start_agent_run(
    store,
    reader_id = reader_id,
    kind = "orientation",
    request_key = "orientation-before-question",
    pinned_inputs = list(boundary_hash = "boundary-before-question"),
    requested_at = started_at,
    worker_id = "orientation-worker"
  )
  orientation <- store_claim_agent_run(
    store,
    reader_id = reader_id,
    run_id = orientation$run_id,
    worker_id = "orientation-worker",
    started_at = started_at + 1,
    lease_expires_at = started_at + 120
  )
  pinned_inputs <- list(
    document_id = "document-for-question",
    question = "What changed?"
  )

  waiting <- store_start_prioritized_reader_question(
    store,
    reader_id = reader_id,
    request_key = "question-after-orientation",
    pinned_inputs = pinned_inputs,
    requested_at = started_at + 2,
    worker_id = "question-worker",
    transition_at = started_at + 2
  )
  deferred <- store_get_deferred_reader_question(store, reader_id)

  testthat::expect_null(waiting$run)
  testthat::expect_identical(waiting$preempted$status, "cancelling")
  testthat::expect_identical(
    deferred$request_key,
    "question-after-orientation"
  )
  testthat::expect_identical(
    as.character(canonical_json(deferred$pinned_inputs)),
    as.character(canonical_json(pinned_inputs))
  )
  testthat::expect_error(
    store_save_deferred_reader_question(
      store,
      reader_id,
      request_key = "different-question",
      pinned_inputs = list(question = "Different")
    ),
    class = "rill_agent_run_draining"
  )

  store_finish_agent_run(
    store,
    reader_id = reader_id,
    run_id = orientation$run_id,
    worker_id = "orientation-worker",
    status = "cancelled",
    terminal_reason = "reader_question",
    finished_at = started_at + 3
  )
  transitioned_at <- started_at + 90
  started <- store_start_prioritized_reader_question(
    store,
    reader_id = reader_id,
    request_key = deferred$request_key,
    pinned_inputs = deferred$pinned_inputs,
    requested_at = deferred$requested_at,
    worker_id = "question-worker",
    transition_at = transitioned_at
  )

  testthat::expect_identical(started$run$status, "pending")
  testthat::expect_equal(started$run$requested_at, started_at + 2)
  testthat::expect_equal(
    started$run$lease_expires_at,
    transitioned_at + agent_run_pending_lease_seconds()
  )
  testthat::expect_null(store_get_deferred_reader_question(store, reader_id))

  replay <- store_start_prioritized_reader_question(
    store,
    reader_id = reader_id,
    request_key = deferred$request_key,
    pinned_inputs = deferred$pinned_inputs,
    requested_at = deferred$requested_at,
    worker_id = "another-worker",
    transition_at = transitioned_at + 1
  )
  testthat::expect_identical(replay$run$run_id, started$run$run_id)
}

orientation_test_tool_state <- function(
  agent,
  output = NULL,
  source_calls = if (is.null(output)) 0L else 1L
) {
  state <- rill_orientation_tool_state()
  state$source_calls <- as.integer(source_calls)
  if (!is.null(output)) {
    state$submission_attempts <- 1L
    state$submission_calls <- 1L
    state$output <- output
  }
  attr(agent, "rill_orientation_tool_state") <- state
  state
}

orientation_test_submit <- function(state, output) {
  state$submission_attempts <- 1L
  state$submission_calls <- 1L
  state$output <- output
  invisible(output)
}

orientation_test_agent_result <- function(
  stop_reason = "complete",
  requests = 1L,
  tool_calls = 2L,
  run_id = "deputy-orientation-test"
) {
  list(
    stop_reason = stop_reason,
    usage = deputy::AgentUsage(
      requests = requests,
      tool_calls = tool_calls
    ),
    run_id = run_id
  )
}

local_orientation_backend_store <- function(
  backend,
  reader_id,
  env = parent.frame()
) {
  backend <- match.arg(backend, c("memory", "postgres"))
  if (identical(backend, "memory")) {
    store <- rill_store(list(demo_mode = TRUE, actor_id = reader_id))
    store$memory$agent_runs <- list()
    store$memory$orientations <- list()
    return(store)
  }

  database_url <- Sys.getenv("RILL_TEST_DATABASE_URL", unset = "")
  testthat::skip_if(
    !nzchar(database_url),
    "RILL_TEST_DATABASE_URL is not configured"
  )
  connection_args <- postgres_connection_args(database_url)
  admin <- do.call(
    DBI::dbConnect,
    c(list(drv = RPostgres::Postgres()), connection_args)
  )
  schema_name <- paste0(
    "rill_orientation_contract_",
    substr(rill_id(Sys.getpid(), Sys.time(), stats::runif(1)), 1L, 16L)
  )
  schema_identifier <- DBI::dbQuoteIdentifier(admin, schema_name)
  DBI::dbExecute(admin, paste("CREATE SCHEMA", schema_identifier))
  withr::defer(
    {
      DBI::dbExecute(
        admin,
        paste("DROP SCHEMA", schema_identifier, "CASCADE")
      )
      DBI::dbDisconnect(admin)
    },
    envir = env
  )

  connection_args$options <- paste0("-csearch_path=", schema_name)
  database_pool <- do.call(
    pool::dbPool,
    c(
      list(drv = RPostgres::Postgres()),
      connection_args,
      list(minSize = 1, maxSize = 2, idleTimeout = 60)
    )
  )
  store <- structure(
    list(mode = "postgres", pool = database_pool),
    class = "rill_store"
  )
  withr::defer(rill_store_close(store), envir = env)
  store_apply_schema(store)

  sample <- sample_rill_data()
  for (index in seq_len(nrow(sample$feeds))) {
    store_upsert_feed(store, as.list(sample$feeds[index, , drop = FALSE]))
  }
  store_upsert_entries(store, sample$entries)
  for (document in sample$documents) {
    store_save_document(store, document)
  }
  store
}

seed_orientation_backend <- function(store, reader_id, candidate_limit = 12L) {
  candidates <- orientation_candidates(
    store,
    reader_id,
    limit = candidate_limit
  )
  boundary <- orientation_boundary(candidates)
  worker_id <- paste0("seed-worker-", store$mode)
  run <- store_start_agent_run(
    store,
    reader_id = reader_id,
    kind = "orientation",
    request_key = paste0("seed-orientation:", boundary$hash),
    pinned_inputs = list(
      boundary_hash = boundary$hash,
      candidate_limit = as.integer(candidate_limit)
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
  cards <- Map(
    function(candidate, role, frame) {
      list(
        role = role,
        frame = frame,
        document_id = candidate$document$document_id,
        entry_id = candidate$entry$entry_id,
        interpretation = "This Document tests the shared store contract.",
        why_now = "It is part of the current bounded unread set.",
        evidence = "Rill keeps the source feed"
      )
    },
    candidates[1:2],
    c("anchor", "contrast"),
    c("unresolved_question", "counterpoint")
  )
  orientation <- new_rill_orientation(
    reader_id = reader_id,
    boundary = boundary,
    question = "What must remain source-grounded?",
    introduction = "Read these two Documents as a short path.",
    cards = cards,
    agent_run_id = run$run_id
  )
  completed <- store_complete_orientation_run(
    store,
    orientation,
    worker_id = worker_id,
    finished_at = orientation$evaluated_at
  )
  completed$orientation
}

orientation_backend_run_count <- function(store, reader_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      "SELECT count(*)::integer AS count FROM agent_runs WHERE reader_id = $1",
      params = list(reader_id)
    )
    return(rows$count[[1L]])
  }
  sum(vapply(
    store$memory$agent_runs,
    \(run) identical(run$reader_id, reader_id),
    logical(1)
  ))
}

orientation_backend_open_events <- function(store, reader_id) {
  if (identical(store$mode, "postgres")) {
    return(DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT entry_id, event_type, surface, position, payload::text AS payload",
        "FROM events WHERE actor_id = $1 AND event_type = 'entry_opened'",
        "ORDER BY happened_at, event_id"
      ),
      params = list(reader_id)
    ))
  }
  store$memory$events[
    store$memory$events$actor_id == reader_id &
      store$memory$events$event_type == "entry_opened",
    c("entry_id", "event_type", "surface", "position", "payload"),
    drop = FALSE
  ]
}

orientation_backend_entry_state <- function(store, reader_id, entry_id) {
  if (identical(store$mode, "postgres")) {
    return(DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT actor_id, entry_id, read_at, read_reason, last_opened_at",
        "FROM entry_state WHERE actor_id = $1 AND entry_id = $2"
      ),
      params = list(reader_id, entry_id)
    ))
  }
  store$memory$state[
    store$memory$state$actor_id == reader_id &
      store$memory$state$entry_id == entry_id,
    c("actor_id", "entry_id", "read_at", "read_reason", "last_opened_at"),
    drop = FALSE
  ]
}

expect_orientation_selection_rollback <- function(store, reader_id) {
  orientation <- seed_orientation_backend(store, reader_id)
  card <- orientation$cards[[1L]]
  before_state <- orientation_backend_entry_state(
    store,
    reader_id,
    card$entry_id
  )
  before_events <- orientation_backend_open_events(store, reader_id)

  testthat::expect_error(
    store_select_orientation_card(
      store,
      reader_id = reader_id,
      orientation_id = orientation$orientation_id,
      revision_id = orientation$revision_id,
      card_id = card$card_id,
      entry_id = card$entry_id,
      document_id = card$document_id,
      basis_hash = card$basis_hash,
      rationale_hash = card$rationale_hash,
      event_id = paste0("failed-selection-", store$mode),
      session_id = "selection-session",
      selected_at = as.POSIXct("2026-09-02 13:00:00", tz = "UTC")
    ),
    class = "rill_test_event_failure"
  )

  testthat::expect_equal(
    orientation_backend_entry_state(store, reader_id, card$entry_id),
    before_state
  )
  testthat::expect_equal(
    orientation_backend_open_events(store, reader_id),
    before_events
  )
}

expect_orientation_selection_event_conflict <- function(store, reader_id) {
  orientation <- seed_orientation_backend(store, reader_id)
  card <- orientation$cards[[1L]]
  event_id <- paste0("occupied-selection-event-", store$mode)
  store_record_event(
    store,
    list(
      event_id = event_id,
      actor_id = reader_id,
      entry_id = card$entry_id,
      session_id = "other-session",
      event_type = "entry_saved",
      happened_at = as.POSIXct("2026-09-02 12:30:00", tz = "UTC"),
      surface = "story_list",
      position = 3L,
      payload = list(source = "unrelated")
    )
  )
  before_state <- orientation_backend_entry_state(
    store,
    reader_id,
    card$entry_id
  )

  testthat::expect_error(
    store_select_orientation_card(
      store,
      reader_id = reader_id,
      orientation_id = orientation$orientation_id,
      revision_id = orientation$revision_id,
      card_id = card$card_id,
      entry_id = card$entry_id,
      document_id = card$document_id,
      basis_hash = card$basis_hash,
      rationale_hash = card$rationale_hash,
      event_id = event_id,
      session_id = "selection-session",
      selected_at = as.POSIXct("2026-09-02 13:00:00", tz = "UTC")
    ),
    class = "rill_event_id_conflict"
  )

  testthat::expect_equal(
    orientation_backend_entry_state(store, reader_id, card$entry_id),
    before_state
  )
  if (identical(store$mode, "postgres")) {
    events <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT event_id, event_type FROM events",
        "WHERE actor_id = $1 ORDER BY event_id"
      ),
      params = list(reader_id)
    )
  } else {
    events <- store$memory$events[
      store$memory$events$actor_id == reader_id,
      c("event_id", "event_type"),
      drop = FALSE
    ]
  }
  testthat::expect_equal(nrow(events), 1L)
  testthat::expect_identical(events$event_id[[1L]], event_id)
  testthat::expect_identical(events$event_type[[1L]], "entry_saved")
}

expect_orientation_backend_contract <- function(store, reader_id) {
  candidate_limit <- 12L
  seeded <- seed_orientation_backend(store, reader_id, candidate_limit)
  initial_run_count <- orientation_backend_run_count(store, reader_id)
  launched <- FALSE

  reused <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = paste0("reuse-worker-", store$mode),
    model = "openai/gpt-test",
    candidate_limit = candidate_limit,
    agent_factory = function(...) {
      launched <<- TRUE
      stop("A current Orientation must not construct an Agent.")
    },
    schedule_timeout = FALSE
  )

  testthat::expect_identical(reused$status, "current")
  testthat::expect_identical(reused$orientation$revision_id, seeded$revision_id)
  testthat::expect_null(reused$run)
  testthat::expect_null(reused$promise)
  testthat::expect_identical(launched, FALSE)
  testthat::expect_identical(
    orientation_backend_run_count(store, reader_id),
    initial_run_count
  )

  invalidated <- seeded$cards[[1L]]
  retained <- seeded$cards[[2L]]
  store_mark_opened(store, reader_id, invalidated$entry_id)
  state <- orientation_status(store, reader_id, limit = candidate_limit)

  testthat::expect_identical(state$due, TRUE)
  testthat::expect_identical(
    state$invalid_document_ids,
    invalidated$document_id
  )
  testthat::expect_length(state$orientation$cards, 1L)
  testthat::expect_identical(
    state$orientation$cards[[1L]]$document_id,
    retained$document_id
  )
  testthat::expect_length(store_get_orientation(store, reader_id)$cards, 2L)

  agent_factory <- function(...) {
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    orientation_test_tool_state(agent)
    agent$run_async <- function(...) {
      promises::promise_reject(simpleError("Provider unavailable."))
    }
    agent$interrupt <- \(reason) TRUE
    agent
  }
  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = paste0("failure-worker-", store$mode),
    model = "openai/gpt-test",
    candidate_limit = candidate_limit,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )
  rejected <- NULL
  promises::then(
    control$promise,
    onRejected = function(error) {
      rejected <<- error
      NULL
    }
  )
  deadline <- Sys.time() + 2
  while (is.null(rejected) && Sys.time() < deadline) {
    later::run_now(0.01)
  }

  failed_run <- store_get_agent_run(
    store,
    reader_id,
    control$run$run_id
  )
  fallback <- orientation_status(store, reader_id, limit = candidate_limit)
  queue <- store_list_entries(store, reader_id, view = "unread", limit = 500L)

  testthat::expect_s3_class(rejected, "simpleError")
  testthat::expect_identical(failed_run$status, "failed")
  testthat::expect_identical(
    failed_run$terminal_reason,
    "agent_error:simpleError"
  )
  testthat::expect_identical(
    store_get_orientation(store, reader_id)$revision_id,
    seeded$revision_id
  )
  testthat::expect_length(fallback$orientation$cards, 1L)
  testthat::expect_identical(
    fallback$orientation$cards[[1L]]$document_id,
    retained$document_id
  )
  testthat::expect_identical(invalidated$entry_id %in% queue$entry_id, FALSE)
  testthat::expect_gt(nrow(queue), 0L)

  visible_orientation <- fallback$orientation
  visible_card <- visible_orientation$cards[[1L]]
  config <- rill_config()
  config$actor_id <- reader_id
  config$demo_mode <- identical(store$mode, "memory")
  config$orientation_enabled <- FALSE
  config$refresh_on_start <- FALSE
  selection <- NULL
  shiny::testServer(rill_server(config, store), {
    session$setInputs(select_entry = NULL)
    session$flushReact()
    session$setInputs(
      select_entry = list(
        id = visible_card$entry_id,
        position = 1L,
        surface = "orientation",
        orientation_id = visible_orientation$orientation_id,
        revision_id = visible_orientation$revision_id,
        card_id = visible_card$card_id,
        document_id = visible_card$document_id,
        basis_hash = visible_card$basis_hash,
        rationale_hash = visible_card$rationale_hash,
        nonce = 1
      )
    )
    session$flushReact()
    selection <<- list(
      entry_id = selected_id(),
      document_id = selected_document_id(),
      document = selected_document(),
      provenance = selected_orientation_provenance()
    )
  })

  testthat::expect_identical(selection$entry_id, visible_card$entry_id)
  testthat::expect_identical(selection$document_id, visible_card$document_id)
  testthat::expect_identical(
    selection$document$document_id,
    visible_card$document_id
  )
  opened_state <- orientation_backend_entry_state(
    store,
    reader_id,
    visible_card$entry_id
  )
  testthat::expect_identical(nrow(opened_state), 1L)
  testthat::expect_identical(opened_state$read_reason[[1L]], "opened")
  testthat::expect_identical(is.na(opened_state$read_at[[1L]]), FALSE)
  testthat::expect_equal(
    opened_state$last_opened_at[[1L]],
    opened_state$read_at[[1L]]
  )
  testthat::expect_identical(
    selection$provenance,
    list(
      orientation_id = visible_orientation$orientation_id,
      revision_id = visible_orientation$revision_id,
      card_id = visible_card$card_id,
      entry_id = visible_card$entry_id,
      document_id = visible_card$document_id,
      basis_hash = visible_card$basis_hash,
      rationale_hash = visible_card$rationale_hash,
      agent_run_id = visible_orientation$agent_run_id,
      role = visible_card$role,
      frame = visible_card$frame,
      interpretation = visible_card$interpretation,
      why_now = visible_card$why_now,
      evidence = visible_card$evidence,
      boundary_hash = visible_orientation$boundary$hash,
      evaluated_at = format(
        visible_orientation$evaluated_at,
        tz = "UTC",
        usetz = TRUE
      ),
      policy_version = visible_orientation$policy_version
    )
  )

  events <- orientation_backend_open_events(store, reader_id)
  testthat::expect_identical(nrow(events), 1L)
  testthat::expect_identical(events$entry_id[[1L]], visible_card$entry_id)
  testthat::expect_identical(events$surface[[1L]], "orientation")
  testthat::expect_identical(events$position[[1L]], 1L)
  payload <- jsonlite::fromJSON(events$payload[[1L]], simplifyVector = FALSE)
  testthat::expect_identical(
    payload$orientation_id,
    visible_orientation$orientation_id
  )
  testthat::expect_identical(
    payload$revision_id,
    visible_orientation$revision_id
  )
  testthat::expect_identical(payload$card_id, visible_card$card_id)
  testthat::expect_identical(payload$entry_id, visible_card$entry_id)
  testthat::expect_identical(payload$document_id, visible_card$document_id)
  testthat::expect_identical(payload$basis_hash, visible_card$basis_hash)
  testthat::expect_identical(
    payload$rationale_hash,
    visible_card$rationale_hash
  )
  testthat::expect_identical(
    payload$agent_run_id,
    visible_orientation$agent_run_id
  )
  testthat::expect_identical(payload$role, visible_card$role)
  testthat::expect_identical(payload$frame, visible_card$frame)
  testthat::expect_identical(
    payload$interpretation,
    visible_card$interpretation
  )
  testthat::expect_identical(payload$why_now, visible_card$why_now)
  testthat::expect_identical(payload$evidence, visible_card$evidence)
  testthat::expect_identical(
    payload$boundary_hash,
    visible_orientation$boundary$hash
  )
  testthat::expect_identical(
    payload$evaluated_at,
    format(visible_orientation$evaluated_at, tz = "UTC", usetz = TRUE)
  )
  testthat::expect_identical(
    payload$policy_version,
    visible_orientation$policy_version
  )
}
