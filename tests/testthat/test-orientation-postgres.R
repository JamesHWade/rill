testthat::test_that("PostgreSQL persists the current Orientation aggregate", {
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
    "rill_orientation_",
    substr(rill_id(Sys.getpid(), Sys.time(), stats::runif(1)), 1L, 16L)
  )
  schema_identifier <- DBI::dbQuoteIdentifier(admin, schema_name)
  DBI::dbExecute(admin, paste("CREATE SCHEMA", schema_identifier))
  withr::defer({
    DBI::dbExecute(
      admin,
      paste("DROP SCHEMA", schema_identifier, "CASCADE")
    )
    DBI::dbDisconnect(admin)
  })

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
    list(
      mode = "postgres",
      pool = database_pool,
      private_reader_id = "reader-1"
    ),
    class = "rill_store"
  )
  withr::defer(rill_store_close(store))
  store_apply_schema(store)

  sample <- sample_rill_data()
  for (index in seq_len(nrow(sample$feeds))) {
    feed <- as.list(sample$feeds[index, , drop = FALSE])
    store_upsert_feed(store, feed)
    store_subscribe_feed(
      store,
      "reader-1",
      feed$feed_id,
      folder = feed$folder
    )
  }
  store_upsert_entries(store, sample$entries[1:3, , drop = FALSE])
  for (index in seq_len(3L)) {
    store_save_document(store, sample$documents[[index]])
  }
  candidates <- orientation_candidates(store, "reader-1", limit = 3L)
  boundary <- orientation_boundary(candidates)
  document <- candidates[[1]]$document
  worker_id <- "orientation-worker-1"
  run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = paste0("orientation:", boundary$hash),
    pinned_inputs = list(boundary_hash = boundary$hash),
    worker_id = worker_id
  )
  run <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    worker_id = worker_id,
    lease_expires_at = Sys.time() + 120
  )
  orientation <- new_rill_orientation(
    reader_id = "reader-1",
    boundary = boundary,
    question = "What deserves attention?",
    introduction = "Start with this source boundary.",
    cards = list(list(
      role = "anchor",
      document_id = document$document_id,
      entry_id = document$entry_id,
      interpretation = "This story establishes the boundary.",
      why_now = "It is the clearest unread account.",
      evidence = "Rill keeps the source feed"
    )),
    agent_run_id = run$run_id,
    evaluated_at = as.POSIXct("2026-09-02 16:00:00", tz = "UTC")
  )

  completed <- store_complete_orientation_run(
    store,
    orientation,
    worker_id = worker_id,
    deputy_run_id = "deputy-orientation-1",
    finished_at = orientation$evaluated_at
  )
  restored <- store_get_orientation(store, "reader-1")
  poll_token <- store_orientation_poll_token(store, "reader-1")

  testthat::expect_identical(completed$run$status, "completed")
  testthat::expect_identical(
    restored$orientation_id,
    orientation$orientation_id
  )
  testthat::expect_identical(restored$revision_id, orientation$revision_id)
  testthat::expect_identical(restored$reader_id, "reader-1")
  testthat::expect_identical(restored$boundary$hash, boundary$hash)
  testthat::expect_identical(
    restored$cards[[1]]$document_id,
    document$document_id
  )
  testthat::expect_identical(
    as.numeric(restored$evaluated_at),
    as.numeric(orientation$evaluated_at)
  )
  testthat::expect_match(poll_token, "^[[:xdigit:]]{32}$")
  testthat::expect_null(store_get_orientation(store, "reader-2"))

  evaluation_column <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT attnotnull FROM pg_attribute",
      "WHERE attrelid = 'orientations'::regclass",
      "AND attname = 'evaluation_run_id'"
    )
  )
  constraints <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT contype, pg_get_constraintdef(oid) AS definition",
      "FROM pg_constraint",
      "WHERE conrelid = 'orientations'::regclass"
    )
  )
  agent_run_constraints <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT conname, pg_get_constraintdef(oid) AS definition",
      "FROM pg_constraint",
      "WHERE conrelid = 'agent_runs'::regclass"
    )
  )
  linked_run <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT ar.status FROM orientations AS o",
      "JOIN agent_runs AS ar ON ar.run_id = o.evaluation_run_id",
      "WHERE o.reader_id = 'reader-1'"
    )
  )

  testthat::expect_identical(evaluation_column$attnotnull, TRUE)
  unique_constraint <- constraints[
    constraints$contype == "u" &
      constraints$definition == "UNIQUE (evaluation_run_id)",
    ,
    drop = FALSE
  ]
  foreign_key <- constraints[
    constraints$contype == "f" &
      constraints$definition ==
        paste(
          "FOREIGN KEY (reader_id, evaluation_run_id)",
          "REFERENCES agent_runs(reader_id, run_id)"
        ),
    ,
    drop = FALSE
  ]
  testthat::expect_identical(nrow(unique_constraint), 1L)
  testthat::expect_identical(nrow(foreign_key), 1L)
  testthat::expect_identical(
    agent_run_constraints$definition[
      agent_run_constraints$conname == "agent_runs_reader_run_key"
    ],
    "UNIQUE (reader_id, run_id)"
  )
  testthat::expect_identical(linked_run$status, "completed")

  dismissal_event <- list(
    event_id = "postgres-orientation-dismissal",
    actor_id = "reader-1",
    entry_id = NULL,
    session_id = "postgres-session",
    event_type = "orientation_card_dismissed",
    happened_at = as.POSIXct("2026-09-02 16:01:00", tz = "UTC"),
    surface = "orientation",
    position = 1L,
    payload = list()
  )
  dismissed <- store_dismiss_orientation_card(
    store,
    "reader-1",
    orientation$cards[[1L]]$card_id,
    orientation$revision_id,
    orientation$cards[[1L]]$rationale_hash,
    event = dismissal_event
  )
  dismissed_poll_token <- store_orientation_poll_token(store, "reader-1")
  replayed <- store_dismiss_orientation_card(
    store,
    "reader-1",
    orientation$cards[[1L]]$card_id,
    orientation$revision_id,
    orientation$cards[[1L]]$rationale_hash,
    event = dismissal_event
  )
  dismissal_rows <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT payload::text AS payload FROM events",
      "WHERE event_id = 'postgres-orientation-dismissal'"
    )
  )
  current <- orientation_status(store, "reader-1", limit = 3L)

  testthat::expect_identical(replayed$basis_hash, dismissed$basis_hash)
  testthat::expect_length(unique(c(dismissed_poll_token, poll_token)), 2L)
  testthat::expect_identical(nrow(dismissal_rows), 1L)
  dismissal_payload <- jsonlite::fromJSON(dismissal_rows$payload[[1L]])
  testthat::expect_identical(
    dismissal_payload$revision_id,
    orientation$revision_id
  )
  testthat::expect_identical(current$due, TRUE)
  testthat::expect_length(current$orientation$cards, 0L)

  stale_boundary <- current$boundary
  stale_run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = paste0("orientation:", stale_boundary$hash),
    pinned_inputs = list(
      boundary_hash = stale_boundary$hash,
      candidate_limit = 3L
    ),
    worker_id = worker_id
  )
  stale_run <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = stale_run$run_id,
    worker_id = worker_id,
    lease_expires_at = Sys.time() + 120
  )
  stale_orientation <- new_rill_orientation(
    reader_id = "reader-1",
    boundary = stale_boundary,
    question = NULL,
    introduction = NULL,
    cards = list(),
    agent_run_id = stale_run$run_id
  )
  store_mark_opened(
    store,
    "reader-1",
    current$candidates[[1L]]$entry$entry_id
  )
  stale_publication <- store_complete_orientation_run(
    store,
    stale_orientation,
    worker_id = worker_id
  )

  testthat::expect_null(stale_publication)
  testthat::expect_identical(
    store_get_agent_run(store, "reader-1", stale_run$run_id)$status,
    "running"
  )

  migrations <- DBI::dbGetQuery(
    store$pool,
    "SELECT migration_id FROM schema_migrations ORDER BY migration_id"
  )
  testthat::expect_identical(
    migrations$migration_id,
    c(
      "001_init",
      "002_agent_runs",
      "003_agent_run_question_kind",
      "004_orientations",
      "005_orientation_data_destination_settings",
      "006_deferred_reader_questions",
      "007_agent_run_response",
      "008_reader_identities",
      "009_reader_library",
      "010_reader_documents",
      "011_feed_polling",
      "012_article_preparation"
    )
  )
})

testthat::test_that("PostgreSQL creates one fallback for a missing head", {
  database_url <- Sys.getenv("RILL_TEST_DATABASE_URL", unset = "")
  testthat::skip_if(
    !nzchar(database_url),
    "RILL_TEST_DATABASE_URL is not configured"
  )

  store <- local_orientation_backend_store("postgres", "reader-1")
  schema_name <- DBI::dbGetQuery(
    store$pool,
    "SELECT current_schema() AS schema_name"
  )$schema_name[[1L]]
  connection_args <- postgres_connection_args(database_url)

  entry <- sample_rill_data()$entries[1L, , drop = FALSE]
  entry$entry_id <- "orientation-fallback-race"
  entry$external_id <- "orientation-fallback-race"
  entry$url <- "https://example.com/orientation-fallback-race"
  store_upsert_entries(store, entry)
  entry <- as.list(entry)

  fallback_document <- function(captured_at) {
    new_rill_document(
      entry_id = entry$entry_id,
      source_url = entry$url,
      canonical_url = entry$canonical_url,
      acquisition_method = "feed_fallback",
      producer = "orientation-feed-copy",
      title = entry$title,
      author = entry$author,
      published_at = entry$published_at,
      markdown = entry$summary,
      captured_at = captured_at,
      received_at = captured_at,
      provenance = list(kind = "feed_content_fallback")
    )
  }
  documents <- list(
    fallback_document(as.POSIXct("2026-09-02 12:00:00", tz = "UTC")),
    fallback_document(as.POSIXct("2026-09-02 12:00:01", tz = "UTC"))
  )
  testthat::expect_length(
    unique(vapply(
      documents,
      `[[`,
      character(1),
      "document_id"
    )),
    2L
  )

  blocker_args <- connection_args
  blocker_args$options <- paste0("-csearch_path=", schema_name)
  blocker <- do.call(
    DBI::dbConnect,
    c(list(drv = RPostgres::Postgres()), blocker_args)
  )
  blocker_active <- TRUE
  withr::defer({
    if (blocker_active) {
      try(DBI::dbRollback(blocker), silent = TRUE)
    }
    DBI::dbDisconnect(blocker)
  })
  DBI::dbBegin(blocker)
  DBI::dbGetQuery(
    blocker,
    "SELECT entry_id FROM entries WHERE entry_id = $1 FOR UPDATE",
    params = list(entry$entry_id)
  )

  package_path <- normalizePath(
    testthat::test_path("..", ".."),
    mustWork = FALSE
  )
  if (!file.exists(file.path(package_path, "DESCRIPTION"))) {
    package_path <- NULL
  }
  application_names <- paste0(
    "rill_fallback_race_",
    Sys.getpid(),
    "_",
    seq_along(documents)
  )
  save_fallback <- function(
    package_path,
    connection_args,
    schema_name,
    application_name,
    document
  ) {
    if (is.null(package_path)) {
      loadNamespace("rill")
    } else {
      pkgload::load_all(package_path, quiet = TRUE)
    }
    connection_args$options <- paste0("-csearch_path=", schema_name)
    connection_args$application_name <- application_name
    database_pool <- do.call(
      pool::dbPool,
      c(
        list(drv = RPostgres::Postgres()),
        connection_args,
        list(minSize = 1, maxSize = 1, idleTimeout = 60)
      )
    )
    on.exit(pool::poolClose(database_pool), add = TRUE)
    worker_store <- structure(
      list(mode = "postgres", pool = database_pool),
      class = "rill_store"
    )
    saved <- rill:::store_save_document_if_missing_head(
      worker_store,
      "reader-1",
      document
    )
    list(
      created = saved$created,
      document_id = saved$document$document_id
    )
  }
  workers <- Map(
    function(application_name, document) {
      callr::r_bg(
        save_fallback,
        args = list(
          package_path = package_path,
          connection_args = connection_args,
          schema_name = schema_name,
          application_name = application_name,
          document = document
        ),
        stdout = "|",
        stderr = "|",
        supervise = TRUE
      )
    },
    application_names,
    documents
  )
  withr::defer({
    for (worker in workers) {
      if (worker$is_alive()) {
        worker$kill_tree()
      }
    }
  })

  blocked <- character()
  deadline <- Sys.time() + 20
  while (Sys.time() < deadline) {
    activity <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT application_name, wait_event_type FROM pg_stat_activity",
        "WHERE application_name IN ($1, $2)"
      ),
      params = as.list(application_names)
    )
    blocked <- activity$application_name[
      activity$wait_event_type == "Lock"
    ]
    if (setequal(blocked, application_names)) {
      break
    }
    Sys.sleep(0.05)
  }
  testthat::expect_setequal(blocked, application_names)

  DBI::dbCommit(blocker)
  blocker_active <- FALSE
  results <- lapply(workers, function(worker) {
    worker$wait(timeout = 20000)
    testthat::expect_identical(worker$is_alive(), FALSE)
    worker$get_result()
  })
  workers <- list()

  rows <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT d.document_id, h.document_id AS head_document_id",
      "FROM documents AS d",
      "LEFT JOIN public_document_heads AS h",
      "ON h.document_id = d.document_id",
      "WHERE d.entry_id = $1"
    ),
    params = list(entry$entry_id)
  )
  testthat::expect_identical(nrow(rows), 1L)
  testthat::expect_identical(
    rows$document_id[[1L]],
    rows$head_document_id[[1L]]
  )
  testthat::expect_setequal(
    vapply(results, `[[`, logical(1), "created"),
    c(TRUE, FALSE)
  )
  testthat::expect_identical(
    unique(vapply(results, `[[`, character(1), "document_id")),
    rows$document_id[[1L]]
  )
})
