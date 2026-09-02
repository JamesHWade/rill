testthat::test_that("PostgreSQL migrates and persists Agent Runs", {
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
    "rill_agent_runs_",
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
    list(mode = "postgres", pool = database_pool),
    class = "rill_store"
  )
  withr::defer(rill_store_close(store))

  store_apply_schema(store)
  store_apply_schema(store)

  migrations <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT migration_id, checksum FROM schema_migrations",
      "ORDER BY migration_id"
    )
  )
  testthat::expect_identical(
    migrations$migration_id,
    c("001_init", "002_agent_runs")
  )
  testthat::expect_match(migrations$checksum, "^[0-9a-f]{64}$")

  requested_at <- as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  first <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "conversation",
    request_key = "conversation-message-17",
    pinned_inputs = list(
      document_id = "document-1",
      policy_version = "v1"
    ),
    requested_at = requested_at
  )
  replay <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "conversation",
    request_key = "conversation-message-17",
    pinned_inputs = list(
      document_id = "document-1",
      policy_version = "v1"
    ),
    requested_at = requested_at + 60
  )

  testthat::expect_identical(replay, first)
  testthat::expect_identical(
    store_get_agent_run(store, "reader-1", first$run_id),
    first
  )
  testthat::expect_error(
    store_start_agent_run(
      store,
      reader_id = "reader-1",
      kind = "conversation",
      request_key = "conversation-message-17",
      pinned_inputs = list(document_id = "different-document")
    ),
    class = "rill_agent_run_replay_conflict"
  )
  testthat::expect_error(
    store_start_agent_run(
      store,
      reader_id = "reader-1",
      kind = "orientation",
      request_key = "orientation-library-41",
      pinned_inputs = list(document_id = "document-2")
    ),
    class = "rill_agent_run_conflict"
  )

  claimed <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = first$run_id,
    worker_id = "worker-1",
    started_at = requested_at + 1,
    lease_expires_at = requested_at + 61
  )
  testthat::expect_identical(claimed$status, "running")
  testthat::expect_identical(claimed$worker_id, "worker-1")

  completed <- store_finish_agent_run(
    store,
    reader_id = "reader-1",
    run_id = first$run_id,
    worker_id = "worker-1",
    status = "completed",
    usage = list(requests = 1L, output_tokens = 42L),
    terminal_reason = "complete",
    deputy_run_id = "deputy-run-17",
    finished_at = requested_at + 2
  )
  testthat::expect_identical(completed$status, "completed")
  testthat::expect_null(completed$lease_expires_at)
  testthat::expect_equal(
    completed$usage[c("requests", "output_tokens")],
    list(requests = 1, output_tokens = 42)
  )
  testthat::expect_identical(completed$terminal_reason, "complete")
  testthat::expect_identical(completed$deputy_run_id, "deputy-run-17")
  testthat::expect_identical(
    store_get_agent_run(store, "reader-1", first$run_id),
    completed
  )

  second <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = "orientation-library-42",
    pinned_inputs = list(document_id = "document-2")
  )
  second <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = second$run_id,
    worker_id = "worker-1",
    lease_expires_at = requested_at + 120
  )
  cancelling <- store_request_agent_run_cancel(
    store,
    reader_id = "reader-1",
    run_id = second$run_id,
    requested_at = requested_at + 90
  )
  testthat::expect_identical(cancelling$status, "cancelling")
  cancelled <- store_finish_agent_run(
    store,
    reader_id = "reader-1",
    run_id = second$run_id,
    worker_id = "worker-1",
    status = "cancelled",
    finished_at = requested_at + 91
  )
  testthat::expect_identical(cancelled$status, "cancelled")

  pending <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "conversation",
    request_key = "conversation-message-18",
    pinned_inputs = list(document_id = "document-3")
  )
  pending_cancelled <- store_request_agent_run_cancel(
    store,
    reader_id = "reader-1",
    run_id = pending$run_id,
    requested_at = requested_at + 92
  )
  testthat::expect_identical(pending_cancelled$status, "cancelled")
  testthat::expect_identical(
    pending_cancelled$terminal_reason,
    "cancelled_before_start"
  )

  original_checksum <- migrations$checksum[[1L]]
  DBI::dbExecute(
    store$pool,
    paste(
      "UPDATE schema_migrations SET checksum = 'changed'",
      "WHERE migration_id = '001_init'"
    )
  )
  testthat::expect_error(
    store_apply_schema(store),
    class = "rill_schema_drift"
  )
  DBI::dbExecute(
    store$pool,
    paste(
      "UPDATE schema_migrations SET checksum = $1",
      "WHERE migration_id = '001_init'"
    ),
    params = list(original_checksum)
  )
  DBI::dbExecute(
    store$pool,
    paste(
      "INSERT INTO schema_migrations (migration_id, checksum)",
      "VALUES ('999_future', $1)"
    ),
    params = list(strrep("0", 64L))
  )
  testthat::expect_error(
    store_apply_schema(store),
    class = "rill_schema_newer"
  )
})
