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

  migration_files <- schema_migration_files()
  apply_migration <- function(migration_id) {
    migration <- migration_files[[match(
      migration_id,
      vapply(migration_files, `[[`, character(1), "migration_id")
    )]]
    statements <- Filter(
      nzchar,
      trimws(strsplit(migration$sql, ";", fixed = TRUE)[[1]])
    )
    for (statement in statements) {
      DBI::dbExecute(store$pool, statement)
    }
  }
  apply_migration("002_agent_runs")
  DBI::dbExecute(
    store$pool,
    paste(
      "INSERT INTO agent_runs (",
      paste(
        "run_id, reader_id, kind, request_key, status, pinned_inputs,",
        "requested_at, updated_at"
      ),
      ") VALUES ('legacy-run', 'reader-1', 'conversation',",
      "'legacy-question', 'completed', '{}'::jsonb, now(), now())"
    )
  )
  apply_migration("003_agent_run_question_kind")
  migrated_kind <- DBI::dbGetQuery(
    store$pool,
    "SELECT kind FROM agent_runs WHERE run_id = 'legacy-run'"
  )$kind
  testthat::expect_identical(migrated_kind, "question")
  constraint <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT pg_get_constraintdef(oid) AS definition",
      "FROM pg_constraint WHERE conname = 'agent_runs_kind_check'"
    )
  )$definition
  testthat::expect_match(constraint, "question", fixed = TRUE)
  testthat::expect_no_match(constraint, "conversation", fixed = TRUE)
  DBI::dbExecute(store$pool, "DROP TABLE agent_runs")

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
    c("001_init", "002_agent_runs", "003_agent_run_question_kind")
  )
  testthat::expect_match(migrations$checksum, "^[0-9a-f]{64}$")

  requested_at <- as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  pinned_inputs <- list(
    submission_id = "ask-rill-message-17",
    entry_id = "entry-1",
    document_id = "document-1",
    document_content_hash = strrep("a", 64L),
    document_record_hash = strrep("b", 64L),
    research_scope = list(
      kind = "selected_document",
      document_ids = "document-1"
    ),
    data_destination = "OpenAI",
    question = "What is the main claim?",
    model = "gpt-5.4",
    policy_version = "ask-rill-v1",
    limits = list(
      wall_time_seconds = 300,
      max_requests = 8L,
      max_tool_calls = 16L,
      max_total_tokens = 128000L,
      max_output_tokens = 8000L,
      max_cost_usd = 2
    )
  )
  first <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "ask-rill-message-17",
    pinned_inputs = pinned_inputs,
    requested_at = requested_at
  )
  replay <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "ask-rill-message-17",
    pinned_inputs = pinned_inputs[rev(names(pinned_inputs))],
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
      kind = "question",
      request_key = "ask-rill-message-17",
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

  timed_out <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "ask-rill-message-timeout",
    pinned_inputs = pinned_inputs
  )
  timed_out <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = timed_out$run_id,
    worker_id = "worker-1",
    lease_expires_at = requested_at + 120
  )
  timed_out <- store_finish_agent_run(
    store,
    reader_id = "reader-1",
    run_id = timed_out$run_id,
    worker_id = "worker-1",
    status = "failed",
    terminal_reason = "wall_time_limit",
    finished_at = requested_at + 92
  )
  settled <- store_enrich_timed_out_agent_run(
    store,
    reader_id = "reader-1",
    run_id = timed_out$run_id,
    worker_id = "worker-1",
    usage = list(requests = 1L, output_tokens = 12L),
    deputy_run_id = "deputy-run-time-limited",
    updated_at = requested_at + 93
  )
  testthat::expect_identical(settled$status, "failed")
  testthat::expect_identical(settled$terminal_reason, "wall_time_limit")
  testthat::expect_identical(settled$terminal_at, timed_out$terminal_at)
  testthat::expect_identical(
    settled$deputy_run_id,
    "deputy-run-time-limited"
  )
  testthat::expect_equal(
    settled$usage[c("requests", "output_tokens")],
    list(requests = 1, output_tokens = 12)
  )
  duplicate <- store_enrich_timed_out_agent_run(
    store,
    reader_id = "reader-1",
    run_id = timed_out$run_id,
    worker_id = "worker-1",
    usage = list(requests = 1L, output_tokens = 12L),
    deputy_run_id = "deputy-run-time-limited",
    updated_at = requested_at + 94
  )
  testthat::expect_identical(
    duplicate$deputy_run_id,
    settled$deputy_run_id
  )
  rejected <- store_enrich_timed_out_agent_run(
    store,
    reader_id = "reader-1",
    run_id = timed_out$run_id,
    worker_id = "worker-1",
    usage = list(requests = 99L),
    deputy_run_id = "different-deputy-run",
    updated_at = requested_at + 95
  )
  testthat::expect_null(rejected)

  pending <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "ask-rill-message-18",
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

  unclaimed <- store_start_agent_run(
    store,
    reader_id = "reader-claim-failed",
    kind = "question",
    request_key = "unclaimed-question",
    pinned_inputs = list(document_id = "document-unclaimed"),
    requested_at = requested_at,
    worker_id = "worker-claim-failed"
  )
  unclaimed <- store_fail_unstarted_agent_run(
    store,
    reader_id = "reader-claim-failed",
    run_id = unclaimed$run_id,
    worker_id = "worker-claim-failed",
    phase = "start",
    terminal_reason = "claim_error:test_database_error",
    failed_at = requested_at + 1
  )
  testthat::expect_identical(unclaimed$status, "failed")
  testthat::expect_identical(
    unclaimed$terminal_reason,
    "claim_error:test_database_error"
  )
  testthat::expect_null(
    store_fail_unstarted_agent_run(
      store,
      reader_id = "reader-claim-failed",
      run_id = unclaimed$run_id,
      worker_id = "worker-claim-failed",
      phase = "start",
      terminal_reason = "claim_failed",
      failed_at = requested_at + 2
    )
  )
  after_claim_failure <- store_start_agent_run(
    store,
    reader_id = "reader-claim-failed",
    kind = "question",
    request_key = "after-claim-failure",
    pinned_inputs = list(document_id = "document-after-claim-failure"),
    requested_at = requested_at + 2
  )
  testthat::expect_identical(after_claim_failure$status, "pending")
  store_request_agent_run_cancel(
    store,
    reader_id = "reader-claim-failed",
    run_id = after_claim_failure$run_id,
    requested_at = requested_at + 3
  )

  expired <- store_start_agent_run(
    store,
    reader_id = "reader-recovery",
    kind = "question",
    request_key = "expired-question",
    pinned_inputs = list(document_id = "document-expired"),
    requested_at = requested_at
  )
  expired <- store_claim_agent_run(
    store,
    reader_id = "reader-recovery",
    run_id = expired$run_id,
    worker_id = "expired-process",
    started_at = requested_at,
    lease_expires_at = requested_at + 1
  )
  expired_recovery <- store_interrupt_expired_agent_runs(
    store,
    recovered_at = requested_at + 2
  )
  testthat::expect_identical(
    vapply(expired_recovery, `[[`, character(1), "run_id"),
    expired$run_id
  )
  replacement <- store_start_agent_run(
    store,
    reader_id = "reader-recovery",
    kind = "question",
    request_key = "replacement-question",
    pinned_inputs = list(document_id = "document-replacement"),
    requested_at = requested_at + 2
  )
  expired <- store_get_agent_run(store, "reader-recovery", expired$run_id)
  testthat::expect_identical(expired$status, "interrupted")
  testthat::expect_identical(expired$terminal_reason, "lease_expired")
  testthat::expect_identical(replacement$status, "pending")

  running <- store_start_agent_run(
    store,
    reader_id = "reader-restart-running",
    kind = "question",
    request_key = "running-question",
    pinned_inputs = list(document_id = "document-running"),
    requested_at = requested_at
  )
  running <- store_claim_agent_run(
    store,
    reader_id = "reader-restart-running",
    run_id = running$run_id,
    worker_id = "previous-process",
    started_at = requested_at,
    lease_expires_at = requested_at + 3600
  )
  cancelling <- store_start_agent_run(
    store,
    reader_id = "reader-restart-cancelling",
    kind = "question",
    request_key = "cancelling-question",
    pinned_inputs = list(document_id = "document-cancelling"),
    requested_at = requested_at
  )
  cancelling <- store_claim_agent_run(
    store,
    reader_id = "reader-restart-cancelling",
    run_id = cancelling$run_id,
    worker_id = "previous-process",
    started_at = requested_at,
    lease_expires_at = requested_at + 3600
  )
  cancelling <- store_request_agent_run_cancel(
    store,
    reader_id = "reader-restart-cancelling",
    run_id = cancelling$run_id,
    requested_at = requested_at + 5
  )
  restarted <- store_interrupt_agent_runs(
    store,
    recovery = "process_restart",
    recovered_at = requested_at + 10
  )
  testthat::expect_setequal(
    vapply(restarted, `[[`, character(1), "run_id"),
    c(replacement$run_id, running$run_id, cancelling$run_id)
  )
  testthat::expect_identical(
    vapply(restarted, `[[`, character(1), "status"),
    rep("interrupted", 3L)
  )
  testthat::expect_identical(
    vapply(restarted, `[[`, character(1), "terminal_reason"),
    rep("process_restarted", 3L)
  )
  testthat::expect_length(
    store_interrupt_agent_runs(
      store,
      recovery = "process_restart",
      recovered_at = requested_at + 11
    ),
    0L
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
