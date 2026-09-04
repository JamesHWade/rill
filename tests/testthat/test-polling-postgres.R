testthat::test_that("the Feed polling migration is bundled", {
  migration_ids <- vapply(
    schema_migration_files(),
    `[[`,
    character(1),
    "migration_id"
  )

  testthat::expect_in("011_feed_polling", migration_ids)
})

testthat::test_that("PostgreSQL serializes and records Feed polling", {
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
    "rill_feed_polling_",
    substr(rill_id(Sys.getpid(), Sys.time(), stats::runif(1)), 1L, 16L)
  )
  schema_identifier <- DBI::dbQuoteIdentifier(admin, schema_name)
  DBI::dbExecute(admin, paste("CREATE SCHEMA", schema_identifier))
  withr::defer({
    DBI::dbExecute(admin, paste("DROP SCHEMA", schema_identifier, "CASCADE"))
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
      private_reader_id = "reader-one"
    ),
    class = "rill_store"
  )
  withr::defer(rill_store_close(store))
  store_apply_schema(store)

  feed <- as.list(sample_rill_data()$feeds[1L, , drop = FALSE])
  store_upsert_feed(store, feed)
  store_subscribe_feed(store, "reader-one", feed$feed_id)
  entries <- sample_rill_data()$entries
  entries <- entries[entries$feed_id == feed$feed_id, , drop = FALSE]
  testthat::expect_identical(
    store_upsert_entries(store, entries),
    nrow(entries)
  )
  entries$title <- paste(entries$title, "updated")
  testthat::expect_identical(store_upsert_entries(store, entries), 0L)
  stored <- store_list_entries(store, "reader-one", view = "all")
  testthat::expect_setequal(stored$title, entries$title)
  DBI::dbExecute(
    store$pool,
    "UPDATE feeds SET last_polled_at = '2026-09-03 10:00:00 UTC'"
  )

  blocker <- do.call(
    DBI::dbConnect,
    c(list(drv = RPostgres::Postgres()), connection_args)
  )
  withr::defer(DBI::dbDisconnect(blocker))
  DBI::dbBegin(blocker)
  withr::defer(try(DBI::dbRollback(blocker), silent = TRUE))
  acquired <- DBI::dbGetQuery(
    blocker,
    paste(
      "SELECT pg_try_advisory_xact_lock(",
      "hashtext('rill:feed-poll')) AS acquired"
    )
  )$acquired[[1L]]
  testthat::expect_identical(acquired, TRUE)
  overlapping <- run_due_feed_polling(
    store,
    interval_minutes = 60L,
    failure_threshold = 5L,
    refresh = function(...) stop("refresh should not run"),
    now = "2026-09-03 12:00:00 UTC"
  )
  testthat::expect_identical(overlapping$status, "skipped_overlap")
  DBI::dbRollback(blocker)

  store_start_feed_poll_run(
    store,
    "interrupted-run",
    started_at = "2026-09-03 11:00:00 UTC",
    due_count = 2L,
    failure_threshold = 5L
  )
  store_record_feed_poll_outcome(
    store,
    list(
      run_id = "interrupted-run",
      feed_id = feed$feed_id,
      status = "not_modified",
      added_count = 0L,
      error_class = NA_character_,
      error_message = NA_character_
    ),
    started_at = "2026-09-03 11:15:00 UTC",
    completed_at = "2026-09-03 11:30:00 UTC"
  )
  DBI::dbExecute(
    store$pool,
    "UPDATE feeds SET last_polled_at = '2026-09-03 10:00:00 UTC'"
  )
  result <- run_due_feed_polling(
    store,
    interval_minutes = 60L,
    failure_threshold = 5L,
    refresh = function(store, feed) {
      list(feed_id = feed$feed_id, added = 0L, not_modified = TRUE)
    },
    now = "2026-09-03 12:00:00 UTC"
  )
  run <- DBI::dbGetQuery(
    store$pool,
    "SELECT * FROM feed_poll_runs WHERE run_id = $1",
    params = list(result$run_id)
  )
  outcomes <- DBI::dbGetQuery(
    store$pool,
    "SELECT * FROM feed_poll_outcomes WHERE run_id = $1",
    params = list(result$run_id)
  )
  stored_feed <- DBI::dbGetQuery(
    store$pool,
    "SELECT poll_status, last_polled_at FROM feeds WHERE feed_id = $1",
    params = list(feed$feed_id)
  )
  interrupted <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT status, succeeded_count, failed_count, error_class",
      "FROM feed_poll_runs",
      "WHERE run_id = 'interrupted-run'"
    )
  )

  testthat::expect_identical(result$status, "succeeded")
  testthat::expect_identical(result$due_count, 1L)
  testthat::expect_identical(result$recovered_count, 1L)
  testthat::expect_identical(run$status, "succeeded")
  testthat::expect_identical(as.integer(run$succeeded_count), 1L)
  testthat::expect_identical(outcomes$status, "not_modified")
  testthat::expect_identical(stored_feed$poll_status, "not_modified")
  testthat::expect_identical(interrupted$status, "failed")
  testthat::expect_identical(as.integer(interrupted$succeeded_count), 1L)
  testthat::expect_identical(as.integer(interrupted$failed_count), 1L)
  testthat::expect_identical(
    interrupted$error_class,
    "rill_feed_poll_interrupted"
  )
  testthat::expect_gt(
    stored_feed$last_polled_at,
    as.POSIXct("2026-09-03 10:00:00", tz = "UTC")
  )
})
