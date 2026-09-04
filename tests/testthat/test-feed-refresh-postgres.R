testthat::test_that("background workers use independent PostgreSQL connections and shared polling locks", {
  database_url <- Sys.getenv("RILL_TEST_DATABASE_URL", unset = "")
  testthat::skip_if(
    !nzchar(database_url),
    "RILL_TEST_DATABASE_URL is not configured"
  )
  gate <- withr::local_tempfile()
  local_feed_refresh_worker(gate = gate)
  connection_args <- postgres_connection_args(database_url)
  admin <- do.call(
    DBI::dbConnect,
    c(list(drv = RPostgres::Postgres()), connection_args)
  )
  schema_name <- paste0(
    "rill_background_",
    substr(rill_id(Sys.getpid(), Sys.time(), stats::runif(1)), 1L, 16L)
  )
  schema_identifier <- DBI::dbQuoteIdentifier(admin, schema_name)
  DBI::dbExecute(admin, paste("CREATE SCHEMA", schema_identifier))
  withr::defer({
    DBI::dbExecute(admin, paste("DROP SCHEMA", schema_identifier, "CASCADE"))
    DBI::dbDisconnect(admin)
  })
  parsed <- httr2::url_parse(database_url)
  parsed$query$options <- paste0("-csearch_path=", schema_name)
  config <- list(
    database_url = httr2::url_build(parsed),
    demo_mode = FALSE,
    actor_id = "reader-one"
  )
  store <- rill_store(config)
  withr::defer(rill_store_close(store))
  feed <- as.list(sample_rill_data()$feeds[1L, , drop = FALSE])
  store_upsert_feed(store, feed)
  store_subscribe_feed(store, config$actor_id, feed$feed_id)

  DBI::dbBegin(admin)
  DBI::dbGetQuery(
    admin,
    "SELECT pg_advisory_xact_lock(hashtext('rill:feed-poll'))"
  )
  withr::defer(try(DBI::dbRollback(admin), silent = TRUE))
  skipped <- start_feed_refresh(config, store, config$actor_id)
  withr::defer(close_feed_refresh(skipped))
  testthat::expect_identical(
    wait_for_feed_refresh(skipped)$status,
    "skipped_overlap"
  )
  DBI::dbRollback(admin)

  job <- start_feed_refresh(config, store, config$actor_id, feed$feed_id)
  withr::defer(close_feed_refresh(job))
  deadline <- Sys.time() + 30
  repeat {
    progress <- poll_feed_refresh(job)
    if (
      !is.null(progress$total) ||
        !identical(progress$status, "running") ||
        Sys.time() > deadline
    ) {
      break
    }
    later::run_now(0.01)
  }
  testthat::expect_identical(progress$status, "running")
  testthat::expect_equal(progress$total, 1L)
  DBI::dbBegin(admin)
  testthat::expect_identical(
    DBI::dbGetQuery(
      admin,
      "SELECT pg_try_advisory_xact_lock(hashtext('rill:feed-poll')) AS acquired"
    )$acquired[[1L]],
    FALSE
  )
  DBI::dbRollback(admin)
  store_rename_feed(store, config$actor_id, feed$feed_id, "My feed")
  file.create(gate)
  result <- wait_for_feed_refresh(job)
  testthat::expect_identical(result$status, "succeeded")
  testthat::expect_equal(result$outcomes[[1L]]$added_count, 1L)
  testthat::expect_identical(
    store_list_feeds(store, config$actor_id)$title,
    "My feed"
  )
  testthat::expect_identical(
    store_list_entries(store, config$actor_id, view = "all")$title,
    "A new entry"
  )
  testthat::expect_identical(
    DBI::dbGetQuery(store$pool, "SELECT status FROM feed_poll_runs")$status,
    "succeeded"
  )

  store_unsubscribe_feed(store, config$actor_id, feed$feed_id)
  inactive <- start_feed_refresh(config, store, config$actor_id, feed$feed_id)
  withr::defer(close_feed_refresh(inactive))
  testthat::expect_equal(wait_for_feed_refresh(inactive)$due_count, 0L)
})
