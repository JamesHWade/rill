testthat::test_that("due polling refreshes each active shared Feed once", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader-one"))
  store$memory$feeds$last_polled_at <- "2026-09-03 10:00:00 UTC"
  store_ensure_reader(store, "reader-two")
  shared_feed_id <- store$memory$feeds$feed_id[[1L]]
  store_subscribe_feed(store, "reader-two", shared_feed_id)
  inactive_feed_id <- store$memory$feeds$feed_id[[3L]]
  store_unsubscribe_feed(store, "reader-one", inactive_feed_id)
  refreshed <- character()
  refresh <- function(store, feed) {
    refreshed <<- c(refreshed, feed$feed_id)
    list(feed_id = feed$feed_id, added = 0L, not_modified = TRUE)
  }

  result <- run_due_feed_polling(
    store,
    interval_minutes = 60L,
    failure_threshold = 5L,
    refresh = refresh,
    now = "2026-09-03 12:00:00 UTC"
  )

  testthat::expect_identical(result$status, "succeeded")
  testthat::expect_identical(result$due_count, 2L)
  testthat::expect_identical(result$succeeded_count, 2L)
  testthat::expect_identical(result$failed_count, 0L)
  testthat::expect_disjoint(refreshed, inactive_feed_id)
  testthat::expect_identical(sum(refreshed == shared_feed_id), 1L)
  testthat::expect_identical(
    store$memory$feed_poll_outcomes$status,
    rep("not_modified", 2L)
  )
  testthat::expect_identical(store$memory$feed_poll_runs$status, "succeeded")

  cached <- run_due_feed_polling(
    store,
    interval_minutes = 60L,
    failure_threshold = 5L,
    refresh = refresh,
    now = "2026-09-03 12:30:00 UTC"
  )
  testthat::expect_identical(cached$due_count, 0L)
  testthat::expect_length(refreshed, 2L)
})

testthat::test_that("isolated Feed failures remain durable without failing", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader"))
  store$memory$feeds$last_polled_at <- "2026-09-03 10:00:00 UTC"
  stale_feed_id <- store$memory$feeds$feed_id[[1L]]
  refresh <- function(store, feed) {
    if (identical(feed$feed_id, stale_feed_id)) {
      cli::cli_abort("Feed unavailable.", class = "rill_feed_stale")
    }
    list(feed_id = feed$feed_id, added = 2L, not_modified = FALSE)
  }

  result <- run_due_feed_polling(
    store,
    interval_minutes = 60L,
    failure_threshold = 2L,
    refresh = refresh,
    now = "2026-09-03 12:00:00 UTC"
  )

  testthat::expect_identical(result$status, "partial")
  testthat::expect_identical(result$succeeded_count, 2L)
  testthat::expect_identical(result$failed_count, 1L)
  failure <- store$memory$feed_poll_outcomes[
    store$memory$feed_poll_outcomes$feed_id == stale_feed_id,
    ,
    drop = FALSE
  ]
  testthat::expect_identical(failure$status, "failed")
  testthat::expect_identical(failure$error_class, "rill_feed_stale")
  testthat::expect_identical(failure$error_message, "Feed unavailable.")
  testthat::expect_identical(
    store$memory$feeds$poll_status[
      store$memory$feeds$feed_id == stale_feed_id
    ],
    "failed"
  )
})

testthat::test_that("the configured failure threshold marks a run failed", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader"))
  store$memory$feeds$last_polled_at <- "2026-09-03 10:00:00 UTC"
  stale_feed_id <- store$memory$feeds$feed_id[[1L]]
  refresh <- function(store, feed) {
    if (identical(feed$feed_id, stale_feed_id)) {
      cli::cli_abort("Feed unavailable.", class = "rill_feed_stale")
    }
    list(feed_id = feed$feed_id, added = 0L, not_modified = TRUE)
  }

  result <- run_due_feed_polling(
    store,
    interval_minutes = 60L,
    failure_threshold = 1L,
    refresh = refresh,
    now = "2026-09-03 12:00:00 UTC"
  )

  testthat::expect_identical(result$status, "failed")
  testthat::expect_identical(store$memory$feed_poll_runs$status, "failed")
})

testthat::test_that("an overlapping polling run is skipped", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader"))

  outer <- store_with_feed_poll_lock(store, function() {
    run_due_feed_polling(
      store,
      interval_minutes = 60L,
      failure_threshold = 5L,
      refresh = function(...) stop("refresh should not run")
    )
  })

  testthat::expect_identical(outer$acquired, TRUE)
  testthat::expect_identical(outer$value$status, "skipped_overlap")
  testthat::expect_identical(store$memory$feed_poll_locked, FALSE)
})

testthat::test_that("a new run marks an interrupted predecessor failed", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader"))
  store$memory$feeds$last_polled_at <- utc_now()
  store_start_feed_poll_run(
    store,
    "interrupted-run",
    started_at = "2026-09-03 10:00:00 UTC",
    due_count = 1L,
    failure_threshold = 5L
  )

  result <- run_due_feed_polling(
    store,
    interval_minutes = 60L,
    failure_threshold = 5L,
    now = "2026-09-03 12:00:00 UTC"
  )
  interrupted <- store$memory$feed_poll_runs[
    store$memory$feed_poll_runs$run_id == "interrupted-run",
    ,
    drop = FALSE
  ]

  testthat::expect_identical(result$recovered_count, 1L)
  testthat::expect_identical(interrupted$status, "failed")
  testthat::expect_identical(
    interrupted$error_class,
    "rill_feed_poll_interrupted"
  )
  testthat::expect_identical(
    interrupted$completed_at,
    "2026-09-03 12:00:00 UTC"
  )
})

testthat::test_that("poll_feeds reports the configured failure threshold", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader"))
  testthat::local_mocked_bindings(
    rill_config = \() {
      list(
        demo_mode = FALSE,
        poll_interval_minutes = 60L,
        poll_failure_threshold = 1L
      )
    },
    init_telemetry = \(config) NULL,
    rill_store = \(config) store,
    rill_store_close = \(store) NULL,
    run_due_feed_polling = function(...) {
      list(
        status = "failed",
        due_count = 3L,
        failed_count = 1L,
        failure_threshold = 1L
      )
    }
  )

  testthat::expect_error(
    poll_feeds(),
    class = "rill_feed_poll_failure_threshold"
  )
})
