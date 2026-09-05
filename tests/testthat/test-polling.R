testthat::test_that("disabled Readers do not keep Feeds eligible for polling", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader-one"))
  store_ensure_reader(store, "reader-two")
  shared_feed_id <- store$memory$feeds$feed_id[[1L]]
  store_subscribe_feed(store, "reader-two", shared_feed_id)
  subscriptions <- store$memory$subscriptions

  store_disable_reader(store, "reader-one", "operator:test", "polling fixture")
  testthat::expect_identical(
    store_list_active_feeds(store)$feed_id,
    shared_feed_id
  )
  testthat::expect_identical(
    store_list_due_feeds(
      store,
      now = "2099-09-03 12:00:00 UTC",
      interval_minutes = 60L
    )$feed_id,
    shared_feed_id
  )
  store_disable_reader(store, "reader-two", "operator:test", "polling fixture")
  testthat::expect_length(store_list_active_feeds(store)$feed_id, 0L)
  testthat::expect_length(
    store_list_due_feeds(
      store,
      now = "2099-09-03 12:00:00 UTC",
      interval_minutes = 60L
    )$feed_id,
    0L
  )
  testthat::expect_equal(store$memory$subscriptions, subscriptions)
})

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
    due_count = 3L,
    failure_threshold = 5L
  )
  feed_ids <- store$memory$feeds$feed_id[1:2]
  store_record_feed_poll_outcome(
    store,
    list(
      run_id = "interrupted-run",
      feed_id = feed_ids[[1L]],
      status = "not_modified",
      added_count = 0L,
      error_class = NA_character_,
      error_message = NA_character_
    ),
    started_at = utc_now(),
    completed_at = utc_now()
  )
  store_record_feed_poll_outcome(
    store,
    list(
      run_id = "interrupted-run",
      feed_id = feed_ids[[2L]],
      status = "failed",
      added_count = 0L,
      error_class = "rill_feed_unavailable",
      error_message = "Feed unavailable."
    ),
    started_at = utc_now(),
    completed_at = utc_now()
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
  testthat::expect_identical(interrupted$succeeded_count, 1L)
  testthat::expect_identical(interrupted$failed_count, 2L)
  testthat::expect_identical(
    interrupted$error_class,
    "rill_feed_poll_interrupted"
  )
  testthat::expect_identical(
    interrupted$completed_at,
    "2026-09-03 12:00:00 UTC"
  )
})

testthat::test_that("poll_feeds reports skipped and successful runs", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader"))
  result <- list(
    status = "skipped_overlap",
    due_count = 0L,
    failed_count = 0L,
    failure_threshold = 5L
  )
  testthat::local_mocked_bindings(
    rill_config = \() {
      list(
        demo_mode = FALSE,
        poll_interval_minutes = 60L,
        poll_failure_threshold = 5L
      )
    },
    init_telemetry = \(config) NULL,
    rill_store = \(config) store,
    rill_store_close = \(store) NULL,
    run_due_feed_polling = function(...) result
  )

  testthat::expect_message(
    poll_feeds(),
    "Another Feed polling run is active; skipped."
  )
  result$status <- "succeeded"
  result$due_count <- 1L
  testthat::expect_message(
    poll_feeds(),
    "Checked 1 due Feed; all succeeded."
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
testthat::test_that("manual refresh checks only active feeds in this Library", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader-one"))
  ids <- store_list_feeds(store, "reader-one")$feed_id
  store_unsubscribe_feed(store, "reader-one", ids[[2L]])
  store_subscribe_feed(store, "reader-two", ids[[2L]])
  checked <- character()
  progress <- integer()

  result <- refresh_reader_feeds(
    store,
    "reader-one",
    feed_ids = c(ids[1:2], "unowned"),
    refresh = function(store, feed) {
      checked <<- c(checked, feed$feed_id)
      list(added = 0L, not_modified = TRUE)
    },
    progress = function(index, total, title) {
      progress <<- c(progress, index)
    }
  )

  testthat::expect_identical(checked, ids[[1L]])
  testthat::expect_identical(progress, c(0L, 1L))
  testthat::expect_identical(result$due_count, 1L)
  testthat::expect_identical(
    store$memory$feed_poll_outcomes$status,
    "not_modified"
  )
  feed <- store_list_feeds(store, "reader-one")
  feed <- feed[feed$feed_id == ids[[1L]], , drop = FALSE]
  testthat::expect_identical(feed$poll_status, "not_modified")
  testthat::expect_match(feed$last_polled_at, "UTC", fixed = TRUE)
})

testthat::test_that("manual refresh retries failed feeds and respects polling overlap", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader"))
  ids <- store_list_feeds(store, "reader")$feed_id
  first <- refresh_reader_feeds(
    store,
    "reader",
    refresh = function(store, feed) {
      if (identical(feed$feed_id, ids[[1L]])) {
        cli::cli_abort("Feed unavailable", class = "rill_test_feed_unavailable")
      }
      list(added = 2L, not_modified = FALSE)
    }
  )
  testthat::expect_identical(first$failed_count, 1L)
  testthat::expect_match(feed_refresh_summary(first), "1 failed", fixed = TRUE)

  checked <- character()
  retry <- refresh_reader_feeds(
    store,
    "reader",
    failed_only = TRUE,
    refresh = function(store, feed) {
      checked <<- c(checked, feed$feed_id)
      list(added = 1L, not_modified = FALSE)
    }
  )
  testthat::expect_identical(checked, ids[[1L]])
  testthat::expect_identical(retry$failed_count, 0L)
  testthat::expect_identical(
    feed_refresh_summary(retry),
    "1 feed checked \u00b7 1 new story."
  )

  none <- refresh_reader_feeds(
    store,
    "reader",
    failed_only = TRUE,
    refresh = function(...) stop("No failures remain")
  )
  testthat::expect_identical(none$due_count, 0L)
  testthat::expect_identical(
    feed_refresh_summary(none),
    "No feeds need checking in this selection."
  )
  testthat::expect_identical(
    feed_refresh_summary(list(status = "error")),
    "Refresh stopped. Please try again."
  )

  store$memory$feed_poll_locked <- TRUE
  overlap <- refresh_reader_feeds(store, "reader", refresh = function(...) {
    stop("The poller holds the lock")
  })
  testthat::expect_identical(overlap$status, "skipped_overlap")
  testthat::expect_match(
    feed_refresh_summary(overlap),
    "Another refresh",
    fixed = TRUE
  )
})
