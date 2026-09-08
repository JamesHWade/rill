testthat::test_that("scheduled polling backs off repeated failures and manual retry resets it", {
  for (backend in c("memory", "postgres")) {
    store <- local_orientation_backend_store(backend, "reader")
    feed_id <- store_list_feeds(store, "reader")$feed_id[[1]]
    now <- as.POSIXct("2026-09-08 01:00:00", tz = "UTC")
    for (index in 1:6) {
      run_id <- paste0("retry-", index)
      store_start_feed_poll_run(
        store,
        run_id,
        format(now, tz = "UTC", usetz = TRUE),
        1L,
        1L
      )
      store_record_feed_poll_outcome(
        store,
        list(
          run_id = run_id,
          feed_id = feed_id,
          status = "failed",
          added_count = 0L,
          error_class = "httr2_http_403",
          error_message = "Forbidden"
        ),
        format(now, tz = "UTC", usetz = TRUE),
        format(now + index, tz = "UTC", usetz = TRUE)
      )
      due_at <- now + index + min(1440, 60 * 2^(index - 1L)) * 60
      testthat::expect_disjoint(
        store_list_due_feeds(store, due_at - 1, 60L)$feed_id,
        feed_id
      )
      testthat::expect_contains(
        store_list_due_feeds(store, due_at, 60L)$feed_id,
        feed_id
      )
    }
    result <- refresh_reader_feeds(
      store,
      "reader",
      feed_ids = feed_id,
      refresh = function(store, feed) list(feed_id = feed$feed_id, added = 0L)
    )
    testthat::expect_identical(result$succeeded_count, 1L)
    feeds <- store_list_feeds(store, "reader")
    checked <- as.POSIXct(
      feeds$last_polled_at[match(feed_id, feeds$feed_id)],
      tz = "UTC"
    )
    testthat::expect_contains(
      store_list_due_feeds(store, checked + 3600, 60L)$feed_id,
      feed_id
    )
  }
})

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

testthat::test_that("failed polling prepares healthy articles before signaling the threshold", {
  local_article_preparation_worker()
  store <- preparation_test_store()
  store$memory$entries <- store$memory$entries[0, , drop = FALSE]
  store$memory$feeds$last_polled_at <- NA_character_
  healthy <- store$memory$feeds$feed_url[[1L]]
  xml <- paste0(
    '<rss><channel><title>Healthy</title><link>https://example.org</link>',
    '<item><guid>new</guid><title>New article</title>',
    '<link>https://example.org/new</link></item></channel></rss>'
  )
  testthat::local_mocked_bindings(
    rill_config = \() {
      list(
        demo_mode = FALSE,
        poll_interval_minutes = 60L,
        poll_failure_threshold = 1L,
        defuddle_backend = "hosted"
      )
    },
    init_telemetry = \(config) NULL,
    rill_store = \(config) store,
    rill_store_close = \(store) NULL,
    fetch_feed = function(url, ...) {
      if (!identical(url, healthy)) {
        cli::cli_abort(
          "Unavailable: https://example.org/private?token=secret",
          class = "httr2_http_404"
        )
      }
      parse_feed_document(xml, url)
    },
    fetch_defuddled_markdown = function(source_url, config) {
      testthat::expect_identical(store$memory$feed_poll_locked, FALSE)
      "Complete public article."
    }
  )

  messages <- testthat::capture_messages({
    error <- tryCatch(poll_feeds(), error = identity)
  })

  testthat::expect_s3_class(error, "rill_feed_poll_failure_threshold")
  testthat::expect_identical(error$result$succeeded_count, 1L)
  testthat::expect_identical(error$result$failed_count, 2L)
  testthat::expect_identical(
    error$result$preparation,
    list(prepared = 1L, failed = 0L)
  )
  entry <- store_get_entry(store, "reader", store$memory$entries$entry_id[[1L]])
  document <- store_get_document(store, "reader", entry$entry_id)
  testthat::expect_identical(document$markdown, "Complete public article.")
  testthat::expect_identical(document$source_url, "https://example.org/new")
  testthat::expect_match(
    paste(messages, collapse = "\n"),
    "httr2_http_404: 2",
    fixed = TRUE
  )
  testthat::expect_no_match(
    paste(messages, collapse = "\n"),
    "private|secret|example.org"
  )
  testthat::expect_identical(store$memory$feed_poll_runs$status, "failed")
  failures <- Filter(
    \(outcome) identical(outcome$status, "failed"),
    error$result$outcomes
  )
  testthat::expect_identical(
    failures[[1L]]$error_message,
    "Unavailable: https://example.org/private?token=secret"
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
