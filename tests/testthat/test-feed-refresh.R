testthat::test_that("a real worker records feed failures and cleans up its resources", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  feed_id <- store$memory$feeds$feed_id[[1L]]
  store$memory$feeds$feed_url[[1L]] <- "http://localhost/not-allowed"
  job <- start_feed_refresh(config, store, config$actor_id, feed_id)
  withr::defer(close_feed_refresh(job))
  testthat::expect_r6_class(job$process, "r_process")
  overlap <- start_feed_refresh(config, store, config$actor_id)
  testthat::expect_identical(
    poll_feed_refresh(overlap)$status,
    "skipped_overlap"
  )
  result <- wait_for_feed_refresh(job)
  testthat::expect_identical(result$status, "failed")
  testthat::expect_equal(result$failed_count, 1L)
  testthat::expect_identical(store$memory$feed_poll_outcomes$feed_id, feed_id)
  testthat::expect_identical(store$memory$feed_poll_locked, FALSE)
  testthat::expect_identical(dir.exists(job$directory), FALSE)
})

testthat::test_that("worker acquisition merges without replacing concurrent Reader changes", {
  withr::local_envvar(DATABASE_URL = "")
  gate <- withr::local_tempfile()
  local_feed_refresh_worker(gate = gate)
  config <- rill_config()
  store <- rill_store(config)
  feed_id <- store$memory$feeds$feed_id[[1L]]
  job <- start_feed_refresh(config, store, config$actor_id, feed_id)
  withr::defer(close_feed_refresh(job))
  entry_id <- store$memory$entries$entry_id[[1L]]
  store_toggle_state(store, config$actor_id, entry_id, "starred")
  store_rename_feed(store, config$actor_id, feed_id, "My renamed feed")
  store_move_feed(store, config$actor_id, feed_id, "Research")
  store_unsubscribe_feed(store, config$actor_id, feed_id)
  before <- as.list(store$memory)
  testthat::expect_identical(poll_feed_refresh(job)$status, "running")
  file.create(gate)
  result <- wait_for_feed_refresh(job)
  testthat::expect_identical(result$status, "succeeded")
  testthat::expect_equal(result$outcomes[[1L]]$added_count, 1L)
  testthat::expect_in("background-entry", store$memory$entries$external_id)
  for (field in setdiff(
    names(before),
    c(
      "feeds",
      "entries",
      "feed_poll_runs",
      "feed_poll_outcomes",
      "feed_poll_locked"
    )
  )) {
    testthat::expect_identical(
      store$memory[[field]],
      before[[field]],
      info = field
    )
  }
  testthat::expect_disjoint(
    store_list_feeds(store, config$actor_id)$feed_id,
    feed_id
  )
  store_subscribe_feed(store, config$actor_id, feed_id)
  again <- start_feed_refresh(config, store, config$actor_id, feed_id)
  withr::defer(close_feed_refresh(again))
  repeated <- wait_for_feed_refresh(again)
  testthat::expect_equal(repeated$outcomes[[1L]]$added_count, 0L)
})

testthat::test_that("worker startup failures release the memory polling lock", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  testthat::local_mocked_bindings(launch_feed_refresh = function(...) {
    stop(structure(
      list(message = "worker unavailable"),
      class = c("test_worker_failure", "error", "condition")
    ))
  })
  testthat::expect_error(
    start_feed_refresh(config, store, config$actor_id),
    class = "test_worker_failure"
  )
  testthat::expect_identical(store$memory$feed_poll_locked, FALSE)
})

testthat::test_that("a crashed worker is retryable", {
  withr::local_envvar(DATABASE_URL = "")
  local_feed_refresh_worker(gate = withr::local_tempfile())
  config <- rill_config()
  store <- rill_store(config)
  job <- start_feed_refresh(config, store, config$actor_id)
  withr::defer(close_feed_refresh(job))
  job$process$kill()
  job$process$wait(1000)
  testthat::expect_identical(poll_feed_refresh(job)$status, "error")
  testthat::expect_identical(store$memory$feed_poll_locked, FALSE)
  testthat::expect_identical(dir.exists(job$directory), FALSE)
})

testthat::test_that("the background refresh deadline stops work and releases its resources", {
  withr::local_envvar(DATABASE_URL = "")
  local_feed_refresh_worker(gate = withr::local_tempfile())
  config <- rill_config()
  store <- rill_store(config)
  job <- start_feed_refresh(config, store, config$actor_id)
  withr::defer(close_feed_refresh(job))
  job$started_at <- Sys.time() - 3601
  result <- poll_feed_refresh(job)
  testthat::expect_identical(result$status, "error")
  testthat::expect_identical(poll_feed_refresh(job), result)
  testthat::expect_identical(store$memory$feed_poll_locked, FALSE)
  testthat::expect_identical(dir.exists(job$directory), FALSE)
})
