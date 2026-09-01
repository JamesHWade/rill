testthat::test_that("selecting a story records the open and updates the queue", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]

  shiny::testServer(rill_server(config, store), {
    session$setInputs(select_entry = NULL)
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1)
    )
    session$flushReact()

    testthat::expect_identical(selected_id(), entry_id)
    testthat::expect_identical(selected_position(), 1L)
    testthat::expect_equal(nrow(queue_entries()), 5L)
    testthat::expect_equal(nrow(entries()), 6L)
    testthat::expect_in(entry_id, entries()$entry_id)
    testthat::expect_identical(store$memory$events$event_type, "entry_opened")
    testthat::expect_identical(store$memory$state$read_reason, "opened")

    session$setInputs(close_reader = list(nonce = 1))
    session$flushReact()

    testthat::expect_null(selected_id())
    testthat::expect_equal(nrow(entries()), 6L)
  })
})

testthat::test_that("marking a story unread keeps its open history", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]

  shiny::testServer(rill_server(config, store), {
    session$setInputs(select_entry = NULL, mark_unread = NULL)
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1)
    )
    session$flushReact()
    opened_at <- store$memory$state$last_opened_at[[1]]

    session$setInputs(mark_unread = 1L)
    session$flushReact()

    testthat::expect_identical(store$memory$state$read_at, NA_character_)
    testthat::expect_identical(store$memory$state$read_reason, NA_character_)
    testthat::expect_identical(
      store$memory$state$last_opened_at,
      opened_at
    )
    testthat::expect_identical(
      store$memory$events$event_type,
      c("entry_opened", "read_state_changed")
    )
    testthat::expect_identical(status_text(), "Marked story as unread")
  })
})

testthat::test_that("bulk read actions use non-engagement reasons", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  store$memory$entries$published_at <- format(
    Sys.time() - c(60, rep(2 * 24 * 60 * 60, 5)),
    tz = "UTC",
    usetz = TRUE
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(mark_older_read = NULL, mark_all_read = NULL)
    session$flushReact()
    session$setInputs(mark_older_read = 1L)
    session$flushReact()

    testthat::expect_equal(nrow(queue_entries()), 1L)
    testthat::expect_identical(
      unique(store$memory$state$read_reason),
      "bulk_older_than_day"
    )
    testthat::expect_identical(
      status_text(),
      "Marked 5 stories older than a day as read"
    )

    session$setInputs(mark_all_read = 1L)
    session$flushReact()

    testthat::expect_equal(nrow(queue_entries()), 0L)
    testthat::expect_setequal(
      store$memory$state$read_reason,
      c("bulk_older_than_day", "bulk_all")
    )
    testthat::expect_identical(
      store$memory$events$event_type,
      c("read_state_bulk_changed", "read_state_bulk_changed")
    )
  })
})

testthat::test_that("changing views clears the current reader and retained queue", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]

  shiny::testServer(rill_server(config, store), {
    session$setInputs(view = "unread")
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1)
    )
    session$flushReact()
    session$setInputs(view = "starred")
    session$flushReact()

    testthat::expect_null(selected_id())
    testthat::expect_length(retained_ids(), 0L)
    testthat::expect_equal(nrow(entries()), 0L)
  })
})

testthat::test_that("renaming a selected feed updates its durable label", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  feed_id <- store$memory$feeds$feed_id[[1]]

  shiny::testServer(rill_server(config, store), {
    session$setInputs(select_feed = NULL)
    session$flushReact()
    session$setInputs(
      select_feed = list(id = feed_id, nonce = 1)
    )
    session$flushReact()
    session$setInputs(feed_title = "R news", rename_feed = 1L)
    session$flushReact()

    renamed <- store_list_feeds(store, config$actor_id)
    renamed <- renamed[renamed$feed_id == feed_id, , drop = FALSE]
    testthat::expect_identical(renamed$title, "R news")
    testthat::expect_identical(status_text(), "Renamed feed to R news")
    testthat::expect_identical(
      tail(store$memory$events$event_type, 1L),
      "feed_renamed"
    )
  })
})

testthat::test_that("changing story sort reorders the queue and clears the reader", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  newest_id <- store$memory$entries$entry_id[[1]]
  oldest_id <- store$memory$entries$entry_id[[6]]

  shiny::testServer(rill_server(config, store), {
    testthat::expect_identical(queue_entries()$entry_id[[1]], newest_id)

    session$setInputs(
      select_entry = list(id = newest_id, position = 1L, nonce = 1)
    )
    session$flushReact()
    session$setInputs(story_sort = "oldest")
    session$flushReact()

    testthat::expect_identical(queue_entries()$entry_id[[1]], oldest_id)
    testthat::expect_null(selected_id())
    testthat::expect_length(retained_ids(), 0L)
  })
})

testthat::test_that("calendar views filter the queue and clear the reader", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  recent_id <- store$memory$entries$entry_id[[1]]
  store$memory$entries$published_at <- format(
    Sys.time() - c(60, rep(60 * 60 * 24 * 40, 5)),
    tz = "UTC",
    usetz = TRUE
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(
      select_entry = list(id = recent_id, position = 1L, nonce = 1)
    )
    session$flushReact()
    session$setInputs(view = "today")
    session$flushReact()

    testthat::expect_identical(queue_entries()$entry_id, recent_id)
    testthat::expect_null(selected_id())
    testthat::expect_length(retained_ids(), 0L)
  })
})

testthat::test_that("preparing today reports progress and records the result", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  progress_calls <- 0L
  testthat::local_mocked_bindings(
    prepare_today_documents = function(store, config, progress) {
      progress(1L, 2L, "First article")
      progress_calls <<- progress_calls + 1L
      list(
        total = 3L,
        cached = 1L,
        prepared = 2L,
        failed = 0L,
        errors = character()
      )
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(view = "today")
    session$flushReact()
    session$setInputs(prepare_today = 1L)
    session$flushReact()

    testthat::expect_identical(progress_calls, 1L)
    testthat::expect_identical(status_kind(), "success")
    testthat::expect_identical(
      status_text(),
      "Prepared 2 reading copies · 1 already ready"
    )
    testthat::expect_identical(
      tail(store$memory$events$event_type, 1L),
      "today_prepared"
    )
  })
})

testthat::test_that("client telemetry accepts only known event types", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)

  shiny::testServer(rill_server(config, store), {
    session$setInputs(client_event = NULL)
    session$flushReact()
    session$setInputs(client_event = list(type = "not-allowed", nonce = 1))
    session$flushReact()

    testthat::expect_equal(nrow(store$memory$events), 0L)
  })
})

testthat::test_that("uploading OPML reports the result and records an event", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  file <- withr::local_tempfile(
    fileext = ".opml",
    lines = paste0(
      "<opml version=\"2.0\"><head/><body>",
      "<outline type=\"rss\" text=\"Private\" ",
      "xmlUrl=\"http://127.0.0.1/feed.xml\"/>",
      "</body></opml>"
    )
  )
  upload <- data.frame(
    name = "subscriptions.opml",
    size = file.info(file)$size,
    type = "text/x-opml",
    datapath = file,
    stringsAsFactors = FALSE
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(import_opml = NULL)
    session$flushReact()
    session$setInputs(import_opml = upload)
    session$flushReact()

    testthat::expect_identical(status_kind(), "error")
    testthat::expect_identical(status_text(), "1 feed skipped")
    testthat::expect_identical(refresh_tick(), 1L)
    testthat::expect_identical(
      store$memory$events$event_type,
      "opml_imported"
    )
  })
})

testthat::test_that("OPML import registers feeds before any refresh", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  file <- withr::local_tempfile(
    fileext = ".opml",
    lines = paste0(
      "<opml version=\"2.0\"><head/><body>",
      "<outline type=\"rss\" text=\"New feed\" ",
      "xmlUrl=\"https://example.com/feed.xml\"/>",
      "</body></opml>"
    )
  )
  upload <- data.frame(
    name = "subscriptions.opml",
    size = file.info(file)$size,
    type = "text/x-opml",
    datapath = file,
    stringsAsFactors = FALSE
  )
  refresh_calls <- 0L
  testthat::local_mocked_bindings(
    refresh_feed = function(...) {
      refresh_calls <<- refresh_calls + 1L
      cli::cli_abort("Refresh ran inline.")
    },
    .package = "rill"
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(import_opml = NULL)
    session$flushReact()
    session$setInputs(import_opml = upload)
    session$flushReact()

    testthat::expect_identical(refresh_calls, 0L)
    testthat::expect_identical(status_kind(), "success")
    testthat::expect_match(status_text(), "Refresh feeds to fetch stories")
    testthat::expect_equal(nrow(store_list_feeds(store, config$actor_id)), 4L)
  })
})
