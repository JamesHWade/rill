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

    session$setInputs(close_reader = list(nonce = 1))
    session$flushReact()

    testthat::expect_null(selected_id())
    testthat::expect_equal(nrow(entries()), 6L)
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
