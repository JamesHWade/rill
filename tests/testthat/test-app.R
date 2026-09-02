testthat::test_that("the package exposes a focused application API", {
  testthat::expect_setequal(
    getNamespaceExports("rill"),
    c(
      "poll_feeds",
      "prepare_today",
      "read_opml",
      "rill_app",
      "write_opml"
    )
  )
})

testthat::test_that("rill_app creates a Shiny application in demo mode", {
  withr::local_envvar(DATABASE_URL = "")

  app <- rill_app()

  testthat::expect_s3_class(app, "shiny.appobj")
  testthat::expect_type(app$serverFuncSource, "closure")
})

testthat::test_that("rill_app interrupts Agent Runs orphaned by a restart", {
  withr::local_envvar(DATABASE_URL = "")
  store <- rill_store(list(demo_mode = TRUE))
  recovered_at <- as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  statuses <- c("pending", "running", "cancelling")

  for (index in seq_along(statuses)) {
    reader_id <- paste0("reader-", index)
    run <- store_start_agent_run(
      store,
      reader_id = reader_id,
      kind = "question",
      request_key = paste0("question-", index),
      pinned_inputs = list(document_id = paste0("document-", index)),
      requested_at = recovered_at - 60
    )
    if (!identical(statuses[[index]], "pending")) {
      run <- store_claim_agent_run(
        store,
        reader_id = reader_id,
        run_id = run$run_id,
        worker_id = "previous-process",
        started_at = recovered_at - 30,
        lease_expires_at = recovered_at + 60
      )
    }
    if (identical(statuses[[index]], "cancelling")) {
      store_request_agent_run_cancel(
        store,
        reader_id = reader_id,
        run_id = run$run_id,
        requested_at = recovered_at - 10
      )
    }
  }

  testthat::local_mocked_bindings(
    rill_store = \(config) store,
    utc_now = \() recovered_at
  )

  rill_app()

  recovered <- unname(store$memory$agent_runs)
  testthat::expect_identical(
    vapply(recovered, `[[`, character(1), "status"),
    rep("interrupted", 3L)
  )
  testthat::expect_identical(
    vapply(recovered, `[[`, character(1), "terminal_reason"),
    rep("process_restarted", 3L)
  )
  testthat::expect_identical(
    lapply(recovered, `[[`, "terminal_at"),
    rep(list(recovered_at), 3L)
  )
})

testthat::test_that("rill_app mounts the capture route", {
  withr::local_envvar(c(
    DATABASE_URL = "",
    RILL_CAPTURE_TOKEN = "test-secret"
  ))
  app <- rill_app()
  request <- list2env(
    list(
      PATH_INFO = capture_endpoint_path,
      REQUEST_METHOD = "POST",
      HTTP_AUTHORIZATION = "Bearer wrong-secret"
    ),
    parent = emptyenv()
  )

  response <- app$httpHandler(request)

  testthat::expect_identical(response$status, 401L)
})

testthat::test_that("installed runtime assets are available", {
  assets <- c(
    rill_package_file("app", "_brand.yml"),
    rill_package_file("app", "www", "app.js"),
    rill_package_file("app", "www", "rill-duck-dark.png"),
    rill_package_file("app", "www", "rill-duck.png"),
    rill_package_file("app", "www", "styles.css"),
    rill_package_file("sql", "001_init.sql"),
    rill_package_file("sql", "002_agent_runs.sql")
  )

  testthat::expect_length(assets, 7L)
  testthat::expect_identical(file.exists(assets), rep(TRUE, 7L))
})

testthat::test_that("chat submissions receive an idempotency token", {
  javascript <- readLines(
    rill_package_file("app", "www", "app.js"),
    warn = FALSE
  )
  javascript <- paste(javascript, collapse = "\n")

  testthat::expect_match(
    javascript,
    'event.name !== "reader_chat_user_input"',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    '["", "shinychat.userInput"].includes(event.inputType)',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    '"reader_chat_submission_id"',
    fixed = TRUE
  )
})

testthat::test_that("polling requires durable configuration", {
  withr::local_envvar(DATABASE_URL = "")

  testthat::expect_snapshot(poll_feeds(), error = TRUE)
})

testthat::test_that("preparing today requires durable configuration", {
  withr::local_envvar(DATABASE_URL = "")

  testthat::expect_snapshot(prepare_today(), error = TRUE)
})
