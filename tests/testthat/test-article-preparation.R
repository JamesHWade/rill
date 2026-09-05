testthat::test_that("reading a cache miss never calls the extractor", {
  store <- preparation_test_store()
  entry <- store_get_entry(store, "reader", store$memory$entries$entry_id[[1]])
  testthat::local_mocked_bindings(
    document_from_defuddle = function(...) stop("Reading must not extract")
  )
  document <- reading_document(store, "reader", entry)
  testthat::expect_identical(document$acquisition_method, "feed_fallback")
  testthat::expect_match(document$markdown, "[[:alnum:]]")
  testthat::expect_identical(reading_document(store, "reader", entry), document)
})

testthat::test_that("preparation leases prevent duplicate work and stale completion", {
  store <- preparation_test_store()
  id <- store$memory$entries$entry_id[[1]]
  now <- as.POSIXct("2026-09-05 12:00:00", tz = "UTC")
  first <- claim_preparation(store, id, now = now)
  testthat::expect_type(first, "character")
  testthat::expect_null(claim_preparation(store, id, now = now + 1))
  second <- claim_preparation(store, id, now = now + 301)
  testthat::expect_type(second, "character")
  testthat::expect_identical(
    finish_preparation(store, id, first, now = now + 302),
    FALSE
  )
  testthat::expect_identical(preparation_attempt(store, id)$token, second)
})

testthat::test_that("failed acquisition backs off and bounds automatic attempts", {
  store <- preparation_test_store()
  id <- store$memory$entries$entry_id[[1]]
  now <- as.POSIXct("2026-09-05 12:00:00", tz = "UTC")
  for (attempt in seq_len(5L)) {
    token <- claim_preparation(store, id, now = now)
    testthat::expect_type(token, "character")
    finish_preparation(
      store,
      id,
      token,
      failure = list(code = "http_failed"),
      now = now
    )
    testthat::expect_null(claim_preparation(
      store,
      id,
      retry = TRUE,
      now = now + 1
    ))
    now <- preparation_attempt(store, id)$next_attempt_at + 1
  }
  testthat::expect_null(claim_preparation(store, id, now = now))
  testthat::expect_type(
    claim_preparation(store, id, retry = TRUE, now = now),
    "character"
  )
  testthat::expect_identical(preparation_attempt(store, id)$attempts, 1L)
})

testthat::test_that("background completion preserves explicitly selected copies", {
  store <- preparation_test_store()
  id <- store$memory$entries$entry_id[[1]]
  entry <- store_get_entry(store, "reader", id)
  fallback <- reading_document(store, "reader", entry)
  store_select_document(store, "reader", fallback$document_id)
  testthat::local_mocked_bindings(
    fetch_defuddled_markdown = function(...) {
      "# Full article\n\nThe complete source text."
    }
  )
  document <- document_from_defuddle(entry, list(defuddle_backend = "hosted"))
  token <- claim_preparation(store, id)
  testthat::expect_identical(
    finish_preparation(store, id, token, document),
    TRUE
  )
  testthat::expect_identical(
    public_reading_document(store, id)$document_id,
    document$document_id
  )
  testthat::expect_identical(
    store_get_document(store, "reader", id)$document_id,
    fallback$document_id
  )
  testthat::expect_identical(
    store_get_document_record(store, fallback$document_id),
    fallback
  )
})

testthat::test_that("automatic candidates exclude disabled Readers and old entries", {
  store <- preparation_test_store()
  now <- as.POSIXct("2026-08-19 12:00:00", tz = "UTC")
  testthat::expect_identical(
    preparation_candidates(store, "reader", now = now),
    store$memory$entries$entry_id[[1]]
  )
  store$memory$readers$status <- "disabled"
  testthat::expect_identical(
    preparation_candidates(store, "reader", now = now),
    character()
  )
  testthat::expect_null(preparation_entry(
    store,
    store$memory$entries$entry_id[[1]],
    "reader"
  ))
})

testthat::test_that("workers receive only public entry and extraction settings", {
  store <- preparation_test_store()
  id <- store$memory$entries$entry_id[[1]]
  request <- NULL
  testthat::local_mocked_bindings(launch_article_preparation = function(
    entry,
    config,
    ...
  ) {
    request <<- list(entry = entry, config = config)
    list(is_alive = \() FALSE)
  })
  config <- list(
    defuddle_backend = "local",
    defuddle_command = "defuddle",
    database_url = "private-db",
    auth0_client_secret = "private-auth",
    agent_api_key = "private-model"
  )
  job <- start_article_preparation(store, config, "reader", id)
  withr::defer(close_article_preparation(job))
  testthat::expect_named(
    request$config,
    c("defuddle_backend", "defuddle_command")
  )
  testthat::expect_identical(request$entry$entry_id, id)
  testthat::expect_no_match(
    jsonlite::toJSON(request),
    "private-db|private-auth|private-model|reader_id"
  )
})

testthat::test_that("the real article worker reports failure without discarding its feed copy", {
  store <- preparation_test_store()
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  fallback <- reading_document(store, "reader", entry)
  config <- list(
    defuddle_backend = "local",
    defuddle_command = "rill-test-no-such-extractor"
  )
  job <- start_article_preparation(store, config, "reader", entry$entry_id)
  withr::defer(close_article_preparation(job))
  testthat::expect_type(job, "list")
  job$process$wait(timeout = 30000)
  messages <- testthat::capture_messages({
    result <- poll_article_preparation(job, store, config)
  })
  testthat::expect_identical(result$failure$code, "extractor_missing")
  testthat::expect_match(messages, result$failure$reference, fixed = TRUE)
  testthat::expect_identical(
    store_get_document(store, "reader", entry$entry_id)$document_id,
    fallback$document_id
  )
  testthat::expect_identical(file.exists(job$directory), FALSE)
})

testthat::test_that("scheduled preparation counts worker launch failures", {
  store <- preparation_test_store()
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  store$memory$entries$published_at[[1]] <- utc_now()
  fallback <- reading_document(store, "reader", entry)
  launch_directory <- NULL
  testthat::local_mocked_bindings(launch_article_preparation = function(
    entry,
    config,
    directory,
    package_path
  ) {
    launch_directory <<- directory
    stop("private-launch-token")
  })
  testthat::capture_messages({
    result <- prepare_recent_articles(store, list(defuddle_backend = "hosted"))
  })
  testthat::expect_identical(result, list(prepared = 0L, failed = 1L))
  testthat::expect_type(launch_directory, "character")
  testthat::expect_identical(file.exists(launch_directory), FALSE)
  testthat::expect_identical(
    preparation_attempt(store, entry$entry_id)$status,
    "failed"
  )
  testthat::expect_identical(
    store_get_document(store, "reader", entry$entry_id)$document_id,
    fallback$document_id
  )
})

testthat::test_that("scheduled preparation is bounded and isolates failed articles", {
  local_article_preparation_worker()
  store <- preparation_test_store()
  store$memory$entries$published_at <- utc_now()
  called <- character()
  testthat::local_mocked_bindings(fetch_defuddled_markdown = function(
    source_url,
    config
  ) {
    called <<- c(called, source_url)
    if (length(called) == 1L) {
      stop("unavailable")
    }
    "Complete public article."
  })
  config <- list(defuddle_backend = "hosted")
  testthat::capture_messages({
    result <- prepare_recent_articles(store, config, limit = 2L)
  })
  testthat::expect_identical(result, list(prepared = 1L, failed = 1L))
  testthat::expect_length(called, 2L)
  testthat::expect_length(store$memory$documents, 1L)
  testthat::expect_identical(
    prepare_recent_articles(store, config, budget = 0),
    list(prepared = 0L, failed = 0L)
  )
})

testthat::test_that("a timed-out worker is stopped and leaves its copy retryable", {
  store <- preparation_test_store()
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  fallback <- reading_document(store, "reader", entry)
  killed <- FALSE
  directory <- withr::local_tempdir()
  job <- list(
    entry_id = entry$entry_id,
    token = claim_preparation(store, entry$entry_id),
    directory = directory,
    started_at = Sys.time(),
    process = list(
      is_alive = \() TRUE,
      get_result = \() stop("not ready: private-token"),
      kill = function() {
        killed <<- TRUE
      }
    )
  )
  config <- list(defuddle_backend = "hosted")
  testthat::expect_null(poll_article_preparation(job, store, config))
  logs <- testthat::capture_messages({
    result <- poll_article_preparation(job, store, config, timeout = 0)
  })
  testthat::expect_identical(killed, TRUE)
  testthat::expect_identical(file.exists(directory), FALSE)
  testthat::expect_identical(
    preparation_attempt(store, entry$entry_id)$status,
    "failed"
  )
  testthat::expect_identical(
    store_get_document(store, "reader", entry$entry_id)$document_id,
    fallback$document_id
  )
  testthat::expect_no_match(
    paste(logs, jsonlite::toJSON(result)),
    "private-token"
  )
})

testthat::test_that("a real running worker returns promptly at its deadline", {
  store <- preparation_test_store()
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  fallback <- reading_document(store, "reader", entry)
  directory <- withr::local_tempdir()
  process <- callr::r_bg(
    \() Sys.sleep(30),
    supervise = TRUE,
    user_profile = FALSE,
    stdout = file.path(directory, "stdout"),
    stderr = file.path(directory, "stderr")
  )
  job <- list(
    entry_id = entry$entry_id,
    token = claim_preparation(store, entry$entry_id),
    directory = directory,
    started_at = Sys.time(),
    process = process
  )
  withr::defer(close_article_preparation(job))
  testthat::expect_identical(process$is_alive(), TRUE)
  started <- Sys.time()
  testthat::capture_messages({
    result <- poll_article_preparation(
      job,
      store,
      list(defuddle_backend = "hosted"),
      timeout = 0
    )
  })
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  testthat::expect_lt(elapsed, 5)
  testthat::expect_type(result$failure, "list")
  testthat::expect_identical(process$is_alive(), FALSE)
  testthat::expect_identical(file.exists(directory), FALSE)
  testthat::expect_identical(
    preparation_attempt(store, entry$entry_id)$status,
    "failed"
  )
  testthat::expect_identical(
    store_get_document(store, "reader", entry$entry_id)$document_id,
    fallback$document_id
  )
})

testthat::test_that("completion can restore a previously stored full copy as the head", {
  store <- preparation_test_store()
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  testthat::local_mocked_bindings(fetch_defuddled_markdown = function(...) {
    "Full copy."
  })
  full <- document_from_defuddle(entry, list(defuddle_backend = "hosted"))
  store_save_document(store, full)
  store_save_document(store, document_fallback(entry))
  token <- claim_preparation(store, entry$entry_id)
  finish_preparation(store, entry$entry_id, token, full)
  testthat::expect_identical(
    public_reading_document(store, entry$entry_id)$document_id,
    full$document_id
  )
  testthat::expect_length(store$memory$documents, 2L)
})

testthat::test_that("completion cannot overwrite a private copy or a newer public head", {
  store <- preparation_test_store()
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  private <- capture_document(
    store,
    capture_test_payload(source_url = entry$url, canonical_url = entry$url),
    "reader"
  )
  token <- claim_preparation(store, entry$entry_id)
  testthat::local_mocked_bindings(fetch_defuddled_markdown = function(...) {
    "Full public copy."
  })
  full <- document_from_defuddle(entry, list(defuddle_backend = "hosted"))
  store_save_document(store, full)
  testthat::expect_identical(
    finish_preparation(store, entry$entry_id, token, document_fallback(entry)),
    TRUE
  )
  testthat::expect_identical(
    public_reading_document(store, entry$entry_id)$document_id,
    full$document_id
  )
  testthat::expect_identical(
    store_get_document(store, "reader", entry$entry_id)$document_id,
    private$document_id
  )
})
