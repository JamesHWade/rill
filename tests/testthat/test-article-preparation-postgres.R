testthat::test_that("PostgreSQL preparation claims deduplicate and reject stale results", {
  store <- local_orientation_backend_store("postgres", "reader")
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$entry_id <- "background-entry"
  entry$external_id <- "background-entry"
  entry$url <- "https://example.org/background-entry"
  entry$published_at <- utc_now()
  store_upsert_entries(store, as.data.frame(entry))
  fallback <- reading_document(store, "reader", entry)
  store_select_document(store, "reader", fallback$document_id)
  now <- Sys.time()
  first <- claim_preparation(store, entry$entry_id, now = now)
  connection <- pool::poolCheckout(store$pool)
  withr::defer(pool::poolReturn(connection))
  other <- structure(
    list(mode = "postgres", pool = connection),
    class = "rill_store"
  )
  testthat::expect_null(claim_preparation(other, entry$entry_id, now = now + 1))
  testthat::expect_identical(
    preparation_attempt(other, entry$entry_id)$attempts,
    1L
  )
  second <- claim_preparation(other, entry$entry_id, now = now + 301)
  testthat::expect_type(second, "character")
  testthat::local_mocked_bindings(fetch_defuddled_markdown = function(...) {
    "Complete article text."
  })
  document <- document_from_defuddle(entry, list(defuddle_backend = "hosted"))
  testthat::expect_identical(
    finish_preparation(store, entry$entry_id, first, document),
    FALSE
  )
  testthat::expect_identical(
    public_reading_document(store, entry$entry_id)$document_id,
    fallback$document_id
  )
  testthat::expect_identical(
    finish_preparation(store, entry$entry_id, second, document),
    TRUE
  )
  testthat::expect_identical(
    public_reading_document(store, entry$entry_id)$document_id,
    document$document_id
  )
  testthat::expect_identical(
    store_get_document(store, "reader", entry$entry_id)$document_id,
    fallback$document_id
  )
  testthat::expect_identical(
    preparation_attempt(other, entry$entry_id)$status,
    "succeeded"
  )
})

testthat::test_that("the preparation cutoff is independent of the database time zone", {
  store <- local_orientation_backend_store("postgres", "reader")
  now <- as.POSIXct("2026-09-05 12:00:00", tz = "UTC")
  cutoff <- now - 7 * 86400
  ids <- c("before-cutoff", "at-cutoff", "after-cutoff")
  for (index in seq_along(ids)) {
    entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
    entry$entry_id <- entry$external_id <- ids[[index]]
    entry$url <- paste0("https://example.org/", ids[[index]])
    entry$published_at <- format(
      cutoff + index - 2L,
      "%Y-%m-%d %H:%M:%S%z",
      tz = "UTC"
    )
    store_upsert_entries(store, as.data.frame(entry))
  }
  connection <- pool::poolCheckout(store$pool)
  withr::defer(pool::poolReturn(connection))
  original <- DBI::dbGetQuery(connection, "SHOW TimeZone")[[1L]]
  withr::defer(DBI::dbGetQuery(
    connection,
    "SELECT set_config('TimeZone', $1, false)",
    params = list(original)
  ))
  other <- structure(
    list(mode = "postgres", pool = connection),
    class = "rill_store"
  )
  for (zone in c("UTC", "America/New_York", "Asia/Tokyo")) {
    DBI::dbGetQuery(
      connection,
      "SELECT set_config('TimeZone', $1, false)",
      params = list(zone)
    )
    testthat::expect_identical(
      preparation_candidates(other, "reader", now = now),
      ids[c(3L, 2L)],
      info = zone
    )
  }
})

testthat::test_that("PostgreSQL persists backoff and filters active public candidates", {
  store <- local_orientation_backend_store("postgres", "reader")
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$entry_id <- "background-failure"
  entry$external_id <- "background-failure"
  entry$url <- "https://example.org/background-failure"
  entry$published_at <- utc_now()
  store_upsert_entries(store, as.data.frame(entry))
  testthat::expect_identical(
    preparation_candidates(store, "reader"),
    entry$entry_id
  )
  now <- Sys.time()
  token <- claim_preparation(store, entry$entry_id, now = now)
  failure <- list(
    reference = "safe-reference",
    code = "http_failed",
    http_status = 403L
  )
  finish_preparation(store, entry$entry_id, token, failure = failure, now = now)
  testthat::expect_identical(
    preparation_attempt(store, entry$entry_id)$failure[names(failure)],
    failure
  )
  testthat::expect_identical(
    preparation_candidates(store, "reader", now = now + 1),
    character()
  )
  testthat::expect_identical(
    preparation_candidates(store, "reader", now = now + 301),
    entry$entry_id
  )
  DBI::dbExecute(
    store$pool,
    "UPDATE readers SET status = 'disabled' WHERE reader_id = 'reader'"
  )
  testthat::expect_identical(
    preparation_candidates(store, "reader", now = now + 301),
    character()
  )
  testthat::expect_null(preparation_entry(store, entry$entry_id, "reader"))
})
