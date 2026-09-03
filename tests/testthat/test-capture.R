capture_test_payload <- function(...) {
  utils::modifyList(
    list(
      capture_id = "browser-capture-001",
      source_url = "https://example.com/notes/one",
      canonical_url = "https://example.com/notes/one",
      title = "A locally captured document",
      author = "Ada Lovelace",
      site = "Example Notes",
      published_at = "2026-08-29T12:30:00Z",
      markdown = "# A local document\n\nSource-grounded text.",
      captured_at = "2026-08-30T22:15:00Z",
      producer = "rill-test-clipper",
      producer_version = "1.0.0",
      metadata = list(selection = "article")
    ),
    list(...)
  )
}

capture_test_request <- function(
  payload,
  token = "test-secret",
  path = capture_endpoint_path,
  method = "POST",
  content_type = "application/json"
) {
  body <- if (is.character(payload)) {
    payload
  } else {
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  }
  list2env(
    list(
      PATH_INFO = path,
      REQUEST_METHOD = method,
      CONTENT_TYPE = content_type,
      HTTP_AUTHORIZATION = paste("Bearer", token),
      rook.input = list(read = function() charToRaw(body))
    ),
    parent = emptyenv()
  )
}

testthat::test_that("browser capture reaches the normal document boundary", {
  store <- rill_store(list(demo_mode = TRUE))
  result <- capture_document(
    store,
    capture_test_payload(),
    "reader",
    received_at = "2026-08-30 22:16:00 UTC"
  )

  entry <- store_get_entry(store, "reader", result$entry_id)
  document <- store_get_document(store, result$entry_id)
  documents <- store_list_documents(store, result$entry_id)
  capture_sources <- store_list_feeds(store, "reader", source_kind = "capture")
  subscriptions <- store_list_feeds(
    store,
    "reader",
    source_kind = "subscription"
  )

  testthat::expect_identical(result$created, TRUE)
  testthat::expect_identical(entry$title, "A locally captured document")
  testthat::expect_s3_class(document, "rill_document")
  testthat::expect_identical(document$document_id, result$document_id)
  testthat::expect_identical(document$acquisition_method, "browser_capture")
  testthat::expect_identical(document$producer, "rill-test-clipper")
  testthat::expect_identical(document$producer_record_id, "browser-capture-001")
  testthat::expect_identical(document$provenance$captured_by, "reader")
  testthat::expect_identical(
    document$provenance$producer_metadata$selection,
    "article"
  )
  testthat::expect_named(documents, result$entry_id)
  testthat::expect_identical(capture_sources$title, "Local captures")
  testthat::expect_equal(nrow(subscriptions), 3L)
  testthat::expect_match(subscriptions$feed_url, "^https?://", all = TRUE)
  testthat::expect_in(
    result$entry_id,
    store_list_entries(
      store,
      "reader",
      "unread"
    )$entry_id
  )
})

testthat::test_that("capture sources are not polled as subscriptions", {
  store <- rill_store(list(demo_mode = TRUE))
  capture_document(store, capture_test_payload(), "reader")
  store_ensure_reader(store, "other-reader")
  store_subscribe_feed(
    store,
    "other-reader",
    store$memory$feeds$feed_id[[1L]]
  )
  polled <- character()
  testthat::local_mocked_bindings(
    refresh_feed = function(store, feed) {
      polled <<- c(polled, feed$source_kind)
      list(feed_id = feed$feed_id, added = 0L, not_modified = TRUE)
    },
    .package = "rill"
  )

  results <- refresh_all_feeds(store)

  testthat::expect_length(results, 3L)
  testthat::expect_identical(polled, rep("subscription", 3L))
})

testthat::test_that("capture sources cannot be unsubscribed as Feeds", {
  store <- rill_store(list(demo_mode = TRUE))
  result <- capture_document(store, capture_test_payload(), "reader")
  capture_feed_id <- store$memory$entries$feed_id[
    store$memory$entries$entry_id == result$entry_id
  ][[1L]]

  testthat::expect_error(
    store_unsubscribe_feed(store, "reader", capture_feed_id),
    class = "rill_subscription_source_invalid"
  )
  testthat::expect_in(
    result$entry_id,
    store_list_entries(store, "reader", view = "all")$entry_id
  )
})

testthat::test_that("capture uses an existing feed entry for the same URL", {
  store <- rill_store(list(demo_mode = TRUE))
  entry <- store$memory$entries[1, , drop = FALSE]
  payload <- capture_test_payload(
    capture_id = "existing-entry-capture",
    source_url = entry$url[[1]],
    canonical_url = entry$url[[1]],
    title = entry$title[[1]]
  )

  result <- capture_document(store, payload, "reader")
  current <- store_get_document(store, entry$entry_id[[1]])

  testthat::expect_identical(result$entry_id, entry$entry_id[[1]])
  testthat::expect_equal(nrow(store$memory$entries), 6L)
  testthat::expect_identical(current$document_id, result$document_id)
  testthat::expect_identical(current$markdown, payload$markdown)
})

testthat::test_that("capture retries are idempotent", {
  store <- rill_store(list(demo_mode = TRUE))
  payload <- capture_test_payload()

  first <- capture_document(store, payload, "reader")
  second <- capture_document(store, payload, "reader")

  testthat::expect_identical(first$created, TRUE)
  testthat::expect_identical(second$created, FALSE)
  testthat::expect_identical(second$document_id, first$document_id)
  testthat::expect_equal(
    sum(store$memory$events$event_type == "document_captured"),
    1L
  )
})

testthat::test_that("capture IDs cannot be reused for different evidence", {
  store <- rill_store(list(demo_mode = TRUE))
  capture_document(store, capture_test_payload(), "reader")

  testthat::expect_snapshot(
    capture_document(
      store,
      capture_test_payload(markdown = "Different evidence."),
      "reader"
    ),
    error = TRUE
  )
})

testthat::test_that("the HTTP endpoint authenticates and reports replays", {
  store <- rill_store(list(demo_mode = TRUE))
  store_ensure_reader(store, "reader")
  base_calls <- 0L
  base_handler <- function(request) {
    base_calls <<- base_calls + 1L
    list(status = 299L, headers = list(), body = "base")
  }
  handler <- capture_http_handler(
    base_handler,
    store,
    list(actor_id = "reader", capture_token = "test-secret")
  )

  unauthorized <- handler(capture_test_request(
    capture_test_payload(),
    token = "wrong-secret"
  ))
  created <- handler(capture_test_request(capture_test_payload()))
  replayed <- handler(capture_test_request(capture_test_payload()))
  conflict <- handler(capture_test_request(capture_test_payload(
    markdown = "Different evidence."
  )))
  preflight <- handler(capture_test_request(
    capture_test_payload(),
    method = "OPTIONS"
  ))
  passthrough <- handler(capture_test_request(
    capture_test_payload(),
    path = "/"
  ))

  created_body <- jsonlite::fromJSON(created$body)
  replayed_body <- jsonlite::fromJSON(replayed$body)
  testthat::expect_identical(unauthorized$status, 401L)
  testthat::expect_identical(created$status, 201L)
  testthat::expect_identical(created_body$status, "created")
  testthat::expect_identical(replayed$status, 200L)
  testthat::expect_identical(replayed_body$status, "replayed")
  testthat::expect_identical(
    replayed_body$document_id,
    created_body$document_id
  )
  testthat::expect_identical(conflict$status, 409L)
  testthat::expect_identical(preflight$status, 204L)
  testthat::expect_identical(preflight$body, raw())
  testthat::expect_identical(passthrough$status, 299L)
  testthat::expect_identical(base_calls, 1L)
})

testthat::test_that("invalid capture input returns a client error", {
  store <- rill_store(list(demo_mode = TRUE))
  store_ensure_reader(store, "reader")
  handler <- capture_http_handler(
    function(request) NULL,
    store,
    list(actor_id = "reader", capture_token = "test-secret")
  )
  payload <- capture_test_payload(source_url = "http://127.0.0.1/private")

  response <- handler(capture_test_request(payload))

  testthat::expect_identical(response$status, 422L)
  testthat::expect_match(
    jsonlite::fromJSON(response$body)$error,
    "public HTTP or HTTPS URL"
  )
})

testthat::test_that("the HTTP endpoint denies a disabled Reader", {
  store <- rill_store(list(demo_mode = TRUE))
  store_ensure_reader(store, "reader")
  store_disable_reader(
    store,
    "reader",
    responsible_id = "operator:james",
    reason = "access revoked"
  )
  handler <- capture_http_handler(
    function(request) NULL,
    store,
    list(actor_id = "reader", capture_token = "test-secret")
  )
  response <- handler(capture_test_request(capture_test_payload()))

  testthat::expect_identical(response$status, 403L)
  testthat::expect_identical(
    jsonlite::fromJSON(response$body)$error,
    "Capture is disabled for this Reader."
  )
})
