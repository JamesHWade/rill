testthat::test_that("fresh connections preserve two Readers and isolate session revocation", {
  open_store <- local_reader_recovery_database()
  store <- open_store()
  store_apply_schema(store)
  config <- list(
    identity_mode = "oidc_proxy",
    actor_id = "reader-one",
    oidc_issuer = "https://reader.us.auth0.com/",
    allowed_oidc_subjects = "github|one"
  )
  adapter <- reader_identity_adapter(config, store)
  store_admit_reader_identity(
    store,
    config$oidc_issuer,
    "google-oauth2|two",
    "reader-two",
    responsible_id = "operator:test",
    reason = "isolation fixture"
  )
  sample <- sample_rill_data()
  feed <- as.list(sample$feeds[1L, , drop = FALSE])
  entry <- sample$entries[
    sample$entries$feed_id == feed$feed_id,
    ,
    drop = FALSE
  ][1L, , drop = FALSE]
  entry_id <- entry$entry_id[[1L]]
  store_upsert_feed(store, feed)
  store_upsert_entries(store, entry)
  store_subscribe_feed(store, "reader-one", feed$feed_id, folder = "Research")
  store_subscribe_feed(store, "reader-two", feed$feed_id, folder = "Morning")
  store_rename_feed(store, "reader-one", feed$feed_id, "Work reading")
  public_document <- document_fallback(as.list(entry), reason = "fixture")
  store_save_document(store, public_document)
  captured <- capture_document(
    store,
    capture_test_payload(
      source_url = entry$url[[1L]],
      canonical_url = entry$url[[1L]]
    ),
    "reader-one"
  )
  store_mark_opened(store, "reader-one", entry_id)
  store_toggle_state(store, "reader-two", entry_id, "saved")
  store_set_capture_credential(store, "reader-one", "reader-one-token")
  store_set_capture_credential(store, "reader-two", "reader-two-token")
  events_before <- DBI::dbGetQuery(
    store$pool,
    "SELECT * FROM events ORDER BY event_id"
  )
  testthat::expect_gt(nrow(events_before), 0L)
  testthat::expect_identical(unique(events_before$reader_id), "reader-one")
  rill_store_close(store)

  one <- open_store()
  two <- open_store()
  adapter_one <- reader_identity_adapter(config, one)
  adapter_two <- reader_identity_adapter(config, two)
  resolution_one <- reader_identity_resolve(
    adapter_one,
    identity_test_request("github|one")
  )
  resolution_two <- reader_identity_resolve(
    adapter_two,
    identity_test_request("google-oauth2|two")
  )
  testthat::expect_identical(resolution_one$reader_id, "reader-one")
  testthat::expect_identical(resolution_two$reader_id, "reader-two")
  testthat::expect_identical(
    store_list_feeds(one, "reader-one")$folder,
    "Research"
  )
  testthat::expect_identical(
    store_list_feeds(one, "reader-one")$title,
    "Work reading"
  )
  testthat::expect_identical(
    store_list_feeds(two, "reader-two")$folder,
    "Morning"
  )
  testthat::expect_identical(
    store_list_feeds(two, "reader-two")$title,
    feed$title
  )
  testthat::expect_length(
    store_list_entries(one, "reader-one", view = "unread")$entry_id,
    0L
  )
  testthat::expect_identical(
    store_list_entries(two, "reader-two", view = "unread")$entry_id,
    entry_id
  )
  testthat::expect_identical(
    store_get_entry(two, "reader-two", entry_id)$saved,
    TRUE
  )
  testthat::expect_identical(
    store_get_entry(one, "reader-one", entry_id)$saved,
    FALSE
  )
  testthat::expect_identical(
    store_get_document(one, "reader-one", entry_id)$document_id,
    captured$document_id
  )
  testthat::expect_identical(
    store_get_document(two, "reader-two", entry_id)$document_id,
    public_document$document_id
  )
  testthat::expect_null(store_get_document_by_id(
    two,
    "reader-two",
    captured$document_id
  ))
  testthat::expect_error(
    store_select_document(two, "reader-two", captured$document_id),
    class = "rill_document_forbidden"
  )
  testthat::expect_equal(
    DBI::dbGetQuery(two$pool, "SELECT * FROM events ORDER BY event_id"),
    events_before
  )
  testthat::expect_identical(
    store_resolve_capture_reader(one, "reader-one-token")$reader_id,
    "reader-one"
  )
  testthat::expect_identical(
    store_resolve_capture_reader(two, "reader-two-token")$reader_id,
    "reader-two"
  )

  session_one <- shiny::MockShinySession$new()
  session_two <- shiny::MockShinySession$new()
  withr::defer(session_one$close())
  withr::defer(session_two$close())
  reader_identity_guard_session(adapter_one, resolution_one, session_one)
  reader_identity_guard_session(adapter_two, resolution_two, session_two)
  session_one$flushReact()
  session_two$flushReact()
  testthat::expect_identical(session_one$isClosed(), FALSE)
  testthat::expect_identical(session_two$isClosed(), FALSE)

  store_disable_reader(one, "reader-one", "operator:test", "isolation fixture")
  session_one$elapse(rill_session_poll_interval_ms)
  session_two$elapse(rill_session_poll_interval_ms)
  testthat::expect_identical(session_one$isClosed(), TRUE)
  testthat::expect_identical(session_two$isClosed(), FALSE)
  testthat::expect_identical(
    reader_identity_resolve(
      adapter_one,
      identity_test_request("github|one")
    )$status,
    "disabled"
  )
  testthat::expect_identical(
    adapter_two$session_status(resolution_two)$status,
    "active"
  )
  testthat::expect_null(
    store_resolve_capture_reader(one, "reader-one-token")$reader_id
  )
  testthat::expect_identical(
    store_resolve_capture_reader(two, "reader-two-token")$reader_id,
    "reader-two"
  )
  testthat::expect_identical(
    store_get_document(two, "reader-two", entry_id)$document_id,
    public_document$document_id
  )
  testthat::expect_identical(store_list_active_feeds(two)$feed_id, feed$feed_id)
})
