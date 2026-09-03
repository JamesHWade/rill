testthat::test_that("memory satisfies the shared Orientation behavior contract", {
  reader_id <- "shared-orientation-reader"
  store <- local_orientation_backend_store("memory", reader_id)

  expect_orientation_backend_contract(store, reader_id)
})

testthat::test_that("PostgreSQL satisfies the shared Orientation behavior contract", {
  reader_id <- "shared-orientation-reader"
  store <- local_orientation_backend_store("postgres", reader_id)

  expect_orientation_backend_contract(store, reader_id)
})

testthat::test_that("memory preserves a deferred Reader question", {
  reader_id <- "memory-deferred-question-reader"
  store <- local_orientation_backend_store("memory", reader_id)

  expect_deferred_reader_question_contract(store, reader_id)
})

testthat::test_that("PostgreSQL preserves a deferred Reader question", {
  reader_id <- "postgres-deferred-question-reader"
  store <- local_orientation_backend_store("postgres", reader_id)

  expect_deferred_reader_question_contract(store, reader_id)
})

testthat::test_that("memory rolls back a failed Orientation selection event", {
  reader_id <- "memory-selection-rollback-reader"
  store <- local_orientation_backend_store("memory", reader_id)
  testthat::local_mocked_bindings(
    store_record_event = function(...) {
      cli::cli_abort(
        "Forced Reading History failure.",
        class = "rill_test_event_failure"
      )
    }
  )

  expect_orientation_selection_rollback(store, reader_id)
})

testthat::test_that("PostgreSQL rolls back a failed Orientation selection event", {
  reader_id <- "postgres-selection-rollback-reader"
  store <- local_orientation_backend_store("postgres", reader_id)
  testthat::local_mocked_bindings(
    store_record_event = function(...) {
      cli::cli_abort(
        "Forced Reading History failure.",
        class = "rill_test_event_failure"
      )
    }
  )

  expect_orientation_selection_rollback(store, reader_id)
})

testthat::test_that("memory rejects an occupied Orientation event ID", {
  reader_id <- "memory-selection-conflict-reader"
  store <- local_orientation_backend_store("memory", reader_id)

  expect_orientation_selection_event_conflict(store, reader_id)
})

testthat::test_that("PostgreSQL rejects an occupied Orientation event ID", {
  reader_id <- "postgres-selection-conflict-reader"
  store <- local_orientation_backend_store("postgres", reader_id)

  expect_orientation_selection_event_conflict(store, reader_id)
})
