testthat::test_that("database startup consumes results without sending cancellation", {
  database_url <- local_startup_database_url()
  proxy <- local_postgres_cancel_proxy(database_url)
  config <- list(
    demo_mode = FALSE,
    database_url = proxy$url,
    actor_id = "startup-reader"
  )

  for (index in seq_len(3L)) {
    store <- rill_store(config)
    testthat::expect_identical(
      store_resolve_reader(store, "startup-reader")$status,
      "active"
    )
    testthat::expect_identical(
      DBI::dbGetQuery(store$pool, "SELECT 1 AS ready")$ready,
      1L
    )
    rill_store_close(store)
  }
  testthat::expect_identical(proxy$process$read_output_lines(), character())
})

testthat::test_that("a failed store initialization closes its database pool", {
  config <- list(
    demo_mode = FALSE,
    database_url = local_startup_database_url(),
    actor_id = "startup-reader"
  )
  failed_pool <- NULL
  testthat::local_mocked_bindings(
    store_apply_schema = function(store, ...) {
      failed_pool <<- store$pool
      cli::cli_abort("Schema drift fixture.", class = "rill_schema_drift")
    }
  )

  testthat::expect_error(rill_store(config), class = "rill_schema_drift")
  testthat::expect_identical(failed_pool$valid, FALSE)
})
