testthat::test_that("the memory store tracks reading state", {
  store <- rill_store(list(demo_mode = TRUE))
  actor_id <- "test-reader"
  entry_id <- store$memory$entries$entry_id[[1]]

  unread <- store_list_entries(store, actor_id, view = "unread")
  testthat::expect_equal(nrow(unread), 6L)

  store_mark_opened(store, actor_id, entry_id)
  unread <- store_list_entries(store, actor_id, view = "unread")
  testthat::expect_equal(nrow(unread), 5L)

  testthat::expect_identical(
    store_toggle_state(store, actor_id, entry_id, "starred"),
    TRUE
  )
  starred <- store_list_entries(store, actor_id, view = "starred")
  testthat::expect_identical(starred$entry_id, entry_id)
})

testthat::test_that("PostgreSQL URLs become explicit connection arguments", {
  args <- postgres_connection_args(paste0(
    "postgresql://reader:p%40ss@db.example:5433/rill?",
    "sslmode=require&channel_binding=require"
  ))

  testthat::expect_identical(args$dbname, "rill")
  testthat::expect_identical(args$host, "db.example")
  testthat::expect_identical(args$port, "5433")
  testthat::expect_identical(args$user, "reader")
  testthat::expect_identical(args$password, "p@ss")
  testthat::expect_identical(args$sslmode, "require")
  testthat::expect_identical(args$channel_binding, "require")
})

testthat::test_that("state fields are validated", {
  store <- rill_store(list(demo_mode = TRUE))

  testthat::expect_snapshot(
    store_toggle_state(store, "reader", "entry", "archived"),
    error = TRUE
  )
})

testthat::test_that("the schema has an immutable document head boundary", {
  schema <- paste(
    readLines(rill_package_file("sql", "001_init.sql"), warn = FALSE),
    collapse = "\n"
  )

  testthat::expect_match(
    schema,
    "CREATE TABLE IF NOT EXISTS documents",
    fixed = TRUE
  )
  testthat::expect_match(
    schema,
    "CREATE TABLE IF NOT EXISTS entry_document_heads",
    fixed = TRUE
  )
  testthat::expect_match(schema, "record_hash text NOT NULL", fixed = TRUE)
})
