testthat::test_that("PostgreSQL isolates Reader Libraries over shared Feeds", {
  database_url <- Sys.getenv("RILL_TEST_DATABASE_URL", unset = "")
  testthat::skip_if(
    !nzchar(database_url),
    "RILL_TEST_DATABASE_URL is not configured"
  )

  connection_args <- postgres_connection_args(database_url)
  admin <- do.call(
    DBI::dbConnect,
    c(list(drv = RPostgres::Postgres()), connection_args)
  )
  schema_name <- paste0(
    "rill_reader_library_",
    substr(rill_id(Sys.getpid(), Sys.time(), stats::runif(1)), 1L, 16L)
  )
  schema_identifier <- DBI::dbQuoteIdentifier(admin, schema_name)
  DBI::dbExecute(admin, paste("CREATE SCHEMA", schema_identifier))
  withr::defer({
    DBI::dbExecute(
      admin,
      paste("DROP SCHEMA", schema_identifier, "CASCADE")
    )
    DBI::dbDisconnect(admin)
  })

  connection_args$options <- paste0("-csearch_path=", schema_name)
  database_pool <- do.call(
    pool::dbPool,
    c(
      list(drv = RPostgres::Postgres()),
      connection_args,
      list(minSize = 1, maxSize = 2, idleTimeout = 60)
    )
  )
  store <- structure(
    list(
      mode = "postgres",
      pool = database_pool,
      private_reader_id = "reader-one"
    ),
    class = "rill_store"
  )
  withr::defer(rill_store_close(store))
  store_apply_schema(store)
  store_ensure_reader(store, "reader-two")

  sample <- sample_rill_data()
  shared_feed <- as.list(sample$feeds[1L, , drop = FALSE])
  private_feed <- as.list(sample$feeds[2L, , drop = FALSE])
  store_upsert_feed(store, shared_feed)
  store_upsert_feed(store, private_feed)
  store_upsert_entries(
    store,
    sample$entries[
      sample$entries$feed_id %in%
        c(
          shared_feed$feed_id,
          private_feed$feed_id
        ),
      ,
      drop = FALSE
    ]
  )
  store_subscribe_feed(
    store,
    "reader-one",
    shared_feed$feed_id,
    folder = "Research"
  )
  store_subscribe_feed(
    store,
    "reader-two",
    shared_feed$feed_id,
    folder = "Morning"
  )
  store_subscribe_feed(store, "reader-one", private_feed$feed_id)
  store_rename_feed(store, "reader-one", shared_feed$feed_id, "Work reading")

  library_one <- store_list_feeds(store, "reader-one")
  library_two <- store_list_feeds(store, "reader-two")
  testthat::expect_setequal(
    library_one$feed_id,
    c(shared_feed$feed_id, private_feed$feed_id)
  )
  testthat::expect_identical(library_two$feed_id, shared_feed$feed_id)
  testthat::expect_identical(library_two$folder, "Morning")
  testthat::expect_identical(library_two$title, library_two$source_title)

  entry_id <- sample$entries$entry_id[
    sample$entries$feed_id == shared_feed$feed_id
  ][[1L]]
  store_mark_opened(store, "reader-one", entry_id)
  testthat::expect_disjoint(
    store_list_entries(store, "reader-one", view = "unread")$entry_id,
    entry_id
  )
  testthat::expect_in(
    entry_id,
    store_list_entries(store, "reader-two", view = "unread")$entry_id
  )

  forbidden_entry <- sample$entries$entry_id[
    sample$entries$feed_id == private_feed$feed_id
  ][[1L]]
  subscription_constraint <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT pg_get_constraintdef(oid) AS definition",
      "FROM pg_constraint",
      "WHERE conname = 'entry_state_subscription_fk'"
    )
  )
  testthat::expect_match(
    subscription_constraint$definition,
    "FOREIGN KEY (reader_id, feed_id)",
    fixed = TRUE
  )
  testthat::expect_error(
    store_mark_opened(store, "reader-two", forbidden_entry),
    class = "rill_entry_forbidden"
  )

  store_unsubscribe_feed(store, "reader-one", shared_feed$feed_id)
  testthat::expect_in(
    shared_feed$feed_id,
    store_list_active_feeds(store)$feed_id
  )
  testthat::expect_null(store_get_entry(store, "reader-one", entry_id))
  store_subscribe_feed(store, "reader-one", shared_feed$feed_id)
  restored <- store_list_feeds(store, "reader-one")
  restored <- restored[
    restored$feed_id == shared_feed$feed_id,
    ,
    drop = FALSE
  ]
  testthat::expect_identical(restored$folder, "Research")
  testthat::expect_identical(restored$title, "Work reading")
  testthat::expect_disjoint(
    store_list_entries(store, "reader-one", view = "unread")$entry_id,
    entry_id
  )
})
