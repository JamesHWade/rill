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

  state_before_revocation <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT read_at, read_reason, starred, saved, last_opened_at",
      "FROM entry_state WHERE reader_id = $1 AND entry_id = $2"
    ),
    params = list("reader-one", entry_id)
  )
  blocker_args <- connection_args
  blocker_args$options <- paste0("-csearch_path=", schema_name)
  blocker <- do.call(
    DBI::dbConnect,
    c(list(drv = RPostgres::Postgres()), blocker_args)
  )
  blocker_active <- TRUE
  withr::defer({
    if (blocker_active) {
      try(DBI::dbRollback(blocker), silent = TRUE)
    }
    DBI::dbDisconnect(blocker)
  })
  DBI::dbBegin(blocker)
  DBI::dbExecute(
    blocker,
    paste(
      "UPDATE subscriptions SET status = 'inactive'",
      "WHERE reader_id = $1 AND feed_id = $2"
    ),
    params = list("reader-one", shared_feed$feed_id)
  )

  package_path <- normalizePath(
    testthat::test_path("..", ".."),
    mustWork = FALSE
  )
  if (!file.exists(file.path(package_path, "DESCRIPTION"))) {
    package_path <- NULL
  }
  operations <- c("open", "unread", "save", "event", "bulk")
  application_names <- paste0(
    "rill_library_revocation_",
    Sys.getpid(),
    "_",
    operations
  )
  mutate_entry <- function(
    package_path,
    connection_args,
    schema_name,
    application_name,
    operation,
    entry_id,
    feed_id
  ) {
    if (is.null(package_path)) {
      loadNamespace("rill")
    } else {
      pkgload::load_all(package_path, quiet = TRUE)
    }
    connection_args$options <- paste0("-csearch_path=", schema_name)
    connection_args$application_name <- application_name
    database_pool <- do.call(
      pool::dbPool,
      c(
        list(drv = RPostgres::Postgres()),
        connection_args,
        list(minSize = 1, maxSize = 1, idleTimeout = 60)
      )
    )
    on.exit(pool::poolClose(database_pool), add = TRUE)
    worker_store <- structure(
      list(mode = "postgres", pool = database_pool),
      class = "rill_store"
    )
    tryCatch(
      {
        value <- switch(
          operation,
          open = rill:::store_mark_opened(
            worker_store,
            "reader-one",
            entry_id,
            as.POSIXct("2099-09-03 12:00:00", tz = "UTC")
          ),
          unread = rill:::store_mark_unread(
            worker_store,
            "reader-one",
            entry_id
          ),
          save = rill:::store_toggle_state(
            worker_store,
            "reader-one",
            entry_id,
            "saved"
          ),
          event = rill:::store_record_event(
            worker_store,
            list(
              event_id = "revoked-reader-event",
              reader_id = "reader-one",
              entry_id = entry_id,
              session_id = "revocation-race",
              event_type = "entry_saved",
              happened_at = as.POSIXct(
                "2099-09-03 12:00:00",
                tz = "UTC"
              ),
              surface = "test",
              position = 1L,
              payload = list()
            )
          ),
          bulk = rill:::store_mark_entries_read(
            worker_store,
            "reader-one",
            feed_id = feed_id,
            reason = "bulk_all"
          )
        )
        list(ok = TRUE, classes = character(), value = value)
      },
      error = function(error) {
        list(ok = FALSE, classes = class(error), value = NULL)
      }
    )
  }
  workers <- Map(
    function(application_name, operation) {
      callr::r_bg(
        mutate_entry,
        args = list(
          package_path = package_path,
          connection_args = connection_args,
          schema_name = schema_name,
          application_name = application_name,
          operation = operation,
          entry_id = entry_id,
          feed_id = shared_feed$feed_id
        ),
        stdout = "|",
        stderr = "|",
        supervise = TRUE
      )
    },
    application_names,
    operations
  )
  withr::defer({
    for (worker in workers) {
      if (worker$is_alive()) {
        worker$kill_tree()
      }
    }
  })

  blocked <- character()
  deadline <- Sys.time() + 20
  while (Sys.time() < deadline) {
    activity <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT application_name, wait_event_type FROM pg_stat_activity",
        "WHERE application_name IN ($1, $2, $3, $4, $5)"
      ),
      params = as.list(application_names)
    )
    blocked <- activity$application_name[
      activity$wait_event_type == "Lock"
    ]
    if (setequal(blocked, application_names)) {
      break
    }
    Sys.sleep(0.05)
  }
  testthat::expect_setequal(blocked, application_names)

  DBI::dbCommit(blocker)
  blocker_active <- FALSE
  results <- lapply(workers, function(worker) {
    worker$wait(timeout = 20000)
    testthat::expect_identical(worker$is_alive(), FALSE)
    worker$get_result()
  })
  workers <- list()
  names(results) <- operations
  testthat::expect_all_equal(
    vapply(
      results[c("open", "unread", "save", "event")],
      `[[`,
      logical(1),
      "ok"
    ),
    FALSE
  )
  testthat::expect_all_true(vapply(
    results[c("open", "unread", "save", "event")],
    \(result) "rill_entry_forbidden" %in% result$classes,
    logical(1)
  ))
  testthat::expect_identical(results$bulk$ok, TRUE)
  testthat::expect_length(results$bulk$value, 0L)

  state_after_revocation <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT read_at, read_reason, starred, saved, last_opened_at",
      "FROM entry_state WHERE reader_id = $1 AND entry_id = $2"
    ),
    params = list("reader-one", entry_id)
  )
  testthat::expect_equal(state_after_revocation, state_before_revocation)
  events_after_revocation <- DBI::dbGetQuery(
    store$pool,
    "SELECT event_id FROM events WHERE event_id = 'revoked-reader-event'"
  )
  testthat::expect_equal(nrow(events_after_revocation), 0L)
})
