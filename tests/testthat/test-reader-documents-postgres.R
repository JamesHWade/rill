testthat::test_that("PostgreSQL isolates private Documents and reading-copy selection", {
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
    "rill_reader_documents_",
    substr(rill_id(Sys.getpid(), Sys.time(), stats::runif(1)), 1L, 16L)
  )
  schema_identifier <- DBI::dbQuoteIdentifier(admin, schema_name)
  DBI::dbExecute(admin, paste("CREATE SCHEMA", schema_identifier))
  withr::defer({
    DBI::dbExecute(admin, paste("DROP SCHEMA", schema_identifier, "CASCADE"))
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
  feed <- as.list(sample$feeds[1L, , drop = FALSE])
  entry <- sample$entries[
    sample$entries$feed_id == feed$feed_id,
    ,
    drop = FALSE
  ][1L, , drop = FALSE]
  store_upsert_feed(store, feed)
  store_upsert_entries(store, entry)
  store_subscribe_feed(store, "reader-one", feed$feed_id)
  store_subscribe_feed(store, "reader-two", feed$feed_id)
  public_document <- sample$documents[[match(
    entry$entry_id[[1L]],
    vapply(sample$documents, `[[`, character(1), "entry_id")
  )]]
  store_save_document(store, public_document)

  captured <- capture_document(
    store,
    capture_test_payload(
      source_url = entry$url[[1L]],
      canonical_url = entry$url[[1L]],
      title = entry$title[[1L]]
    ),
    "reader-one"
  )
  newer_public <- document_fallback(
    as.list(entry),
    reason = "new-public-copy"
  )
  store_save_document(store, newer_public)

  testthat::expect_identical(
    store_get_document(
      store,
      "reader-one",
      entry$entry_id[[1L]]
    )$document_id,
    captured$document_id
  )
  testthat::expect_identical(
    store_get_document(
      store,
      "reader-two",
      entry$entry_id[[1L]]
    )$document_id,
    newer_public$document_id
  )
  testthat::expect_null(
    store_get_document_by_id(store, "reader-two", captured$document_id)
  )
  testthat::expect_error(
    store_select_document(store, "reader-two", captured$document_id),
    class = "rill_document_forbidden"
  )
  ownership_constraint <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT pg_get_constraintdef(oid) AS definition FROM pg_constraint",
      "WHERE conrelid = 'reader_document_selections'::regclass",
      "AND contype = 'c'"
    )
  )
  testthat::expect_match(
    ownership_constraint$definition,
    "ownership_key = ('reader:'::text || reader_id)",
    fixed = TRUE
  )

  standalone_one <- capture_document(
    store,
    capture_test_payload(
      capture_id = "standalone-one",
      source_url = "https://example.com/private-note",
      canonical_url = "https://example.com/private-note"
    ),
    "reader-one"
  )
  standalone_two <- capture_document(
    store,
    capture_test_payload(
      capture_id = "standalone-two",
      source_url = "https://example.com/private-note",
      canonical_url = "https://example.com/private-note",
      markdown = "Reader two's private note."
    ),
    "reader-two"
  )
  testthat::expect_length(
    unique(c(standalone_one$entry_id, standalone_two$entry_id)),
    2L
  )
  testthat::expect_disjoint(
    store_list_entries(store, "reader-two", view = "all")$entry_id,
    standalone_one$entry_id
  )
  testthat::expect_error(
    store_mark_opened(store, "reader-two", standalone_one$entry_id),
    class = "rill_entry_forbidden"
  )

  store_set_capture_credential(store, "reader-one", "reader-one-token")
  store_set_capture_credential(store, "reader-two", "reader-two-token")
  testthat::expect_identical(
    store_resolve_capture_reader(store, "reader-one-token")$reader_id,
    "reader-one"
  )
  testthat::expect_identical(
    store_resolve_capture_reader(store, "reader-two-token")$reader_id,
    "reader-two"
  )
  testthat::expect_error(
    store_set_capture_credential(store, "reader-two", "reader-one-token"),
    class = "rill_capture_credential_conflict"
  )
  credential <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT token_hash FROM reader_capture_credentials",
      "WHERE reader_id = 'reader-one'"
    )
  )
  testthat::expect_no_match(credential$token_hash, "reader-one-token")
})

testthat::test_that("the legacy reading-copy head migrates only to its Reader", {
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
    "rill_reader_document_migration_",
    substr(rill_id(Sys.getpid(), Sys.time(), stats::runif(1)), 1L, 12L)
  )
  schema_identifier <- DBI::dbQuoteIdentifier(admin, schema_name)
  DBI::dbExecute(admin, paste("CREATE SCHEMA", schema_identifier))
  withr::defer({
    DBI::dbExecute(admin, paste("DROP SCHEMA", schema_identifier, "CASCADE"))
    DBI::dbDisconnect(admin)
  })

  connection_args$options <- paste0("-csearch_path=", schema_name)
  connection <- do.call(
    DBI::dbConnect,
    c(list(drv = RPostgres::Postgres()), connection_args)
  )
  withr::defer(DBI::dbDisconnect(connection))
  migrations <- schema_migration_files()
  run_migration <- function(migration_id) {
    migration <- migrations[[match(
      migration_id,
      vapply(migrations, `[[`, character(1), "migration_id")
    )]]
    statements <- Filter(
      nzchar,
      trimws(strsplit(migration$sql, ";", fixed = TRUE)[[1]])
    )
    for (statement in statements) {
      DBI::dbExecute(connection, statement)
    }
  }

  DBI::dbBegin(connection)
  DBI::dbExecute(
    connection,
    "SELECT set_config('rill.legacy_reader_id', 'legacy-reader', true)"
  )
  for (migration_id in sprintf(
    "%03d_%s",
    1:8,
    c(
      "init",
      "agent_runs",
      "agent_run_question_kind",
      "orientations",
      "orientation_data_destination_settings",
      "deferred_reader_questions",
      "agent_run_response",
      "reader_identities"
    )
  )) {
    run_migration(migration_id)
  }
  DBI::dbExecute(
    connection,
    paste(
      "INSERT INTO feeds",
      "(feed_id, feed_url, title, folder, source_kind, poll_status)",
      "VALUES ('feed-1', 'https://example.com/feed', 'Feed', 'Inbox',",
      "'subscription', 'ok')"
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "INSERT INTO entries",
      "(entry_id, feed_id, external_id, url, title)",
      "VALUES ('entry-1', 'feed-1', 'entry-1',",
      "'https://example.com/entry', 'Entry')"
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "INSERT INTO documents",
      paste(
        "(document_id, entry_id, source_url, acquisition_method, producer,",
        "captured_at, markdown, word_count, content_hash, record_hash,",
        "provenance)"
      ),
      paste(
        "VALUES ('capture-1', 'entry-1', 'https://example.com/entry',",
        "'browser_capture', 'clipper', now(), 'Private copy', 2, 'content',",
        "'record', '{\"captured_by\":\"legacy-reader\"}'::jsonb)"
      )
    )
  )
  DBI::dbExecute(
    connection,
    paste(
      "INSERT INTO entry_document_heads (entry_id, document_id)",
      "VALUES ('entry-1', 'capture-1')"
    )
  )
  run_migration("009_reader_library")
  run_migration("010_reader_documents")
  DBI::dbCommit(connection)

  document <- DBI::dbGetQuery(
    connection,
    "SELECT reader_id FROM documents WHERE document_id = 'capture-1'"
  )
  selection <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT reader_id, document_id FROM reader_document_selections",
      "WHERE entry_id = 'entry-1'"
    )
  )
  public_head <- DBI::dbGetQuery(
    connection,
    "SELECT document_id FROM public_document_heads WHERE entry_id = 'entry-1'"
  )

  testthat::expect_identical(document$reader_id, "legacy-reader")
  testthat::expect_identical(selection$reader_id, "legacy-reader")
  testthat::expect_identical(selection$document_id, "capture-1")
  testthat::expect_identical(nrow(public_head), 0L)
})
