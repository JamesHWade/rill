store_apply_schema <- function(
  store,
  legacy_reader_id = store$private_reader_id %||% "reader"
) {
  if (!identical(store$mode, "postgres")) {
    return(invisible(NULL))
  }

  telemetry_local_span("store.migrate")
  if (!store_scalar_string(legacy_reader_id)) {
    cli::cli_abort(
      "The legacy Reader identifier must be a non-empty string.",
      class = "rill_schema_legacy_reader_invalid"
    )
  }
  pool::poolWithTransaction(store$pool, function(connection) {
    # Consume SELECT results so RPostgres does not cancel an unfinished query.
    DBI::dbGetQuery(
      connection,
      "SELECT pg_advisory_xact_lock(hashtext('rill:schema-migrations'))"
    )
    DBI::dbGetQuery(
      connection,
      "SELECT set_config('rill.legacy_reader_id', $1, true)",
      params = list(legacy_reader_id)
    )
    DBI::dbExecute(
      connection,
      paste(
        "CREATE TABLE IF NOT EXISTS schema_migrations (",
        "migration_id text PRIMARY KEY,",
        "checksum text NOT NULL,",
        "applied_at timestamptz NOT NULL DEFAULT now()",
        ")"
      )
    )

    migrations <- schema_migration_files()
    applied <- DBI::dbGetQuery(
      connection,
      paste(
        "SELECT migration_id, checksum FROM schema_migrations",
        "ORDER BY migration_id"
      )
    )
    known_ids <- vapply(migrations, `[[`, character(1), "migration_id")
    unknown_ids <- setdiff(applied$migration_id, known_ids)
    if (length(unknown_ids)) {
      cli::cli_abort(
        paste0(
          "The database has unknown schema migration",
          if (length(unknown_ids) == 1L) " " else "s ",
          paste(unknown_ids, collapse = ", "),
          "."
        ),
        class = "rill_schema_newer"
      )
    }

    for (migration in migrations) {
      applied_index <- match(migration$migration_id, applied$migration_id)
      if (!is.na(applied_index)) {
        if (!identical(applied$checksum[[applied_index]], migration$checksum)) {
          cli::cli_abort(
            "Schema migration {.val {migration$migration_id}} has changed.",
            class = "rill_schema_drift"
          )
        }
        next
      }

      if (
        identical(migration$migration_id, "001_init") &&
          store_has_domain_schema(connection)
      ) {
        store_adopt_initial_release_schema(connection)
        store_verify_baseline_schema(connection)
      } else {
        DBI::dbExecute(connection, migration$sql, immediate = TRUE)
      }

      DBI::dbExecute(
        connection,
        paste(
          "INSERT INTO schema_migrations (migration_id, checksum)",
          "VALUES ($1, $2)"
        ),
        params = list(migration$migration_id, migration$checksum)
      )
      applied <- rbind(
        applied,
        data.frame(
          migration_id = migration$migration_id,
          checksum = migration$checksum,
          stringsAsFactors = FALSE
        )
      )
    }
  })

  invisible(NULL)
}

schema_migration_files <- function() {
  paths <- list.files(
    rill_package_file("sql"),
    pattern = "^[0-9]{3}_[a-z0-9_]+[.]sql$",
    full.names = TRUE
  )
  paths <- sort(paths)
  lapply(paths, function(path) {
    sql <- paste(readLines(path, warn = FALSE), collapse = "\n")
    list(
      migration_id = sub("[.]sql$", "", basename(path)),
      checksum = digest::digest(sql, algo = "sha256", serialize = FALSE),
      sql = sql
    )
  })
}

store_has_domain_schema <- function(connection) {
  tables <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT table_name FROM information_schema.tables",
      "WHERE table_schema = current_schema()",
      "AND table_name <> 'schema_migrations'"
    )
  )
  all(c("feeds", "entries") %in% tables$table_name)
}

store_adopt_initial_release_schema <- function(connection) {
  legacy_tables <- c(
    "feeds",
    "entries",
    "article_documents",
    "documents",
    "entry_document_heads",
    "entry_state",
    "events"
  )
  tables <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT table_name FROM information_schema.tables",
      "WHERE table_schema = current_schema()"
    )
  )$table_name
  columns <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT column_name FROM information_schema.columns",
      "WHERE table_schema = current_schema()",
      "AND table_name = 'entry_state'"
    )
  )$column_name
  initial_release <- all(legacy_tables %in% tables) &&
    !"subscription_preferences" %in% tables &&
    !"read_reason" %in% columns
  if (!initial_release) {
    return(invisible(FALSE))
  }

  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE subscription_preferences (",
      "reader_id text NOT NULL,",
      paste(
        "feed_id text NOT NULL REFERENCES feeds(feed_id)",
        "ON DELETE CASCADE,"
      ),
      "display_title text,",
      "PRIMARY KEY (reader_id, feed_id)",
      ")"
    )
  )
  DBI::dbExecute(
    connection,
    "ALTER TABLE entry_state ADD COLUMN read_reason text"
  )
  DBI::dbExecute(
    connection,
    paste(
      "UPDATE entry_state SET read_reason = 'opened'",
      "WHERE read_at IS NOT NULL AND last_opened_at IS NOT NULL",
      "AND read_reason IS NULL"
    )
  )
  invisible(TRUE)
}

store_verify_baseline_schema <- function(connection) {
  required_tables <- c(
    "feeds",
    "entries",
    "article_documents",
    "documents",
    "entry_document_heads",
    "entry_state",
    "subscription_preferences",
    "events"
  )
  tables <- DBI::dbGetQuery(
    connection,
    paste(
      "SELECT table_name FROM information_schema.tables",
      "WHERE table_schema = current_schema()"
    )
  )
  missing <- setdiff(required_tables, tables$table_name)
  if (!"entry_state" %in% missing) {
    columns <- DBI::dbGetQuery(
      connection,
      paste(
        "SELECT column_name FROM information_schema.columns",
        "WHERE table_schema = current_schema()",
        "AND table_name = 'entry_state'"
      )
    )$column_name
    if (!"read_reason" %in% columns) {
      missing <- c(missing, "entry_state.read_reason")
    }
  }
  if (length(missing)) {
    cli::cli_abort(
      paste0(
        "The existing database is not compatible with the Rill baseline: ",
        "missing ",
        paste(missing, collapse = ", "),
        "."
      ),
      class = "rill_schema_incompatible"
    )
  }
  invisible(NULL)
}

store_migrate_legacy_feed_titles <- function(store, reader_id) {
  if (!identical(store$mode, "postgres")) {
    return(invisible(NULL))
  }

  legacy_column <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT EXISTS (",
      "SELECT 1 FROM information_schema.columns",
      "WHERE table_schema = current_schema()",
      "AND table_name = 'feeds' AND column_name = 'display_title'",
      ") AS present"
    )
  )
  if (!isTRUE(legacy_column$present[[1]])) {
    return(invisible(NULL))
  }

  pool::poolWithTransaction(store$pool, function(connection) {
    DBI::dbExecute(
      connection,
      paste(
        "INSERT INTO subscription_preferences",
        "(reader_id, feed_id, display_title)",
        "SELECT $1, feed_id, display_title FROM feeds",
        "WHERE display_title IS NOT NULL AND display_title <> ''",
        "ON CONFLICT (reader_id, feed_id) DO NOTHING"
      ),
      params = list(reader_id)
    )
    DBI::dbExecute(
      connection,
      paste(
        "UPDATE subscriptions s SET display_title = f.display_title,",
        "updated_at = now() FROM feeds f",
        "WHERE s.reader_id = $1 AND s.feed_id = f.feed_id",
        "AND f.display_title IS NOT NULL AND f.display_title <> ''"
      ),
      params = list(reader_id)
    )
    DBI::dbExecute(
      connection,
      "UPDATE feeds SET display_title = NULL WHERE display_title IS NOT NULL"
    )
  })
  invisible(NULL)
}
