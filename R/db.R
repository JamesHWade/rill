postgres_connection_args <- function(database_url) {
  parsed <- tryCatch(
    httr2::url_parse(database_url),
    error = function(error) NULL
  )
  if (
    is.null(parsed) ||
      !tolower(parsed$scheme %||% "") %in% c("postgres", "postgresql") ||
      !nzchar(parsed$hostname %||% "")
  ) {
    cli::cli_abort(
      "{.envvar DATABASE_URL} must be a PostgreSQL connection URL."
    )
  }

  database <- sub("^/", "", parsed$path %||% "")
  if (!nzchar(database) || grepl("/", database, fixed = TRUE)) {
    cli::cli_abort(
      "{.envvar DATABASE_URL} must name exactly one PostgreSQL database."
    )
  }

  args <- list(
    dbname = database,
    host = parsed$hostname
  )
  for (name in c("port", "username", "password")) {
    value <- parsed[[name]] %||% ""
    if (nzchar(value)) {
      target <- if (identical(name, "username")) "user" else name
      args[[target]] <- value
    }
  }

  query <- parsed$query %||% list()
  reserved <- c(
    "drv",
    "minSize",
    "maxSize",
    "idleTimeout",
    "validationInterval",
    "validateQuery",
    "bigint",
    "check_interrupts",
    "timezone",
    "timezone_out"
  )
  if (length(intersect(names(query), reserved))) {
    cli::cli_abort(
      "{.envvar DATABASE_URL} contains a reserved connection parameter."
    )
  }
  for (name in names(query)) {
    value <- query[[name]]
    if (!nzchar(name) || !is.character(value) || length(value) != 1L) {
      cli::cli_abort(
        "{.envvar DATABASE_URL} contains an invalid connection parameter."
      )
    }
    args[[name]] <- value
  }
  args
}

rill_store <- function(config) {
  if (requireNamespace("otel", quietly = TRUE)) {
    try(
      otel::start_local_active_span(
        "store.open",
        attributes = list(
          "store.mode" = if (config$demo_mode) "memory" else "postgres"
        ),
        tracer = "rill",
        end_on_exit = TRUE
      ),
      silent = TRUE
    )
  }

  if (config$demo_mode) {
    sample <- sample_rill_data()
    memory <- new.env(parent = emptyenv())
    memory$feeds <- sample$feeds
    memory$entries <- sample$entries
    memory$documents <- sample$documents
    memory$document_heads <- stats::setNames(
      vapply(sample$documents, `[[`, character(1), "document_id"),
      vapply(sample$documents, `[[`, character(1), "entry_id")
    )
    memory$state <- data.frame(
      actor_id = character(),
      entry_id = character(),
      read_at = character(),
      read_reason = character(),
      starred = logical(),
      saved = logical(),
      hidden = logical(),
      last_opened_at = character(),
      stringsAsFactors = FALSE
    )
    memory$subscription_preferences <- data.frame(
      reader_id = character(),
      feed_id = character(),
      display_title = character(),
      stringsAsFactors = FALSE
    )
    memory$events <- data.frame(
      event_id = character(),
      actor_id = character(),
      entry_id = character(),
      session_id = character(),
      event_type = character(),
      happened_at = character(),
      surface = character(),
      position = integer(),
      payload = character(),
      stringsAsFactors = FALSE
    )
    memory$agent_runs <- list()
    return(structure(
      list(mode = "memory", memory = memory),
      class = "rill_store"
    ))
  }

  connection_args <- postgres_connection_args(config$database_url)
  database_pool <- tryCatch(
    do.call(
      pool::dbPool,
      c(
        list(drv = RPostgres::Postgres()),
        connection_args,
        list(minSize = 1, maxSize = 5, idleTimeout = 60)
      )
    ),
    error = function(error) {
      telemetry_log(
        "error",
        "store.connection_failed",
        list("error.type" = class(error)[[1]])
      )
      cli::cli_abort(
        c(
          "Can't connect to PostgreSQL.",
          "i" = paste(
            "Check the {.envvar DATABASE_URL} connection string and network",
            "access."
          )
        ),
        parent = error
      )
    }
  )

  store <- structure(
    list(mode = "postgres", pool = database_pool),
    class = "rill_store"
  )
  store_apply_schema(store)
  store_migrate_legacy_feed_titles(store, config$actor_id)
  store
}

rill_store_close <- function(store) {
  if (identical(store$mode, "postgres")) {
    pool::poolClose(store$pool)
  }
  invisible(NULL)
}

store_apply_schema <- function(store) {
  if (!identical(store$mode, "postgres")) {
    return(invisible(NULL))
  }

  if (requireNamespace("otel", quietly = TRUE)) {
    try(
      otel::start_local_active_span(
        "store.migrate",
        tracer = "rill",
        end_on_exit = TRUE
      ),
      silent = TRUE
    )
  }
  pool::poolWithTransaction(store$pool, function(connection) {
    DBI::dbExecute(
      connection,
      "SELECT pg_advisory_xact_lock(hashtext('rill:schema-migrations'))"
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
        statements <- Filter(
          nzchar,
          trimws(strsplit(migration$sql, ";", fixed = TRUE)[[1]])
        )
        for (statement in statements) {
          DBI::dbExecute(connection, statement)
        }
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
  nrow(tables) > 0L
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
      "UPDATE feeds SET display_title = NULL WHERE display_title IS NOT NULL"
    )
  })
  invisible(NULL)
}

store_list_feeds <- function(store, actor_id, source_kind = NULL) {
  if (identical(store$mode, "postgres")) {
    feeds <- DBI::dbGetQuery(
      store$pool,
      paste(
        paste(
          "SELECT f.feed_id, f.feed_url, f.site_url, f.title, f.folder,",
          "f.source_kind, f.etag, f.last_modified, f.poll_status,",
          "f.last_polled_at, f.created_at, p.display_title,"
        ),
        "COUNT(e.entry_id) FILTER (WHERE s.read_at IS NULL)::integer AS unread_count,",
        "COUNT(e.entry_id)::integer AS entry_count",
        "FROM feeds f",
        paste(
          "LEFT JOIN subscription_preferences p ON p.feed_id = f.feed_id",
          "AND p.reader_id = $1"
        ),
        "LEFT JOIN entries e ON e.feed_id = f.feed_id",
        "LEFT JOIN entry_state s ON s.entry_id = e.entry_id AND s.actor_id = $1",
        "GROUP BY f.feed_id, p.display_title",
        paste(
          "ORDER BY lower(f.folder),",
          "lower(COALESCE(NULLIF(p.display_title, ''), f.title))"
        )
      ),
      params = list(actor_id)
    )
    feeds <- resolve_feed_titles(feeds)
    if (!is.null(source_kind)) {
      feeds <- feeds[feeds$source_kind %in% source_kind, , drop = FALSE]
    }
    return(feeds)
  }

  feeds <- store$memory$feeds
  preferences <- store$memory$subscription_preferences
  preferences <- preferences[preferences$reader_id == actor_id, , drop = FALSE]
  feeds$display_title <- preferences$display_title[
    match(feeds$feed_id, preferences$feed_id)
  ]
  feeds <- resolve_feed_titles(feeds)
  feeds$unread_count <- vapply(
    feeds$feed_id,
    function(feed_id) {
      ids <- store$memory$entries$entry_id[
        store$memory$entries$feed_id == feed_id
      ]
      read_ids <- store$memory$state$entry_id[
        store$memory$state$actor_id == actor_id &
          !is.na(store$memory$state$read_at) &
          nzchar(store$memory$state$read_at)
      ]
      sum(!ids %in% read_ids)
    },
    integer(1)
  )
  feeds$entry_count <- vapply(
    feeds$feed_id,
    function(feed_id) sum(store$memory$entries$feed_id == feed_id),
    integer(1)
  )
  if (!is.null(source_kind)) {
    feeds <- feeds[feeds$source_kind %in% source_kind, , drop = FALSE]
  }
  feeds[order(tolower(feeds$folder), tolower(feeds$title)), , drop = FALSE]
}

resolve_feed_titles <- function(feeds) {
  if (!"display_title" %in% names(feeds)) {
    feeds$display_title <- NA_character_
  }
  feeds$source_title <- feeds$title
  has_display_title <- !is.na(feeds$display_title) &
    nzchar(trimws(feeds$display_title))
  feeds$title[has_display_title] <- feeds$display_title[has_display_title]
  feeds
}

store_list_entries <- function(
  store,
  actor_id,
  view = "unread",
  feed_id = NULL,
  limit = 100L,
  sort = "newest",
  now = Sys.time(),
  timezone = Sys.timezone()
) {
  view <- normalize_entry_view(view)
  sort <- normalize_entry_sort(sort)
  limit <- max(1L, min(as.integer(limit), 500L))
  window <- entry_view_window(view, now, timezone)

  if (identical(store$mode, "postgres")) {
    parameters <- list(actor_id)
    clauses <- c("COALESCE(s.hidden, false) = false")

    if (!is.null(feed_id) && nzchar(feed_id)) {
      parameters <- append(parameters, feed_id)
      clauses <- c(clauses, paste0("e.feed_id = $", length(parameters)))
    }
    if (identical(view, "unread")) {
      clauses <- c(clauses, "s.read_at IS NULL")
    }
    if (identical(view, "starred")) {
      clauses <- c(clauses, "COALESCE(s.starred, false) = true")
    }
    if (identical(view, "saved")) {
      clauses <- c(clauses, "COALESCE(s.saved, false) = true")
    }
    if (!is.null(window$since)) {
      parameters[[length(parameters) + 1L]] <- window$since
      clauses <- c(
        clauses,
        paste0(
          "COALESCE(e.published_at, e.inserted_at) >= $",
          length(parameters)
        )
      )
    }
    if (!is.null(window$before)) {
      parameters[[length(parameters) + 1L]] <- window$before
      clauses <- c(
        clauses,
        paste0(
          "COALESCE(e.published_at, e.inserted_at) < $",
          length(parameters)
        )
      )
    }

    query <- paste(
      "SELECT e.*,",
      paste(
        "COALESCE(NULLIF(p.display_title, ''), f.title) AS feed_title,",
        "f.title AS source_feed_title, f.folder,"
      ),
      "s.read_at, s.read_reason, COALESCE(s.starred, false) AS starred,",
      "COALESCE(s.saved, false) AS saved, s.last_opened_at",
      "FROM entries e",
      "JOIN feeds f ON f.feed_id = e.feed_id",
      paste(
        "LEFT JOIN subscription_preferences p ON p.feed_id = e.feed_id",
        "AND p.reader_id = $1"
      ),
      "LEFT JOIN entry_state s ON s.entry_id = e.entry_id AND s.actor_id = $1",
      "WHERE",
      paste(clauses, collapse = " AND "),
      "ORDER BY",
      entry_sort_sql(sort),
      "LIMIT",
      limit
    )
    return(DBI::dbGetQuery(store$pool, query, params = parameters))
  }

  memory_feeds <- store_list_feeds(store, actor_id)
  entries <- merge(
    store$memory$entries,
    memory_feeds[c("feed_id", "title", "source_title", "folder")],
    by = "feed_id",
    all.x = TRUE,
    sort = FALSE,
    suffixes = c("", "_feed")
  )
  names(entries)[names(entries) == "title_feed"] <- "feed_title"
  names(entries)[names(entries) == "source_title"] <- "source_feed_title"

  state <- store$memory$state[
    store$memory$state$actor_id == actor_id,
    ,
    drop = FALSE
  ]
  if (nrow(state)) {
    entries <- merge(
      entries,
      state,
      by = "entry_id",
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    entries$read_at <- NA_character_
    entries$read_reason <- NA_character_
    entries$starred <- FALSE
    entries$saved <- FALSE
    entries$hidden <- FALSE
    entries$last_opened_at <- NA_character_
  }
  entries$starred[is.na(entries$starred)] <- FALSE
  entries$saved[is.na(entries$saved)] <- FALSE
  entries$hidden[is.na(entries$hidden)] <- FALSE

  keep <- !entries$hidden
  if (!is.null(feed_id) && nzchar(feed_id)) {
    keep <- keep & entries$feed_id == feed_id
  }
  if (identical(view, "unread")) {
    keep <- keep & (is.na(entries$read_at) | !nzchar(entries$read_at))
  }
  if (identical(view, "starred")) {
    keep <- keep & entries$starred
  }
  if (identical(view, "saved")) {
    keep <- keep & entries$saved
  }
  if (!is.null(window$since)) {
    effective_time <- entry_effective_time(entries)
    keep <- keep &
      !is.na(effective_time) &
      as.numeric(effective_time) >= as.numeric(window$since) &
      as.numeric(effective_time) < as.numeric(window$before)
  }

  entries <- entries[keep, , drop = FALSE]
  entries <- entries[entry_sort_index(entries, sort), , drop = FALSE]
  utils::head(entries, limit)
}

normalize_entry_view <- function(view) {
  allowed <- c(
    "all",
    "unread",
    "starred",
    "saved",
    "today",
    "week",
    "month"
  )
  if (
    !is.character(view) ||
      length(view) != 1L ||
      is.na(view) ||
      !view %in% allowed
  ) {
    return("unread")
  }
  view
}

entry_view_window <- function(
  view,
  now = Sys.time(),
  timezone = Sys.timezone()
) {
  view <- normalize_entry_view(view)
  if (!view %in% c("today", "week", "month")) {
    return(list(since = NULL, before = NULL))
  }
  if (
    !is.character(timezone) ||
      length(timezone) != 1L ||
      is.na(timezone) ||
      !nzchar(timezone)
  ) {
    timezone <- "UTC"
  }

  local_day <- as.Date(format(now, "%Y-%m-%d", tz = timezone))
  start_day <- switch(
    view,
    today = local_day,
    week = local_day - ((as.POSIXlt(now, tz = timezone)$wday + 6L) %% 7L),
    month = as.Date(format(now, "%Y-%m-01", tz = timezone))
  )
  end_day <- switch(
    view,
    today = start_day + 1L,
    week = start_day + 7L,
    month = seq(start_day, by = "month", length.out = 2L)[[2]]
  )
  list(
    since = as.POSIXct(paste(start_day, "00:00:00"), tz = timezone),
    before = as.POSIXct(paste(end_day, "00:00:00"), tz = timezone)
  )
}

entry_view_since <- function(
  view,
  now = Sys.time(),
  timezone = Sys.timezone()
) {
  entry_view_window(view, now, timezone)$since
}

normalize_entry_sort <- function(sort) {
  allowed <- c("newest", "oldest", "recently_added", "feed", "title")
  if (
    !is.character(sort) ||
      length(sort) != 1L ||
      is.na(sort) ||
      !sort %in% allowed
  ) {
    return("newest")
  }
  sort
}

entry_sort_sql <- function(sort) {
  switch(
    normalize_entry_sort(sort),
    newest = "COALESCE(e.published_at, e.inserted_at) DESC, e.entry_id",
    oldest = "COALESCE(e.published_at, e.inserted_at) ASC, e.entry_id",
    recently_added = paste(
      "e.inserted_at DESC,",
      "COALESCE(e.published_at, e.inserted_at) DESC, e.entry_id"
    ),
    feed = paste(
      paste0(
        "lower(COALESCE(NULLIF(p.display_title, ''), f.title)) ",
        "COLLATE \"C\","
      ),
      "COALESCE(e.published_at, e.inserted_at) DESC, e.entry_id"
    ),
    title = paste(
      "lower(e.title) COLLATE \"C\",",
      "COALESCE(e.published_at, e.inserted_at) DESC, e.entry_id"
    )
  )
}

entry_effective_time <- function(entries) {
  published <- suppressWarnings(as.POSIXct(entries$published_at, tz = "UTC"))
  inserted <- suppressWarnings(as.POSIXct(entries$inserted_at, tz = "UTC"))
  published[is.na(published)] <- inserted[is.na(published)]
  published
}

entry_sort_index <- function(entries, sort) {
  published <- entry_effective_time(entries)
  inserted <- suppressWarnings(as.POSIXct(entries$inserted_at, tz = "UTC"))
  published <- as.numeric(published)
  inserted <- as.numeric(inserted)
  entry_id <- as.character(entries$entry_id)

  switch(
    normalize_entry_sort(sort),
    newest = order(-published, entry_id, na.last = TRUE, method = "radix"),
    oldest = order(published, entry_id, na.last = TRUE, method = "radix"),
    recently_added = order(
      -inserted,
      -published,
      entry_id,
      na.last = TRUE,
      method = "radix"
    ),
    feed = order(
      tolower(as.character(entries$feed_title)),
      -published,
      entry_id,
      na.last = TRUE,
      method = "radix"
    ),
    title = order(
      tolower(as.character(entries$title)),
      -published,
      entry_id,
      na.last = TRUE,
      method = "radix"
    )
  )
}

store_get_entry <- function(store, actor_id, entry_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT e.*,",
        paste(
          "COALESCE(NULLIF(p.display_title, ''), f.title) AS feed_title,",
          "f.title AS source_feed_title, f.site_url,"
        ),
        "s.read_at, s.read_reason, COALESCE(s.starred, false) AS starred,",
        "COALESCE(s.saved, false) AS saved, s.last_opened_at",
        "FROM entries e JOIN feeds f ON f.feed_id = e.feed_id",
        paste(
          "LEFT JOIN subscription_preferences p ON p.feed_id = e.feed_id",
          "AND p.reader_id = $1"
        ),
        "LEFT JOIN entry_state s ON s.entry_id = e.entry_id AND s.actor_id = $1",
        "WHERE e.entry_id = $2"
      ),
      params = list(actor_id, entry_id)
    )
    if (!nrow(rows)) {
      return(NULL)
    }
    return(as.list(rows[1, , drop = FALSE]))
  }

  entries <- store_list_entries(store, actor_id, view = "all", limit = 500L)
  row <- entries[entries$entry_id == entry_id, , drop = FALSE]
  if (!nrow(row)) {
    return(NULL)
  }
  result <- as.list(row[1, , drop = FALSE])
  result$site_url <- store$memory$feeds$site_url[
    match(result$feed_id, store$memory$feeds$feed_id)
  ]
  result
}

store_find_entry_by_url <- function(
  store,
  source_url,
  canonical_url = source_url
) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT * FROM entries",
        "WHERE url IN ($1, $2) OR canonical_url IN ($1, $2)",
        "ORDER BY inserted_at, entry_id LIMIT 1"
      ),
      params = list(source_url, canonical_url)
    )
    if (!nrow(rows)) {
      return(NULL)
    }
    return(as.list(rows[1, , drop = FALSE]))
  }

  entries <- store$memory$entries
  canonical_matches <- !is.na(entries$canonical_url) &
    entries$canonical_url %in% c(source_url, canonical_url)
  matches <- entries$url %in% c(source_url, canonical_url) | canonical_matches
  row <- entries[matches, , drop = FALSE]
  if (!nrow(row)) {
    return(NULL)
  }
  as.list(row[1, , drop = FALSE])
}

document_select_sql <- function() {
  paste(
    "SELECT d.document_id, d.entry_id, d.source_url, d.canonical_url,",
    "d.acquisition_method, d.producer, d.producer_version,",
    "d.producer_record_id, d.captured_at, d.received_at, d.title,",
    "d.author, d.site, d.published_at, d.markdown, d.word_count,",
    "d.content_hash, d.record_hash, d.provenance::text AS provenance,",
    "d.schema_version FROM documents d"
  )
}

store_get_document_by_id <- function(store, document_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        document_select_sql(),
        "WHERE d.document_id = $1"
      ),
      params = list(document_id)
    )
    if (!nrow(rows)) {
      return(NULL)
    }
    return(document_from_store_row(rows))
  }

  store$memory$documents[[document_id]] %||% NULL
}

store_list_documents <- function(store, entry_ids = NULL) {
  if (identical(store$mode, "postgres")) {
    query <- paste(
      document_select_sql(),
      "JOIN entry_document_heads h ON h.document_id = d.document_id"
    )
    params <- list()
    if (!is.null(entry_ids)) {
      entry_ids <- unique(as.character(entry_ids))
      if (!length(entry_ids)) {
        return(list())
      }
      placeholders <- paste0("$", seq_along(entry_ids), collapse = ", ")
      query <- paste(query, "WHERE h.entry_id IN (", placeholders, ")")
      params <- as.list(entry_ids)
    }
    query <- paste(query, "ORDER BY h.selected_at DESC, h.entry_id")
    rows <- if (length(params)) {
      DBI::dbGetQuery(store$pool, query, params = params)
    } else {
      DBI::dbGetQuery(store$pool, query)
    }
    if (!nrow(rows)) {
      return(list())
    }
    documents <- lapply(seq_len(nrow(rows)), function(index) {
      document_from_store_row(rows[index, , drop = FALSE])
    })
    return(stats::setNames(
      documents,
      vapply(documents, `[[`, character(1), "entry_id")
    ))
  }

  heads <- store$memory$document_heads
  if (!is.null(entry_ids)) {
    heads <- heads[names(heads) %in% as.character(entry_ids)]
  }
  documents <- unname(lapply(heads, function(id) {
    store$memory$documents[[id]]
  }))
  stats::setNames(
    documents,
    vapply(documents, `[[`, character(1), "entry_id")
  )
}

store_get_document <- function(store, entry_id) {
  documents <- store_list_documents(store, entry_ids = entry_id)
  if (!length(documents)) NULL else documents[[1]]
}

store_save_document <- function(store, document) {
  existing <- store_get_document_by_id(store, document$document_id)
  if (!is.null(existing)) {
    if (!identical(existing$record_hash, document$record_hash)) {
      document_conflict_abort(
        paste(
          "The producer record ID was already used for different content.",
          "Create a new capture ID and retry."
        )
      )
    }
    return(invisible(list(created = FALSE, document = existing)))
  }

  if (identical(store$mode, "postgres")) {
    created <- pool::poolWithTransaction(store$pool, function(connection) {
      inserted <- DBI::dbExecute(
        connection,
        paste(
          "INSERT INTO documents",
          "(document_id, entry_id, source_url, canonical_url,",
          "acquisition_method, producer, producer_version, producer_record_id,",
          "title, author, site, published_at, markdown, word_count,",
          "content_hash, record_hash, captured_at, received_at, provenance,",
          "schema_version)",
          "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12,",
          "$13, $14, $15, $16, $17, $18, $19::jsonb, $20)",
          "ON CONFLICT (document_id) DO NOTHING"
        ),
        params = list(
          document$document_id,
          document$entry_id,
          document$source_url,
          document$canonical_url,
          document$acquisition_method,
          document$producer,
          document$producer_version,
          document$producer_record_id,
          document$title,
          document$author,
          document$site,
          document$published_at,
          document$markdown,
          as.integer(document$word_count),
          document$content_hash,
          document$record_hash,
          document$captured_at,
          document$received_at,
          document_provenance_json(document),
          as.integer(document$schema_version)
        )
      )
      if (identical(inserted, 1L)) {
        DBI::dbExecute(
          connection,
          paste(
            "INSERT INTO entry_document_heads",
            "(entry_id, document_id, selected_at)",
            "VALUES ($1, $2, $3)",
            "ON CONFLICT (entry_id) DO UPDATE SET",
            "document_id = EXCLUDED.document_id,",
            "selected_at = EXCLUDED.selected_at"
          ),
          params = list(
            document$entry_id,
            document$document_id,
            document$received_at
          )
        )
      }
      identical(inserted, 1L)
    })
    if (!created) {
      raced <- store_get_document_by_id(store, document$document_id)
      if (
        is.null(raced) || !identical(raced$record_hash, document$record_hash)
      ) {
        document_conflict_abort("The document conflicted with another write.")
      }
    }
    return(invisible(list(created = created, document = document)))
  }

  store$memory$documents[[document$document_id]] <- document
  store$memory$document_heads[[document$entry_id]] <- document$document_id
  invisible(list(created = TRUE, document = document))
}

store_mark_opened <- function(store, actor_id, entry_id) {
  now <- utc_now()
  if (identical(store$mode, "postgres")) {
    DBI::dbExecute(
      store$pool,
      paste(
        paste(
          "INSERT INTO entry_state",
          "(actor_id, entry_id, read_at, read_reason, last_opened_at)"
        ),
        "VALUES ($1, $2, $3, 'opened', $3)",
        "ON CONFLICT (actor_id, entry_id) DO UPDATE SET",
        "read_at = COALESCE(entry_state.read_at, EXCLUDED.read_at),",
        paste(
          "read_reason = CASE WHEN entry_state.read_at IS NULL",
          "THEN EXCLUDED.read_reason ELSE entry_state.read_reason END,"
        ),
        "last_opened_at = EXCLUDED.last_opened_at"
      ),
      params = list(actor_id, entry_id, now)
    )
    return(invisible(NULL))
  }

  index <- which(
    store$memory$state$actor_id == actor_id &
      store$memory$state$entry_id == entry_id
  )
  if (!length(index)) {
    store$memory$state <- rbind(
      store$memory$state,
      data.frame(
        actor_id = actor_id,
        entry_id = entry_id,
        read_at = now,
        read_reason = "opened",
        starred = FALSE,
        saved = FALSE,
        hidden = FALSE,
        last_opened_at = now,
        stringsAsFactors = FALSE
      )
    )
  } else {
    if (
      !nzchar(store$memory$state$read_at[[index]]) ||
        is.na(store$memory$state$read_at[[index]])
    ) {
      store$memory$state$read_at[[index]] <- now
      store$memory$state$read_reason[[index]] <- "opened"
    }
    store$memory$state$last_opened_at[[index]] <- now
  }
  invisible(NULL)
}

store_mark_unread <- function(store, actor_id, entry_id) {
  if (identical(store$mode, "postgres")) {
    changed <- DBI::dbGetQuery(
      store$pool,
      paste(
        "UPDATE entry_state SET read_at = NULL, read_reason = NULL",
        "WHERE actor_id = $1 AND entry_id = $2 AND read_at IS NOT NULL",
        "RETURNING entry_id"
      ),
      params = list(actor_id, entry_id)
    )
    return(nrow(changed) == 1L)
  }

  index <- which(
    store$memory$state$actor_id == actor_id &
      store$memory$state$entry_id == entry_id
  )
  if (!length(index)) {
    return(FALSE)
  }
  was_read <- !is.na(store$memory$state$read_at[[index]]) &&
    nzchar(store$memory$state$read_at[[index]])
  store$memory$state$read_at[[index]] <- NA_character_
  store$memory$state$read_reason[[index]] <- NA_character_
  was_read
}

store_mark_entries_read <- function(
  store,
  actor_id,
  feed_id = NULL,
  before = NULL,
  reason
) {
  allowed_reasons <- c("bulk_all", "bulk_older_than_day")
  if (
    !is.character(reason) ||
      length(reason) != 1L ||
      is.na(reason) ||
      !reason %in% allowed_reasons
  ) {
    cli::cli_abort(
      "{.arg reason} must describe a supported bulk read action."
    )
  }
  now <- utc_now()

  if (identical(store$mode, "postgres")) {
    parameters <- list(actor_id, now, reason)
    clauses <- c(
      "s.read_at IS NULL",
      "COALESCE(s.hidden, false) = false"
    )
    if (!is.null(feed_id) && nzchar(feed_id)) {
      parameters <- append(parameters, feed_id)
      clauses <- c(clauses, paste0("e.feed_id = $", length(parameters)))
    }
    if (!is.null(before)) {
      parameters <- append(parameters, before)
      clauses <- c(
        clauses,
        paste0(
          "COALESCE(e.published_at, e.inserted_at) < $",
          length(parameters)
        )
      )
    }

    marked <- DBI::dbGetQuery(
      store$pool,
      paste(
        paste(
          "INSERT INTO entry_state",
          "(actor_id, entry_id, read_at, read_reason)"
        ),
        "SELECT $1, e.entry_id, $2, $3",
        "FROM entries e",
        paste(
          "LEFT JOIN entry_state s ON s.entry_id = e.entry_id",
          "AND s.actor_id = $1"
        ),
        "WHERE",
        paste(clauses, collapse = " AND "),
        "ON CONFLICT (actor_id, entry_id) DO UPDATE SET",
        "read_at = EXCLUDED.read_at, read_reason = EXCLUDED.read_reason",
        "WHERE entry_state.read_at IS NULL",
        "RETURNING entry_id"
      ),
      params = parameters
    )
    return(as.character(marked$entry_id))
  }

  entries <- store$memory$entries
  keep <- rep(TRUE, nrow(entries))
  if (!is.null(feed_id) && nzchar(feed_id)) {
    keep <- keep & entries$feed_id == feed_id
  }
  if (!is.null(before)) {
    effective_time <- entry_effective_time(entries)
    keep <- keep & !is.na(effective_time) & effective_time < before
  }

  state <- store$memory$state
  read_ids <- state$entry_id[
    state$actor_id == actor_id &
      !is.na(state$read_at) &
      nzchar(state$read_at)
  ]
  hidden_ids <- state$entry_id[
    state$actor_id == actor_id & state$hidden
  ]
  entry_ids <- entries$entry_id[keep]
  entry_ids <- entry_ids[!entry_ids %in% c(read_ids, hidden_ids)]

  for (entry_id in entry_ids) {
    index <- which(
      store$memory$state$actor_id == actor_id &
        store$memory$state$entry_id == entry_id
    )
    if (!length(index)) {
      store$memory$state <- rbind(
        store$memory$state,
        data.frame(
          actor_id = actor_id,
          entry_id = entry_id,
          read_at = now,
          read_reason = reason,
          starred = FALSE,
          saved = FALSE,
          hidden = FALSE,
          last_opened_at = NA_character_,
          stringsAsFactors = FALSE
        )
      )
    } else {
      store$memory$state$read_at[[index]] <- now
      store$memory$state$read_reason[[index]] <- reason
    }
  }
  as.character(entry_ids)
}

store_toggle_state <- function(store, actor_id, entry_id, field) {
  if (!field %in% c("starred", "saved")) {
    cli::cli_abort(
      "{.arg field} must be one of {.val starred} or {.val saved}, not {.val {field}}."
    )
  }

  if (identical(store$mode, "postgres")) {
    query <- paste0(
      "INSERT INTO entry_state (actor_id, entry_id, ",
      field,
      ") ",
      "VALUES ($1, $2, true) ",
      "ON CONFLICT (actor_id, entry_id) DO UPDATE SET ",
      field,
      " = NOT entry_state.",
      field,
      " RETURNING ",
      field
    )
    result <- DBI::dbGetQuery(
      store$pool,
      query,
      params = list(actor_id, entry_id)
    )
    return(isTRUE(result[[field]][[1]]))
  }

  index <- which(
    store$memory$state$actor_id == actor_id &
      store$memory$state$entry_id == entry_id
  )
  if (!length(index)) {
    row <- data.frame(
      actor_id = actor_id,
      entry_id = entry_id,
      read_at = NA_character_,
      read_reason = NA_character_,
      starred = FALSE,
      saved = FALSE,
      hidden = FALSE,
      last_opened_at = NA_character_,
      stringsAsFactors = FALSE
    )
    row[[field]] <- TRUE
    store$memory$state <- rbind(store$memory$state, row)
    return(TRUE)
  }
  store$memory$state[[field]][[index]] <- !isTRUE(store$memory$state[[field]][[
    index
  ]])
  store$memory$state[[field]][[index]]
}

store_record_event <- function(store, event) {
  payload <- jsonlite::toJSON(
    event$payload %||% list(),
    auto_unbox = TRUE,
    null = "null"
  )

  if (identical(store$mode, "postgres")) {
    DBI::dbExecute(
      store$pool,
      paste(
        "INSERT INTO events",
        "(event_id, actor_id, entry_id, session_id, event_type, happened_at, surface, position, payload)",
        "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9::jsonb)",
        "ON CONFLICT (event_id) DO NOTHING"
      ),
      params = list(
        event$event_id,
        event$actor_id,
        event$entry_id %||% NA_character_,
        event$session_id,
        event$event_type,
        event$happened_at,
        event$surface,
        as.integer(event$position %||% NA_integer_),
        payload
      )
    )
  } else {
    if (event$event_id %in% store$memory$events$event_id) {
      return(invisible(event))
    }
    row <- data.frame(
      event_id = event$event_id,
      actor_id = event$actor_id,
      entry_id = event$entry_id %||% NA_character_,
      session_id = event$session_id,
      event_type = event$event_type,
      happened_at = event$happened_at,
      surface = event$surface %||% NA_character_,
      position = as.integer(event$position %||% NA_integer_),
      payload = payload,
      stringsAsFactors = FALSE
    )
    store$memory$events <- rbind(store$memory$events, row)
  }
  invisible(event)
}

store_upsert_feed <- function(store, feed) {
  if (identical(store$mode, "postgres")) {
    DBI::dbExecute(
      store$pool,
      paste(
        "INSERT INTO feeds",
        "(feed_id, feed_url, site_url, title, folder, source_kind, etag,",
        "last_modified, poll_status, last_polled_at)",
        "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, now())",
        "ON CONFLICT (feed_id) DO UPDATE SET",
        "feed_url = EXCLUDED.feed_url, site_url = EXCLUDED.site_url,",
        "title = EXCLUDED.title, folder = EXCLUDED.folder,",
        "source_kind = EXCLUDED.source_kind,",
        "etag = EXCLUDED.etag, last_modified = EXCLUDED.last_modified,",
        "poll_status = EXCLUDED.poll_status, last_polled_at = now()"
      ),
      params = list(
        feed$feed_id,
        feed$feed_url,
        feed$site_url,
        feed$title,
        feed$folder %||% "Unsorted",
        feed$source_kind %||% "subscription",
        feed$etag,
        feed$last_modified,
        feed$poll_status %||% "ok"
      )
    )
    return(invisible(feed$feed_id))
  }

  index <- which(
    store$memory$feeds$feed_id == feed$feed_id |
      store$memory$feeds$feed_url == feed$feed_url
  )
  row <- data.frame(
    feed_id = feed$feed_id,
    feed_url = feed$feed_url,
    site_url = feed$site_url,
    title = feed$title,
    folder = feed$folder %||% "Unsorted",
    source_kind = feed$source_kind %||% "subscription",
    etag = feed$etag,
    last_modified = feed$last_modified,
    poll_status = feed$poll_status %||% "ok",
    last_polled_at = utc_now(),
    created_at = utc_now(),
    stringsAsFactors = FALSE
  )
  if (length(index)) {
    row$created_at <- store$memory$feeds$created_at[[index[[1]]]]
    store$memory$feeds[index[[1]], ] <- row
  } else {
    store$memory$feeds <- rbind(store$memory$feeds, row)
  }
  invisible(feed$feed_id)
}

store_rename_feed <- function(store, reader_id, feed_id, title) {
  title <- trimws(title)
  if (!nzchar(title)) {
    cli::cli_abort("Feed names cannot be empty.")
  }

  if (identical(store$mode, "postgres")) {
    updated <- DBI::dbGetQuery(
      store$pool,
      paste(
        "INSERT INTO subscription_preferences",
        "(reader_id, feed_id, display_title)",
        "SELECT $1, feed_id, NULLIF($3, title) FROM feeds WHERE feed_id = $2",
        "ON CONFLICT (reader_id, feed_id) DO UPDATE SET",
        "display_title = EXCLUDED.display_title",
        "RETURNING feed_id"
      ),
      params = list(reader_id, feed_id, title)
    )
    if (nrow(updated) != 1L) {
      cli::cli_abort("That feed no longer exists.")
    }
    return(invisible(title))
  }

  index <- match(feed_id, store$memory$feeds$feed_id)
  if (is.na(index)) {
    cli::cli_abort("That feed no longer exists.")
  }
  source_title <- store$memory$feeds$title[[index]]
  display_title <- if (identical(title, source_title)) {
    NA_character_
  } else {
    title
  }
  preference_index <- which(
    store$memory$subscription_preferences$reader_id == reader_id &
      store$memory$subscription_preferences$feed_id == feed_id
  )
  preference <- data.frame(
    reader_id = reader_id,
    feed_id = feed_id,
    display_title = display_title,
    stringsAsFactors = FALSE
  )
  if (length(preference_index)) {
    store$memory$subscription_preferences[preference_index[[1]], ] <- preference
  } else {
    store$memory$subscription_preferences <- rbind(
      store$memory$subscription_preferences,
      preference
    )
  }
  invisible(title)
}

store_upsert_entries <- function(store, entries) {
  if (!nrow(entries)) {
    return(invisible(0L))
  }

  if (identical(store$mode, "postgres")) {
    pool::poolWithTransaction(store$pool, function(connection) {
      for (index in seq_len(nrow(entries))) {
        entry <- entries[index, , drop = FALSE]
        DBI::dbExecute(
          connection,
          paste(
            "INSERT INTO entries",
            "(entry_id, feed_id, external_id, url, canonical_url, title, author,",
            "summary, feed_content, published_at, content_hash)",
            "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)",
            "ON CONFLICT (feed_id, external_id) DO UPDATE SET",
            "url = EXCLUDED.url, canonical_url = EXCLUDED.canonical_url,",
            "title = EXCLUDED.title, author = EXCLUDED.author,",
            "summary = EXCLUDED.summary, feed_content = EXCLUDED.feed_content,",
            "published_at = EXCLUDED.published_at, content_hash = EXCLUDED.content_hash"
          ),
          params = unname(as.list(entry[c(
            "entry_id",
            "feed_id",
            "external_id",
            "url",
            "canonical_url",
            "title",
            "author",
            "summary",
            "feed_content",
            "published_at",
            "content_hash"
          )]))
        )
      }
    })
    return(invisible(nrow(entries)))
  }

  for (index in seq_len(nrow(entries))) {
    entry <- entries[index, , drop = FALSE]
    existing <- which(
      store$memory$entries$feed_id == entry$feed_id &
        store$memory$entries$external_id == entry$external_id
    )
    if (length(existing)) {
      entry$inserted_at <- store$memory$entries$inserted_at[[existing[[1]]]]
      store$memory$entries[existing[[1]], ] <- entry
    } else {
      store$memory$entries <- rbind(store$memory$entries, entry)
    }
  }
  invisible(nrow(entries))
}
