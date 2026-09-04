pkgload::load_all(export_all = FALSE, helpers = FALSE, quiet = TRUE)

required_env <- function(name) {
  value <- trimws(Sys.getenv(name, unset = ""))
  if (!nzchar(value)) {
    cli::cli_abort("Set {.envvar {name}} before copying the Library.")
  }
  value
}

table_exists <- function(connection, table) {
  table %in% DBI::dbListTables(connection)
}

read_table <- function(connection, table) {
  if (!table_exists(connection, table)) {
    cli::cli_abort("The source database has no {.val {table}} table.")
  }
  DBI::dbReadTable(connection, table)
}

append_columns <- function(connection, table, rows, excluded = character()) {
  if (!nrow(rows)) {
    return(invisible(0L))
  }
  target_columns <- DBI::dbListFields(connection, table)
  columns <- setdiff(intersect(target_columns, names(rows)), excluded)
  DBI::dbAppendTable(connection, table, rows[, columns, drop = FALSE])
}

copy_library <- function() {
  source_url <- required_env("RILL_SOURCE_DATABASE_URL")
  source_reader_id <- required_env("RILL_SOURCE_READER_ID")
  target_reader_id <- required_env("RILL_ACTOR_ID")
  dry_run <- rill:::env_flag("RILL_COPY_DRY_RUN", FALSE)
  target_url <- if (dry_run) "" else required_env("DATABASE_URL")
  if (!dry_run && identical(source_url, target_url)) {
    cli::cli_abort("The source and target database URLs must differ.")
  }
  if (
    !dry_run &&
      !identical(Sys.getenv("RILL_COPY_CONFIRM"), "copy-public-library")
  ) {
    cli::cli_abort(c(
      "The Library copy is not confirmed.",
      "i" = paste(
        "Set {.envvar RILL_COPY_CONFIRM} to {.val copy-public-library} after",
        "checking both database URLs."
      )
    ))
  }

  source <- do.call(
    DBI::dbConnect,
    c(
      list(drv = RPostgres::Postgres()),
      rill:::postgres_connection_args(source_url)
    )
  )
  on.exit(DBI::dbDisconnect(source), add = TRUE)
  DBI::dbExecute(
    source,
    "BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ READ ONLY"
  )
  on.exit(try(DBI::dbExecute(source, "ROLLBACK"), silent = TRUE), add = TRUE)

  feeds <- read_table(source, "feeds")
  entries <- read_table(source, "entries")
  documents <- read_table(source, "documents")
  state <- read_table(source, "entry_state")

  if (table_exists(source, "subscriptions")) {
    subscriptions <- read_table(source, "subscriptions")
    subscriptions <- subscriptions[
      subscriptions$reader_id == source_reader_id,
      ,
      drop = FALSE
    ]
    subscriptions$reader_id <- target_reader_id
  } else {
    source_feeds <- if ("source_kind" %in% names(feeds)) {
      feeds[feeds$source_kind == "subscription", , drop = FALSE]
    } else {
      feeds
    }
    subscriptions <- data.frame(
      reader_id = target_reader_id,
      feed_id = source_feeds$feed_id,
      folder = source_feeds$folder,
      display_title = NA_character_,
      status = "active",
      subscribed_at = source_feeds$created_at,
      updated_at = source_feeds$created_at,
      deactivated_at = as.POSIXct(NA),
      stringsAsFactors = FALSE
    )
    if (table_exists(source, "subscription_preferences")) {
      preferences <- read_table(source, "subscription_preferences")
      preferences <- preferences[
        preferences$reader_id == source_reader_id,
        ,
        drop = FALSE
      ]
      subscriptions$display_title <- preferences$display_title[
        match(subscriptions$feed_id, preferences$feed_id)
      ]
    }
  }

  feed_ids <- unique(subscriptions$feed_id)
  feeds <- feeds[feeds$feed_id %in% feed_ids, , drop = FALSE]
  entries <- entries[entries$feed_id %in% feed_ids, , drop = FALSE]
  entry_ids <- unique(entries$entry_id)
  documents <- documents[documents$entry_id %in% entry_ids, , drop = FALSE]
  documents <- documents[
    documents$acquisition_method != "browser_capture",
    ,
    drop = FALSE
  ]
  if ("reader_id" %in% names(documents)) {
    documents <- documents[
      is.na(documents$reader_id) | !nzchar(documents$reader_id),
      ,
      drop = FALSE
    ]
  }

  heads_table <- if (table_exists(source, "public_document_heads")) {
    "public_document_heads"
  } else {
    "entry_document_heads"
  }
  heads <- read_table(source, heads_table)
  heads <- heads[
    heads$entry_id %in%
      entry_ids &
      heads$document_id %in% documents$document_id,
    ,
    drop = FALSE
  ]

  reader_column <- if ("reader_id" %in% names(state)) {
    "reader_id"
  } else {
    "actor_id"
  }
  state <- state[state[[reader_column]] == source_reader_id, , drop = FALSE]
  names(state)[names(state) == reader_column] <- "reader_id"
  state$reader_id <- target_reader_id
  if (!"feed_id" %in% names(state)) {
    state$feed_id <- entries$feed_id[match(state$entry_id, entries$entry_id)]
  }
  state <- state[
    state$feed_id %in% feed_ids & state$entry_id %in% entry_ids,
    ,
    drop = FALSE
  ]

  selected <- c(
    feeds = nrow(feeds),
    entries = nrow(entries),
    documents = nrow(documents),
    subscriptions = nrow(subscriptions),
    public_document_heads = nrow(heads),
    entry_state = nrow(state)
  )
  if (dry_run) {
    cli::cli_alert_info("Dry run selected the public Library rows below.")
    for (name in names(selected)) {
      cli::cli_inform("{name}: {selected[[name]]}")
    }
    return(invisible(selected))
  }

  target_config <- rill:::rill_config()
  target_config$database_url <- target_url
  target_config$demo_mode <- FALSE
  target_config$actor_id <- target_reader_id
  target <- rill:::rill_store(target_config)
  on.exit(rill:::rill_store_close(target), add = TRUE)

  copied <- pool::poolWithTransaction(target$pool, function(connection) {
    protected_tables <- c(
      "feeds",
      "entries",
      "documents",
      "subscriptions",
      "entry_state",
      "events",
      "agent_runs"
    )
    occupied <- vapply(
      protected_tables,
      \(table) {
        DBI::dbGetQuery(
          connection,
          paste0("SELECT count(*) AS n FROM ", table)
        )$n[[1L]]
      },
      numeric(1)
    )
    if (any(occupied > 0)) {
      cli::cli_abort(
        "The target is not an empty trial database; no rows were copied."
      )
    }

    counts <- c(
      feeds = append_columns(connection, "feeds", feeds),
      entries = append_columns(connection, "entries", entries),
      documents = append_columns(
        connection,
        "documents",
        documents,
        excluded = "ownership_key"
      ),
      subscriptions = append_columns(
        connection,
        "subscriptions",
        subscriptions
      ),
      public_document_heads = append_columns(
        connection,
        "public_document_heads",
        heads,
        excluded = "ownership_key"
      ),
      entry_state = append_columns(connection, "entry_state", state)
    )
    counts
  })

  cli::cli_alert_success(
    "Copied the public Library into the empty trial database."
  )
  for (name in names(copied)) {
    cli::cli_inform("{name}: {copied[[name]]}")
  }
  invisible(copied)
}

copy_library()
