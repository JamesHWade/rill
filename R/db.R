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

store_scalar_string <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))
}

rill_store <- function(config) {
  legacy_reader_id <- config$actor_id %||% "reader"
  telemetry_local_span(
    "store.open",
    list("store.mode" = if (config$demo_mode) "memory" else "postgres")
  )

  if (config$demo_mode) {
    sample <- sample_rill_data()
    memory <- new.env(parent = emptyenv())
    memory$feeds <- sample$feeds
    memory$entries <- sample$entries
    memory$documents <- sample$documents
    memory$article_preparations <- list()
    memory$document_heads <- stats::setNames(
      vapply(sample$documents, `[[`, character(1), "document_id"),
      vapply(sample$documents, `[[`, character(1), "entry_id")
    )
    memory$document_selections <- data.frame(
      reader_id = character(),
      feed_id = character(),
      entry_id = character(),
      document_id = character(),
      ownership_key = character(),
      selected_at = character(),
      stringsAsFactors = FALSE
    )
    memory$capture_credentials <- data.frame(
      reader_id = character(),
      token_hash = character(),
      created_at = character(),
      updated_at = character(),
      stringsAsFactors = FALSE
    )
    memory$state <- data.frame(
      reader_id = character(),
      entry_id = character(),
      feed_id = character(),
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
    subscription_feeds <- sample$feeds[
      sample$feeds$source_kind == "subscription",
      ,
      drop = FALSE
    ]
    memory$subscriptions <- data.frame(
      reader_id = rep(legacy_reader_id, nrow(subscription_feeds)),
      feed_id = subscription_feeds$feed_id,
      folder = subscription_feeds$folder,
      display_title = NA_character_,
      status = "active",
      subscribed_at = subscription_feeds$created_at,
      updated_at = subscription_feeds$created_at,
      deactivated_at = NA_character_,
      stringsAsFactors = FALSE
    )
    memory$feed_groups <- empty_feed_groups()
    memory$subscription_groups <- empty_subscription_groups()
    memory$events <- data.frame(
      event_id = character(),
      reader_id = character(),
      entry_id = character(),
      feed_id = character(),
      session_id = character(),
      event_type = character(),
      happened_at = character(),
      surface = character(),
      position = integer(),
      payload = character(),
      stringsAsFactors = FALSE
    )
    memory$feed_poll_runs <- data.frame(
      run_id = character(),
      started_at = character(),
      completed_at = character(),
      status = character(),
      due_count = integer(),
      succeeded_count = integer(),
      failed_count = integer(),
      failure_threshold = integer(),
      error_class = character(),
      error_message = character(),
      stringsAsFactors = FALSE
    )
    memory$feed_poll_outcomes <- data.frame(
      run_id = character(),
      feed_id = character(),
      started_at = character(),
      completed_at = character(),
      status = character(),
      added_count = integer(),
      error_class = character(),
      error_message = character(),
      stringsAsFactors = FALSE
    )
    memory$feed_poll_locked <- FALSE
    memory$agent_runs <- list()
    memory$reader_feedback <- list()
    memory$orientations <- list()
    memory$orientation_destination_settings <- list()
    memory$deferred_reader_questions <- list()
    created_at <- utc_now()
    memory$readers <- data.frame(
      reader_id = legacy_reader_id,
      status = "active",
      created_at = created_at,
      updated_at = created_at,
      disabled_at = NA_character_,
      stringsAsFactors = FALSE
    )
    memory$reader_identities <- data.frame(
      issuer = character(),
      subject = character(),
      reader_id = character(),
      email = character(),
      display_name = character(),
      created_at = character(),
      last_seen_at = character(),
      revoked_at = character(),
      stringsAsFactors = FALSE
    )
    memory$reader_admissions <- data.frame(
      issuer = character(),
      subject = character(),
      status = character(),
      email = character(),
      display_name = character(),
      first_seen_at = character(),
      last_seen_at = character(),
      attempt_count = integer(),
      decided_at = character(),
      stringsAsFactors = FALSE
    )
    memory$reader_identity_events <- data.frame(
      event_sequence = integer(),
      event_id = character(),
      reader_id = character(),
      action = character(),
      responsible_id = character(),
      reason = character(),
      happened_at = character(),
      stringsAsFactors = FALSE
    )
    store <- structure(
      list(
        mode = "memory",
        memory = memory,
        private_reader_id = legacy_reader_id
      ),
      class = "rill_store"
    )
    for (i in seq_len(nrow(subscription_feeds))) {
      subscription <- subscription_feeds[i, , drop = FALSE]
      store_set_legacy_folder(
        store,
        legacy_reader_id,
        subscription$feed_id,
        subscription$folder
      )
    }
    orientation <- sample_rill_orientation(
      store,
      legacy_reader_id
    )
    run <- sample_rill_orientation_run(orientation)
    memory$agent_runs[[run$run_id]] <- run
    store_save_orientation(store, orientation)
    return(store)
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
  initialized <- FALSE
  on.exit(
    {
      if (!initialized) {
        pool::poolClose(database_pool)
      }
    },
    add = TRUE
  )

  store <- structure(
    list(
      mode = "postgres",
      pool = database_pool,
      private_reader_id = legacy_reader_id
    ),
    class = "rill_store"
  )
  store_apply_schema(store, legacy_reader_id = legacy_reader_id)
  store_migrate_legacy_feed_titles(store, legacy_reader_id)
  store_expire_reader_admissions(store)
  initialized <- TRUE
  store
}

rill_store_close <- function(store) {
  if (identical(store$mode, "postgres")) {
    pool::poolClose(store$pool)
  }
  invisible(NULL)
}
