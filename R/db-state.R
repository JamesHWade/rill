store_abort_entry_forbidden <- function() {
  cli::cli_abort(
    "That Feed Entry is not available in this Reader's Library.",
    class = "rill_entry_forbidden"
  )
}

store_entry_feed_for_reader <- function(store, reader_id, entry_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT e.feed_id FROM entries e",
        "JOIN feeds f ON f.feed_id = e.feed_id",
        paste(
          "JOIN subscriptions s ON s.feed_id = e.feed_id",
          "AND s.reader_id = $1 AND s.status = 'active'"
        ),
        paste(
          "WHERE e.entry_id = $2 AND",
          postgres_capture_entry_visible_sql("$1")
        )
      ),
      params = list(reader_id, entry_id)
    )
    if (nrow(rows) == 1L) {
      return(rows$feed_id[[1L]])
    }
  } else {
    entries <- store$memory$entries
    entry_index <- match(entry_id, entries$entry_id)
    if (!is.na(entry_index)) {
      feed_id <- entries$feed_id[[entry_index]]
      subscriptions <- store$memory$subscriptions
      allowed <- subscriptions$reader_id == reader_id &
        subscriptions$feed_id == feed_id &
        subscriptions$status == "active"
      if (
        any(allowed) &&
          entry_id %in% memory_reader_visible_entry_ids(store, reader_id)
      ) {
        return(feed_id)
      }
    }
  }
  store_abort_entry_forbidden()
}

store_mark_opened <- function(
  store,
  reader_id,
  entry_id,
  opened_at = utc_now()
) {
  now <- opened_at
  if (identical(store$mode, "postgres")) {
    opened <- DBI::dbGetQuery(
      store$pool,
      paste(
        paste(
          "WITH authorized AS (SELECT e.entry_id, e.feed_id",
          "FROM entries e"
        ),
        "JOIN feeds f ON f.feed_id = e.feed_id",
        paste(
          "JOIN subscriptions s ON s.feed_id = e.feed_id",
          "AND s.reader_id = $1 AND s.status = 'active'"
        ),
        paste(
          "WHERE e.entry_id = $2 AND",
          postgres_capture_entry_visible_sql("$1"),
          "FOR SHARE OF s)"
        ),
        paste(
          "INSERT INTO entry_state",
          "(reader_id, entry_id, feed_id, read_at, read_reason, last_opened_at)"
        ),
        paste(
          "SELECT $1, entry_id, feed_id, $3, 'opened', $3",
          "FROM authorized"
        ),
        "ON CONFLICT (reader_id, entry_id) DO UPDATE SET",
        "read_at = COALESCE(entry_state.read_at, EXCLUDED.read_at),",
        paste(
          "read_reason = CASE WHEN entry_state.read_at IS NULL",
          "OR entry_state.read_reason = 'manual_queue'",
          "THEN EXCLUDED.read_reason ELSE entry_state.read_reason END,"
        ),
        "last_opened_at = EXCLUDED.last_opened_at",
        "RETURNING entry_id"
      ),
      params = list(reader_id, entry_id, now)
    )
    if (!nrow(opened)) {
      store_abort_entry_forbidden()
    }
    return(invisible(NULL))
  }

  feed_id <- store_entry_feed_for_reader(store, reader_id, entry_id)
  index <- which(
    store$memory$state$reader_id == reader_id &
      store$memory$state$entry_id == entry_id
  )
  if (!length(index)) {
    store$memory$state <- rbind(
      store$memory$state,
      data.frame(
        reader_id = reader_id,
        entry_id = entry_id,
        feed_id = feed_id,
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
    if (identical(store$memory$state$read_reason[[index]], "manual_queue")) {
      store$memory$state$read_reason[[index]] <- "opened"
    }
    store$memory$state$last_opened_at[[index]] <- now
  }
  invisible(NULL)
}

store_mark_unread <- function(store, reader_id, entry_id) {
  if (identical(store$mode, "postgres")) {
    result <- DBI::dbGetQuery(
      store$pool,
      paste(
        paste(
          "WITH authorized AS (SELECT e.entry_id, e.feed_id",
          "FROM entries e"
        ),
        "JOIN feeds f ON f.feed_id = e.feed_id",
        paste(
          "JOIN subscriptions s ON s.feed_id = e.feed_id",
          "AND s.reader_id = $1 AND s.status = 'active'"
        ),
        paste(
          "WHERE e.entry_id = $2 AND",
          postgres_capture_entry_visible_sql("$1"),
          "FOR SHARE OF s),"
        ),
        "changed AS (UPDATE entry_state state SET",
        "read_at = NULL, read_reason = NULL FROM authorized",
        paste(
          "WHERE state.reader_id = $1",
          "AND state.entry_id = authorized.entry_id"
        ),
        "AND state.read_at IS NOT NULL RETURNING state.entry_id)",
        paste(
          "SELECT EXISTS (SELECT 1 FROM authorized) AS authorized,",
          "EXISTS (SELECT 1 FROM changed) AS changed"
        )
      ),
      params = list(reader_id, entry_id)
    )
    if (!isTRUE(result$authorized[[1L]])) {
      store_abort_entry_forbidden()
    }
    return(isTRUE(result$changed[[1L]]))
  }

  store_entry_feed_for_reader(store, reader_id, entry_id)
  index <- which(
    store$memory$state$reader_id == reader_id &
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
  reader_id,
  feed_id = NULL,
  before = NULL,
  reason,
  folder = NULL,
  group_ids = NULL,
  group_match = "any",
  ungrouped = FALSE
) {
  group_match <- match.arg(group_match, c("any", "all"))
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
    parameters <- list(reader_id, now, reason)
    entry_clauses <- postgres_capture_entry_visible_sql("$1")
    state_clauses <- c(
      "s.read_at IS NULL",
      "COALESCE(s.hidden, false) = false"
    )
    if (!is.null(feed_id) && nzchar(feed_id)) {
      parameters <- append(parameters, feed_id)
      entry_clauses <- c(
        entry_clauses,
        paste0("e.feed_id = $", length(parameters))
      )
    }
    if (!is.null(folder)) {
      parameters <- append(parameters, folder)
      entry_clauses <- c(
        entry_clauses,
        paste0("sub.folder = $", length(parameters))
      )
    }
    group_filter <- group_filter_sql(
      group_ids,
      group_match,
      ungrouped,
      parameters
    )
    entry_clauses <- c(entry_clauses, group_filter$sql)
    parameters <- group_filter$parameters
    if (!is.null(before)) {
      parameters[[length(parameters) + 1L]] <- before
      entry_clauses <- c(
        entry_clauses,
        paste0(
          "COALESCE(e.published_at, e.inserted_at) < $",
          length(parameters)
        )
      )
    }

    marked <- DBI::dbGetQuery(
      store$pool,
      paste(
        "WITH authorized_entries AS (SELECT e.* FROM entries e",
        "JOIN feeds f ON f.feed_id = e.feed_id",
        paste(
          "JOIN subscriptions sub ON sub.feed_id = e.feed_id",
          "AND sub.reader_id = $1 AND sub.status = 'active'"
        ),
        if (length(entry_clauses)) {
          paste("WHERE", paste(entry_clauses, collapse = " AND "))
        } else {
          ""
        },
        "FOR SHARE OF sub)",
        paste(
          "INSERT INTO entry_state",
          "(reader_id, entry_id, feed_id, read_at, read_reason)"
        ),
        "SELECT $1, e.entry_id, e.feed_id, $2, $3",
        "FROM authorized_entries e",
        paste(
          "LEFT JOIN entry_state s ON s.entry_id = e.entry_id",
          "AND s.reader_id = $1"
        ),
        "WHERE",
        paste(state_clauses, collapse = " AND "),
        "ON CONFLICT (reader_id, entry_id) DO UPDATE SET",
        "read_at = EXCLUDED.read_at, read_reason = EXCLUDED.read_reason",
        "WHERE entry_state.read_at IS NULL",
        "RETURNING entry_id"
      ),
      params = parameters
    )
    return(as.character(marked$entry_id))
  }

  active_feeds <- store_list_feeds(store, reader_id)
  if (!is.null(folder)) {
    active_feeds <- active_feeds[active_feeds$folder == folder, , drop = FALSE]
  }
  entries <- store$memory$entries[
    store$memory$entries$feed_id %in%
      active_feeds$feed_id &
      store$memory$entries$entry_id %in%
        memory_reader_visible_entry_ids(store, reader_id),
    ,
    drop = FALSE
  ]
  keep <- rep(TRUE, nrow(entries))
  group_feeds <- store_group_feed_ids(
    store,
    reader_id,
    group_ids,
    group_match,
    ungrouped
  )
  if (!is.null(group_feeds)) {
    keep <- keep & entries$feed_id %in% group_feeds
  }
  if (!is.null(feed_id) && nzchar(feed_id)) {
    keep <- keep & entries$feed_id == feed_id
  }
  if (!is.null(before)) {
    effective_time <- entry_effective_time(entries)
    keep <- keep & !is.na(effective_time) & effective_time < before
  }

  state <- store$memory$state
  read_ids <- state$entry_id[
    state$reader_id == reader_id &
      !is.na(state$read_at) &
      nzchar(state$read_at)
  ]
  hidden_ids <- state$entry_id[
    state$reader_id == reader_id & state$hidden
  ]
  entry_ids <- entries$entry_id[keep]
  entry_ids <- entry_ids[!entry_ids %in% c(read_ids, hidden_ids)]

  for (entry_id in entry_ids) {
    index <- which(
      store$memory$state$reader_id == reader_id &
        store$memory$state$entry_id == entry_id
    )
    if (!length(index)) {
      store$memory$state <- rbind(
        store$memory$state,
        data.frame(
          reader_id = reader_id,
          entry_id = entry_id,
          feed_id = entries$feed_id[match(entry_id, entries$entry_id)],
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

store_toggle_state <- function(store, reader_id, entry_id, field) {
  if (!field %in% c("starred", "saved")) {
    cli::cli_abort(
      "{.arg field} must be one of {.val starred} or {.val saved}, not {.val {field}}."
    )
  }

  if (identical(store$mode, "postgres")) {
    query <- paste0(
      "WITH authorized AS (SELECT e.entry_id, e.feed_id ",
      "FROM entries e JOIN feeds f ON f.feed_id = e.feed_id ",
      "JOIN subscriptions s ON s.feed_id = e.feed_id ",
      "AND s.reader_id = $1 AND s.status = 'active' ",
      "WHERE e.entry_id = $2 AND ",
      postgres_capture_entry_visible_sql("$1"),
      " FOR SHARE OF s) ",
      "INSERT INTO entry_state (reader_id, entry_id, feed_id, ",
      field,
      ") ",
      "SELECT $1, entry_id, feed_id, true FROM authorized ",
      "ON CONFLICT (reader_id, entry_id) DO UPDATE SET ",
      field,
      " = NOT entry_state.",
      field,
      " RETURNING ",
      field
    )
    result <- DBI::dbGetQuery(
      store$pool,
      query,
      params = list(reader_id, entry_id)
    )
    if (!nrow(result)) {
      store_abort_entry_forbidden()
    }
    return(isTRUE(result[[field]][[1]]))
  }

  feed_id <- store_entry_feed_for_reader(store, reader_id, entry_id)
  index <- which(
    store$memory$state$reader_id == reader_id &
      store$memory$state$entry_id == entry_id
  )
  if (!length(index)) {
    row <- data.frame(
      reader_id = reader_id,
      entry_id = entry_id,
      feed_id = feed_id,
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

store_record_event <- function(store, event, require_new = FALSE) {
  reader_id <- event$reader_id %||% event$actor_id
  if (!store_scalar_string(reader_id)) {
    cli::cli_abort(
      "Reading History events require a Reader.",
      class = "rill_event_reader_invalid"
    )
  }
  payload <- jsonlite::toJSON(
    event$payload %||% list(),
    auto_unbox = TRUE,
    null = "null"
  )

  if (identical(store$mode, "postgres")) {
    if (is.null(event$entry_id)) {
      inserted <- DBI::dbExecute(
        store$pool,
        paste(
          "INSERT INTO events",
          paste(
            "(event_id, reader_id, entry_id, feed_id, session_id, event_type,",
            "happened_at, surface, position, payload)"
          ),
          "VALUES ($1, $2, NULL, NULL, $3, $4, $5, $6, $7, $8::jsonb)",
          "ON CONFLICT (event_id) DO NOTHING"
        ),
        params = list(
          event$event_id,
          reader_id,
          event$session_id,
          event$event_type,
          event$happened_at,
          event$surface,
          as.integer(event$position %||% NA_integer_),
          payload
        )
      )
    } else {
      result <- DBI::dbGetQuery(
        store$pool,
        paste(
          paste(
            "WITH existing AS (SELECT event_id FROM events",
            "WHERE event_id = $1 AND reader_id = $2 AND entry_id = $3),"
          ),
          paste(
            "authorized AS (SELECT e.entry_id, e.feed_id",
            "FROM entries e"
          ),
          "JOIN feeds f ON f.feed_id = e.feed_id",
          paste(
            "JOIN subscriptions s ON s.feed_id = e.feed_id",
            "AND s.reader_id = $2 AND s.status = 'active'"
          ),
          paste(
            "WHERE e.entry_id = $3",
            "AND NOT EXISTS (SELECT 1 FROM existing)",
            "AND",
            postgres_capture_entry_visible_sql("$2"),
            "FOR SHARE OF s),"
          ),
          "inserted AS (INSERT INTO events",
          paste(
            "(event_id, reader_id, entry_id, feed_id, session_id, event_type,",
            "happened_at, surface, position, payload)"
          ),
          paste(
            "SELECT $1, $2, entry_id, feed_id, $4, $5, $6, $7, $8, $9::jsonb",
            "FROM authorized"
          ),
          "ON CONFLICT (event_id) DO NOTHING RETURNING event_id)",
          paste(
            "SELECT EXISTS (SELECT 1 FROM existing) AS existing,",
            "EXISTS (SELECT 1 FROM authorized) AS authorized,",
            "EXISTS (SELECT 1 FROM inserted) AS inserted"
          )
        ),
        params = list(
          event$event_id,
          reader_id,
          event$entry_id,
          event$session_id,
          event$event_type,
          event$happened_at,
          event$surface,
          as.integer(event$position %||% NA_integer_),
          payload
        )
      )
      if (
        !isTRUE(result$existing[[1L]]) &&
          !isTRUE(result$authorized[[1L]])
      ) {
        store_abort_entry_forbidden()
      }
      inserted <- as.integer(isTRUE(result$inserted[[1L]]))
    }
    if (isTRUE(require_new) && !identical(inserted, 1L)) {
      cli::cli_abort(
        "The Reading History event ID is already in use.",
        class = "rill_event_id_conflict"
      )
    }
  } else {
    existing_index <- match(event$event_id, store$memory$events$event_id)
    existing_matches <- !is.na(existing_index) &&
      identical(
        store$memory$events$reader_id[[existing_index]],
        reader_id
      ) &&
      identical(
        store$memory$events$entry_id[[existing_index]],
        event$entry_id %||% NA_character_
      )
    if (existing_matches) {
      if (isTRUE(require_new)) {
        cli::cli_abort(
          "The Reading History event ID is already in use.",
          class = "rill_event_id_conflict"
        )
      }
      return(invisible(event))
    }
    feed_id <- if (is.null(event$entry_id)) {
      NA_character_
    } else {
      store_entry_feed_for_reader(store, reader_id, event$entry_id)
    }
    if (!is.na(existing_index)) {
      if (isTRUE(require_new)) {
        cli::cli_abort(
          "The Reading History event ID is already in use.",
          class = "rill_event_id_conflict"
        )
      }
      return(invisible(event))
    }
    row <- data.frame(
      event_id = event$event_id,
      reader_id = reader_id,
      entry_id = event$entry_id %||% NA_character_,
      feed_id = feed_id,
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
