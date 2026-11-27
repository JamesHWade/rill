memory_reader_visible_entry_ids <- function(store, reader_id) {
  subscriptions <- store$memory$subscriptions
  active_feed_ids <- subscriptions$feed_id[
    subscriptions$reader_id == reader_id &
      subscriptions$status == "active"
  ]
  entries <- store$memory$entries
  visible <- entries$feed_id %in% active_feed_ids
  capture_feed_ids <- store$memory$feeds$feed_id[
    store$memory$feeds$source_kind == "capture"
  ]
  capture_entries <- entries$feed_id %in% capture_feed_ids
  owned_entry_ids <- vapply(
    Filter(
      \(document) identical(document$reader_id, reader_id),
      store$memory$documents
    ),
    `[[`,
    character(1),
    "entry_id"
  )
  entries$entry_id[
    visible & (!capture_entries | entries$entry_id %in% owned_entry_ids)
  ]
}

postgres_capture_entry_visible_sql <- function(
  reader_parameter,
  entry_alias = "e",
  feed_alias = "f"
) {
  paste0(
    "(",
    feed_alias,
    ".source_kind <> 'capture' OR EXISTS (",
    "SELECT 1 FROM documents private_document ",
    "WHERE private_document.entry_id = ",
    entry_alias,
    ".entry_id AND private_document.reader_id = ",
    reader_parameter,
    "))"
  )
}

store_list_feeds <- function(
  store,
  reader_id,
  source_kind = NULL,
  active_only = TRUE
) {
  if (identical(store$mode, "postgres")) {
    clauses <- c("s.reader_id = $1")
    if (isTRUE(active_only)) {
      clauses <- c(clauses, "s.status = 'active'")
    }
    feeds <- DBI::dbGetQuery(
      store$pool,
      paste(
        paste(
          "SELECT f.feed_id, f.feed_url, f.site_url, f.title, s.folder,",
          "f.source_kind, f.etag, f.last_modified, f.poll_status,",
          "f.last_polled_at, f.created_at, s.display_title, s.status,",
          "s.subscribed_at, s.updated_at, s.deactivated_at,"
        ),
        "COUNT(e.entry_id) FILTER (WHERE st.read_at IS NULL AND NOT COALESCE(st.hidden, false))::integer AS unread_count,",
        "COUNT(e.entry_id) FILTER (WHERE NOT COALESCE(st.hidden, false))::integer AS entry_count",
        "FROM subscriptions s",
        "JOIN feeds f ON f.feed_id = s.feed_id",
        paste(
          "LEFT JOIN entries e ON e.feed_id = f.feed_id AND",
          postgres_capture_entry_visible_sql("$1")
        ),
        paste(
          "LEFT JOIN entry_state st ON st.entry_id = e.entry_id",
          "AND st.reader_id = $1"
        ),
        "WHERE",
        paste(clauses, collapse = " AND "),
        "GROUP BY f.feed_id, s.reader_id, s.folder, s.display_title,",
        "s.status, s.subscribed_at, s.updated_at, s.deactivated_at",
        paste(
          "ORDER BY lower(s.folder),",
          "lower(COALESCE(NULLIF(s.display_title, ''), f.title))"
        )
      ),
      params = list(reader_id)
    )
    feeds <- resolve_feed_titles(feeds)
    if (!is.null(source_kind)) {
      feeds <- feeds[feeds$source_kind %in% source_kind, , drop = FALSE]
    }
    return(store_attach_feed_groups(store, reader_id, feeds))
  }

  feeds <- store$memory$feeds
  subscriptions <- store$memory$subscriptions
  subscriptions <- subscriptions[
    subscriptions$reader_id == reader_id &
      (!isTRUE(active_only) | subscriptions$status == "active"),
    ,
    drop = FALSE
  ]
  feeds <- merge(
    feeds,
    subscriptions,
    by = "feed_id",
    all = FALSE,
    sort = FALSE,
    suffixes = c("", "_subscription")
  )
  feeds$folder <- feeds$folder_subscription
  feeds$folder_subscription <- NULL
  feeds <- resolve_feed_titles(feeds)
  visible_entry_ids <- memory_reader_visible_entry_ids(store, reader_id)
  hidden_ids <- store$memory$state$entry_id[
    store$memory$state$reader_id == reader_id &
      store$memory$state$hidden %in% TRUE
  ]
  visible_entry_ids <- setdiff(visible_entry_ids, hidden_ids)
  feeds$unread_count <- vapply(
    feeds$feed_id,
    function(feed_id) {
      ids <- store$memory$entries$entry_id[
        store$memory$entries$feed_id == feed_id &
          store$memory$entries$entry_id %in% visible_entry_ids
      ]
      read_ids <- store$memory$state$entry_id[
        store$memory$state$reader_id == reader_id &
          !is.na(store$memory$state$read_at) &
          nzchar(store$memory$state$read_at)
      ]
      sum(!ids %in% read_ids)
    },
    integer(1)
  )
  feeds$entry_count <- vapply(
    feeds$feed_id,
    function(feed_id) {
      sum(
        store$memory$entries$feed_id == feed_id &
          store$memory$entries$entry_id %in% visible_entry_ids
      )
    },
    integer(1)
  )
  if (!is.null(source_kind)) {
    feeds <- feeds[feeds$source_kind %in% source_kind, , drop = FALSE]
  }
  feeds <- feeds[
    order(tolower(feeds$folder), tolower(feeds$title)),
    ,
    drop = FALSE
  ]
  store_attach_feed_groups(store, reader_id, feeds)
}

store_list_active_feeds <- function(store, source_kind = "subscription") {
  if (identical(store$mode, "postgres")) {
    return(DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT f.* FROM feeds f",
        "WHERE f.source_kind = $1 AND EXISTS (",
        "SELECT 1 FROM subscriptions s",
        "JOIN readers r ON r.reader_id = s.reader_id AND r.status = 'active'",
        "WHERE s.feed_id = f.feed_id AND s.status = 'active'",
        ") ORDER BY lower(f.title), f.feed_id"
      ),
      params = list(source_kind)
    ))
  }

  readers <- store$memory$readers
  active_reader_ids <- readers$reader_id[readers$status == "active"]
  subscriptions <- store$memory$subscriptions
  active_feed_ids <- unique(subscriptions$feed_id[
    subscriptions$status == "active" &
      subscriptions$reader_id %in% active_reader_ids
  ])
  feeds <- store$memory$feeds[
    store$memory$feeds$feed_id %in%
      active_feed_ids &
      store$memory$feeds$source_kind == source_kind,
    ,
    drop = FALSE
  ]
  feeds[order(tolower(feeds$title), feeds$feed_id), , drop = FALSE]
}

store_subscribe_feed <- function(
  store,
  reader_id,
  feed_id,
  folder = NULL,
  now = utc_now()
) {
  folder <- if (is.null(folder)) NULL else normalize_subscription_folder(folder)
  if (identical(store$mode, "postgres")) {
    subscribed <- DBI::dbGetQuery(
      store$pool,
      paste(
        "INSERT INTO subscriptions (",
        paste(
          "reader_id, feed_id, folder, status, subscribed_at, updated_at,",
          "deactivated_at"
        ),
        ") SELECT $1, f.feed_id, COALESCE($3, 'Unsorted'), 'active',",
        "$4, $4, NULL FROM feeds f WHERE f.feed_id = $2",
        "ON CONFLICT (reader_id, feed_id) DO UPDATE SET",
        "folder = COALESCE($3, subscriptions.folder), status = 'active',",
        "updated_at = $4, deactivated_at = NULL",
        "RETURNING feed_id"
      ),
      params = list(reader_id, feed_id, folder %||% NA_character_, now)
    )
    if (nrow(subscribed) != 1L) {
      cli::cli_abort(
        "That feed no longer exists.",
        class = "rill_feed_missing"
      )
    }
    if (!is.null(folder)) {
      store_set_legacy_folder(store, reader_id, feed_id, folder)
    }
    return(invisible(feed_id))
  }

  if (!feed_id %in% store$memory$feeds$feed_id) {
    cli::cli_abort(
      "That feed no longer exists.",
      class = "rill_feed_missing"
    )
  }
  subscriptions <- store$memory$subscriptions
  index <- which(
    subscriptions$reader_id == reader_id &
      subscriptions$feed_id == feed_id
  )
  if (length(index)) {
    index <- index[[1L]]
    if (!is.null(folder)) {
      subscriptions$folder[[index]] <- folder
    }
    subscriptions$status[[index]] <- "active"
    subscriptions$updated_at[[index]] <- now
    subscriptions$deactivated_at[[index]] <- NA_character_
    store$memory$subscriptions <- subscriptions
    if (!is.null(folder)) {
      store_set_legacy_folder(store, reader_id, feed_id, folder)
    }
    return(invisible(feed_id))
  }
  store$memory$subscriptions <- rbind(
    subscriptions,
    data.frame(
      reader_id = reader_id,
      feed_id = feed_id,
      folder = folder %||% "Unsorted",
      display_title = NA_character_,
      status = "active",
      subscribed_at = now,
      updated_at = now,
      deactivated_at = NA_character_,
      stringsAsFactors = FALSE
    )
  )
  if (!is.null(folder)) {
    store_set_legacy_folder(store, reader_id, feed_id, folder)
  }
  invisible(feed_id)
}

store_unsubscribe_feed <- function(
  store,
  reader_id,
  feed_id,
  now = utc_now()
) {
  if (identical(store$mode, "postgres")) {
    source <- DBI::dbGetQuery(
      store$pool,
      "SELECT source_kind FROM feeds WHERE feed_id = $1",
      params = list(feed_id)
    )
    if (nrow(source) && !identical(source$source_kind[[1L]], "subscription")) {
      cli::cli_abort(
        "Only subscribed Feeds can be unsubscribed.",
        class = "rill_subscription_source_invalid"
      )
    }
    updated <- DBI::dbExecute(
      store$pool,
      paste(
        "UPDATE subscriptions SET status = 'inactive',",
        "updated_at = $3, deactivated_at = $3",
        "WHERE reader_id = $1 AND feed_id = $2 AND status = 'active'"
      ),
      params = list(reader_id, feed_id, now)
    )
    if (!identical(updated, 1L)) {
      cli::cli_abort(
        "That Subscription is not active.",
        class = "rill_subscription_inactive"
      )
    }
    return(invisible(feed_id))
  }

  feed_index <- match(feed_id, store$memory$feeds$feed_id)
  if (
    !is.na(feed_index) &&
      !identical(store$memory$feeds$source_kind[[feed_index]], "subscription")
  ) {
    cli::cli_abort(
      "Only subscribed Feeds can be unsubscribed.",
      class = "rill_subscription_source_invalid"
    )
  }
  subscriptions <- store$memory$subscriptions
  index <- which(
    subscriptions$reader_id == reader_id &
      subscriptions$feed_id == feed_id &
      subscriptions$status == "active"
  )
  if (!length(index)) {
    cli::cli_abort(
      "That Subscription is not active.",
      class = "rill_subscription_inactive"
    )
  }
  index <- index[[1L]]
  subscriptions$status[[index]] <- "inactive"
  subscriptions$updated_at[[index]] <- now
  subscriptions$deactivated_at[[index]] <- now
  store$memory$subscriptions <- subscriptions
  invisible(feed_id)
}

normalize_subscription_folder <- function(folder) {
  if (!store_scalar_string(folder)) {
    cli::cli_abort(
      "Subscription folders must be non-empty strings.",
      class = "rill_subscription_folder_invalid"
    )
  }
  trimws(folder)
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

store_find_feed_by_url <- function(store, feed_url) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      "SELECT * FROM feeds WHERE feed_url = $1",
      params = list(feed_url)
    )
    if (!nrow(rows)) {
      return(NULL)
    }
    return(as.list(rows[1L, , drop = FALSE]))
  }
  index <- match(feed_url, store$memory$feeds$feed_url)
  if (is.na(index)) {
    return(NULL)
  }
  as.list(store$memory$feeds[index, , drop = FALSE])
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
        "title = EXCLUDED.title, source_kind = EXCLUDED.source_kind,",
        "etag = EXCLUDED.etag, last_modified = EXCLUDED.last_modified,",
        "poll_status = EXCLUDED.poll_status, last_polled_at = now()"
      ),
      params = list(
        feed$feed_id,
        feed$feed_url,
        feed$site_url,
        feed$title,
        "Unsorted",
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
    folder = "Unsorted",
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
    row$folder <- store$memory$feeds$folder[[index[[1]]]]
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
        "UPDATE subscriptions s",
        "SET display_title = NULLIF($3, f.title), updated_at = now()",
        "FROM feeds f",
        "WHERE s.reader_id = $1 AND s.feed_id = $2",
        "AND s.status = 'active' AND f.feed_id = s.feed_id",
        "RETURNING s.feed_id"
      ),
      params = list(reader_id, feed_id, title)
    )
    if (nrow(updated) != 1L) {
      cli::cli_abort(
        "That Subscription is not active.",
        class = "rill_subscription_inactive"
      )
    }
    return(invisible(title))
  }

  feed_index <- match(feed_id, store$memory$feeds$feed_id)
  subscription_index <- which(
    store$memory$subscriptions$reader_id == reader_id &
      store$memory$subscriptions$feed_id == feed_id &
      store$memory$subscriptions$status == "active"
  )
  if (is.na(feed_index) || !length(subscription_index)) {
    cli::cli_abort(
      "That Subscription is not active.",
      class = "rill_subscription_inactive"
    )
  }
  source_title <- store$memory$feeds$title[[feed_index]]
  display_title <- if (identical(title, source_title)) {
    NA_character_
  } else {
    title
  }
  subscription_index <- subscription_index[[1L]]
  store$memory$subscriptions$display_title[[subscription_index]] <-
    display_title
  store$memory$subscriptions$updated_at[[subscription_index]] <- utc_now()
  invisible(title)
}

store_move_feed <- function(store, reader_id, feed_id, folder) {
  folder <- normalize_subscription_folder(folder)
  store_set_legacy_folder(store, reader_id, feed_id, folder)
  invisible(folder)
}
