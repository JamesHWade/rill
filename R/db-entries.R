store_list_entries <- function(
  store,
  reader_id,
  view = "unread",
  feed_id = NULL,
  limit = 100L,
  sort = "newest",
  now = Sys.time(),
  timezone = Sys.timezone(),
  include_content = TRUE,
  entry_ids = NULL,
  folder = NULL,
  group_ids = NULL,
  group_match = "any",
  ungrouped = FALSE
) {
  group_match <- match.arg(group_match, c("any", "all"))
  view <- normalize_entry_view(view)
  sort <- normalize_entry_sort(sort)
  limit <- max(1L, as.integer(limit))
  window <- entry_view_window(view, now, timezone)

  if (identical(store$mode, "postgres")) {
    parameters <- list(reader_id)
    clauses <- c(
      "COALESCE(s.hidden, false) = false",
      postgres_capture_entry_visible_sql("$1")
    )

    if (!is.null(entry_ids)) {
      if (length(entry_ids)) {
        slots <- seq_along(entry_ids) + length(parameters)
        clauses <- c(
          clauses,
          paste0(
            "e.entry_id IN (",
            paste0("$", slots, collapse = ", "),
            ")"
          )
        )
        parameters <- c(parameters, as.list(entry_ids))
      } else {
        clauses <- c(clauses, "FALSE")
      }
    }

    if (!is.null(feed_id) && nzchar(feed_id)) {
      parameters <- append(parameters, feed_id)
      clauses <- c(clauses, paste0("e.feed_id = $", length(parameters)))
    }
    if (!is.null(folder)) {
      parameters <- append(parameters, folder)
      clauses <- c(clauses, paste0("sub.folder = $", length(parameters)))
    }
    group_filter <- group_filter_sql(
      group_ids,
      group_match,
      ungrouped,
      parameters
    )
    clauses <- c(clauses, group_filter$sql)
    parameters <- group_filter$parameters
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

    columns <- if (include_content) {
      "e.*"
    } else {
      paste(
        paste0("e.", setdiff(names(empty_entries()), "feed_content")),
        collapse = ", "
      )
    }
    query <- paste(
      "SELECT",
      columns,
      ",",
      paste(
        "COALESCE(NULLIF(sub.display_title, ''), f.title) AS feed_title,",
        "f.title AS source_feed_title, sub.folder,"
      ),
      "s.read_at, s.read_reason, COALESCE(s.starred, false) AS starred,",
      "COALESCE(s.saved, false) AS saved, s.last_opened_at",
      "FROM entries e",
      "JOIN feeds f ON f.feed_id = e.feed_id",
      paste(
        "JOIN subscriptions sub ON sub.feed_id = e.feed_id",
        "AND sub.reader_id = $1 AND sub.status = 'active'"
      ),
      "LEFT JOIN entry_state s ON s.entry_id = e.entry_id AND s.reader_id = $1",
      "WHERE",
      paste(clauses, collapse = " AND "),
      "ORDER BY",
      entry_sort_sql(sort),
      "LIMIT",
      limit
    )
    return(DBI::dbGetQuery(store$pool, query, params = parameters))
  }

  memory_feeds <- store_list_feeds(store, reader_id)
  entries <- merge(
    store$memory$entries,
    memory_feeds[c("feed_id", "title", "source_title", "folder")],
    by = "feed_id",
    all = FALSE,
    sort = FALSE,
    suffixes = c("", "_feed")
  )
  names(entries)[names(entries) == "title_feed"] <- "feed_title"
  names(entries)[names(entries) == "source_title"] <- "source_feed_title"
  entries <- entries[
    entries$entry_id %in% memory_reader_visible_entry_ids(store, reader_id),
    ,
    drop = FALSE
  ]

  state <- store$memory$state[
    store$memory$state$reader_id == reader_id,
    ,
    drop = FALSE
  ]
  if (nrow(state)) {
    entries <- merge(
      entries,
      state,
      by = c("entry_id", "feed_id"),
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    entries$read_at <- rep(NA_character_, nrow(entries))
    entries$read_reason <- rep(NA_character_, nrow(entries))
    entries$starred <- rep(FALSE, nrow(entries))
    entries$saved <- rep(FALSE, nrow(entries))
    entries$hidden <- rep(FALSE, nrow(entries))
    entries$last_opened_at <- rep(NA_character_, nrow(entries))
  }
  entries$starred[is.na(entries$starred)] <- FALSE
  entries$saved[is.na(entries$saved)] <- FALSE
  entries$hidden[is.na(entries$hidden)] <- FALSE

  keep <- !entries$hidden
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
  if (!is.null(folder)) {
    keep <- keep & entries$folder == folder
  }
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
  if (!is.null(entry_ids)) {
    entries <- entries[entries$entry_id %in% entry_ids, , drop = FALSE]
  }
  if (!include_content) {
    entries$feed_content <- NULL
  }
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

reader_calendar_now <- function() {
  Sys.time()
}

normalize_reader_timezone <- function(timezone) {
  if (!rlang::is_string(timezone) || !timezone %in% OlsonNames()) {
    return("UTC")
  }
  timezone
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
        "lower(COALESCE(NULLIF(sub.display_title, ''), f.title)) ",
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

store_get_entry <- function(store, reader_id, entry_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT e.*,",
        paste(
          "COALESCE(NULLIF(sub.display_title, ''), f.title) AS feed_title,",
          "f.title AS source_feed_title, f.site_url,"
        ),
        "s.read_at, s.read_reason, COALESCE(s.starred, false) AS starred,",
        "COALESCE(s.saved, false) AS saved, s.last_opened_at",
        "FROM entries e JOIN feeds f ON f.feed_id = e.feed_id",
        paste(
          "JOIN subscriptions sub ON sub.feed_id = e.feed_id",
          "AND sub.reader_id = $1 AND sub.status = 'active'"
        ),
        "LEFT JOIN entry_state s ON s.entry_id = e.entry_id",
        "AND s.reader_id = $1",
        paste(
          "WHERE e.entry_id = $2 AND",
          postgres_capture_entry_visible_sql("$1")
        )
      ),
      params = list(reader_id, entry_id)
    )
    if (!nrow(rows)) {
      return(NULL)
    }
    return(as.list(rows[1, , drop = FALSE]))
  }

  entries <- store_list_entries(store, reader_id, view = "all", limit = 500L)
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

store_get_entry_for_document_pin <- function(store, reader_id, document_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT e.*,",
        paste(
          "COALESCE(NULLIF(sub.display_title, ''), f.title) AS feed_title,",
          "f.title AS source_feed_title, f.site_url, sub.folder,"
        ),
        "s.read_at, s.read_reason, COALESCE(s.starred, false) AS starred,",
        "COALESCE(s.saved, false) AS saved, s.last_opened_at",
        "FROM documents d JOIN entries e ON e.entry_id = d.entry_id",
        "JOIN feeds f ON f.feed_id = e.feed_id",
        paste(
          "LEFT JOIN subscriptions sub ON sub.feed_id = e.feed_id",
          "AND sub.reader_id = $1"
        ),
        "LEFT JOIN entry_state s ON s.entry_id = e.entry_id",
        "AND s.reader_id = $1",
        "WHERE d.document_id = $2 AND (d.reader_id = $1 OR (",
        "d.reader_id IS NULL AND (EXISTS (SELECT 1 FROM agent_runs ar",
        paste(
          "WHERE ar.reader_id = $1",
          "AND (ar.pinned_inputs ->> 'document_id' = $2",
          paste(
            "OR COALESCE(ar.pinned_inputs -> 'document_ids', '[]'::jsonb)",
            "? $2))"
          )
        ),
        "OR EXISTS (SELECT 1 FROM orientations o WHERE o.reader_id = $1",
        paste(
          "AND o.payload -> 'cards' @> jsonb_build_array(",
          "jsonb_build_object('document_id', d.document_id))))))"
        )
      ),
      params = list(reader_id, document_id)
    )
    if (!nrow(rows)) {
      return(NULL)
    }
    return(as.list(rows[1L, , drop = FALSE]))
  }

  document <- store_get_document_by_id(store, reader_id, document_id)
  if (is.null(document)) {
    return(NULL)
  }
  if (
    is.na(document$reader_id) &&
      !memory_document_is_pinned(store, reader_id, document_id)
  ) {
    return(NULL)
  }
  entry_index <- match(document$entry_id, store$memory$entries$entry_id)
  if (is.na(entry_index)) {
    return(NULL)
  }

  entry <- as.list(store$memory$entries[entry_index, , drop = FALSE])
  feed_index <- match(entry$feed_id, store$memory$feeds$feed_id)
  subscription_index <- which(
    store$memory$subscriptions$reader_id == reader_id &
      store$memory$subscriptions$feed_id == entry$feed_id
  )
  state_index <- which(
    store$memory$state$reader_id == reader_id &
      store$memory$state$entry_id == entry$entry_id
  )
  feed <- store$memory$feeds[feed_index, , drop = FALSE]
  subscription <- store$memory$subscriptions[
    utils::tail(subscription_index, 1L),
    ,
    drop = FALSE
  ]
  entry$feed_title <- if (
    nrow(subscription) &&
      !is.na(subscription$display_title[[1L]]) &&
      nzchar(subscription$display_title[[1L]])
  ) {
    subscription$display_title[[1L]]
  } else {
    feed$title[[1L]]
  }
  entry$source_feed_title <- feed$title[[1L]]
  entry$site_url <- feed$site_url[[1L]]
  entry$folder <- if (nrow(subscription)) {
    subscription$folder[[1L]]
  } else {
    NA_character_
  }
  state_defaults <- list(
    read_at = NA_character_,
    read_reason = NA_character_,
    starred = FALSE,
    saved = FALSE,
    last_opened_at = NA_character_
  )
  for (field in names(state_defaults)) {
    entry[[field]] <- if (length(state_index)) {
      store$memory$state[[field]][[state_index[[1L]]]]
    } else {
      state_defaults[[field]]
    }
  }
  entry
}

store_find_entry_by_url <- function(
  store,
  reader_id,
  source_url,
  canonical_url = source_url
) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT e.* FROM entries e",
        "JOIN feeds f ON f.feed_id = e.feed_id",
        paste(
          "JOIN subscriptions s ON s.feed_id = e.feed_id",
          "AND s.reader_id = $1 AND s.status = 'active'"
        ),
        "WHERE (e.url IN ($2, $3) OR e.canonical_url IN ($2, $3))",
        paste("AND", postgres_capture_entry_visible_sql("$1")),
        "ORDER BY e.inserted_at, e.entry_id LIMIT 1"
      ),
      params = list(reader_id, source_url, canonical_url)
    )
    if (!nrow(rows)) {
      return(NULL)
    }
    return(as.list(rows[1, , drop = FALSE]))
  }

  entries <- store$memory$entries
  subscriptions <- store$memory$subscriptions
  feed_ids <- subscriptions$feed_id[
    subscriptions$reader_id == reader_id &
      subscriptions$status == "active"
  ]
  canonical_matches <- !is.na(entries$canonical_url) &
    entries$canonical_url %in% c(source_url, canonical_url)
  matches <- entries$feed_id %in%
    feed_ids &
    entries$entry_id %in% memory_reader_visible_entry_ids(store, reader_id) &
    (entries$url %in% c(source_url, canonical_url) | canonical_matches)
  row <- entries[matches, , drop = FALSE]
  if (!nrow(row)) {
    return(NULL)
  }
  as.list(row[1, , drop = FALSE])
}

store_upsert_entries <- function(store, entries) {
  entries <- entry_preview_columns(entries)
  if (!nrow(entries)) {
    return(invisible(0L))
  }

  if (identical(store$mode, "postgres")) {
    added <- pool::poolWithTransaction(store$pool, function(connection) {
      added <- 0L
      for (index in seq_len(nrow(entries))) {
        entry <- entries[index, , drop = FALSE]
        result <- DBI::dbGetQuery(
          connection,
          paste(
            "INSERT INTO entries",
            "(entry_id, feed_id, external_id, url, canonical_url, title, author,",
            "summary, feed_content, preview_image_url, preview_image_alt, published_at, content_hash)",
            "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)",
            "ON CONFLICT (feed_id, external_id) DO UPDATE SET",
            "url = EXCLUDED.url, canonical_url = EXCLUDED.canonical_url,",
            "title = EXCLUDED.title, author = EXCLUDED.author,",
            "summary = EXCLUDED.summary, feed_content = EXCLUDED.feed_content,",
            "preview_image_url = EXCLUDED.preview_image_url,",
            "preview_image_alt = EXCLUDED.preview_image_alt,",
            "published_at = EXCLUDED.published_at, content_hash = EXCLUDED.content_hash",
            "RETURNING (xmax = 0) AS inserted"
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
            "preview_image_url",
            "preview_image_alt",
            "published_at",
            "content_hash"
          )]))
        )
        added <- added + sum(result$inserted)
      }
      added
    })
    return(invisible(as.integer(added)))
  }

  added <- 0L
  for (index in seq_len(nrow(entries))) {
    entry <- entries[index, , drop = FALSE]
    existing <- which(
      store$memory$entries$feed_id == entry$feed_id &
        store$memory$entries$external_id == entry$external_id
    )
    if (length(existing)) {
      entry$inserted_at <- store$memory$entries$inserted_at[[existing[[1]]]]
      store$memory$entries[existing[[1]], ] <- entry[names(
        store$memory$entries
      )]
    } else {
      store$memory$entries <- rbind(store$memory$entries, entry)
      added <- added + 1L
    }
  }
  invisible(added)
}
