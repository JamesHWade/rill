empty_feed_groups <- function() {
  data.frame(
    reader_id = character(),
    group_id = character(),
    name = character()
  )
}

empty_subscription_groups <- function() {
  data.frame(
    reader_id = character(),
    feed_id = character(),
    group_id = character()
  )
}

store_list_groups <- function(store, reader_id) {
  if (identical(store$mode, "postgres")) {
    return(DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT reader_id, group_id, name FROM feed_groups",
        "WHERE reader_id = $1 ORDER BY lower(name), name"
      ),
      params = list(reader_id)
    ))
  }
  rows <- store$memory$feed_groups
  rows <- rows[rows$reader_id == reader_id, , drop = FALSE]
  rows[order(tolower(rows$name), rows$name), , drop = FALSE]
}

store_group_memberships <- function(store, reader_id) {
  if (identical(store$mode, "postgres")) {
    return(DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT reader_id, feed_id, group_id FROM subscription_groups",
        "WHERE reader_id = $1"
      ),
      params = list(reader_id)
    ))
  }
  rows <- store$memory$subscription_groups
  rows[rows$reader_id == reader_id, , drop = FALSE]
}

normalize_group_name <- function(name) {
  if (!store_scalar_string(name)) {
    cli::cli_abort(
      "Enter a non-empty Group name.",
      class = "rill_group_invalid"
    )
  }
  trimws(name)
}

store_create_group <- function(store, reader_id, name) {
  name <- normalize_group_name(name)
  group_id <- rill_id("group", reader_id, name, utc_now(), stats::runif(1))
  if (identical(store$mode, "postgres")) {
    row <- DBI::dbGetQuery(
      store$pool,
      paste(
        "INSERT INTO feed_groups (reader_id, group_id, name) VALUES ($1, $2, $3)",
        "ON CONFLICT (reader_id, name) DO UPDATE SET name = EXCLUDED.name",
        "RETURNING group_id"
      ),
      params = list(reader_id, group_id, name)
    )
    return(row$group_id[[1]])
  }
  store_ensure_reader(store, reader_id)
  rows <- store_list_groups(store, reader_id)
  if (name %in% rows$name) {
    return(rows$group_id[match(name, rows$name)])
  }
  store$memory$feed_groups <- rbind(
    store$memory$feed_groups,
    data.frame(reader_id = reader_id, group_id = group_id, name = name)
  )
  group_id
}

store_lock_group_subscriptions <- function(connection, reader_id) {
  DBI::dbGetQuery(
    connection,
    "SELECT feed_id FROM subscriptions WHERE reader_id = $1 ORDER BY feed_id FOR UPDATE",
    params = list(reader_id)
  )
}

store_project_group_folder <- function(connection, reader_id, feed_ids = NULL) {
  scope <- if (is.null(feed_ids)) {
    ""
  } else {
    paste0(
      " AND s.feed_id IN (",
      paste0("$", seq_along(feed_ids) + 1L, collapse = ","),
      ")"
    )
  }
  DBI::dbExecute(
    connection,
    paste(
      "UPDATE subscriptions s SET folder = COALESCE((",
      "SELECT g.name FROM subscription_groups m JOIN feed_groups g",
      "ON g.reader_id = m.reader_id AND g.group_id = m.group_id",
      "WHERE m.reader_id = s.reader_id AND m.feed_id = s.feed_id",
      "ORDER BY lower(g.name), g.name LIMIT 1), 'Unsorted')",
      "WHERE s.reader_id = $1",
      scope
    ),
    params = c(list(reader_id), as.list(feed_ids))
  )
}

memory_project_group_folder <- function(store, reader_id) {
  groups <- store_list_groups(store, reader_id)
  members <- store_group_memberships(store, reader_id)
  rows <- store$memory$subscriptions
  for (i in which(rows$reader_id == reader_id)) {
    ids <- members$group_id[members$feed_id == rows$feed_id[[i]]]
    names <- groups$name[groups$group_id %in% ids]
    rows$folder[[i]] <- if (length(names)) names[[1]] else "Unsorted"
  }
  store$memory$subscriptions <- rows
}

store_rename_group <- function(store, reader_id, group_id, name) {
  name <- normalize_group_name(name)
  groups <- store_list_groups(store, reader_id)
  if (!group_id %in% groups$group_id) {
    cli::cli_abort("That Group no longer exists.", class = "rill_group_missing")
  }
  if (any(groups$name == name & groups$group_id != group_id)) {
    cli::cli_abort(
      "A Group with that name already exists.",
      class = "rill_group_exists"
    )
  }
  if (identical(store$mode, "postgres")) {
    pool::poolWithTransaction(store$pool, function(connection) {
      store_lock_group_subscriptions(connection, reader_id)
      DBI::dbExecute(
        connection,
        "UPDATE feed_groups SET name = $3 WHERE reader_id = $1 AND group_id = $2",
        params = list(reader_id, group_id, name)
      )
      store_project_group_folder(connection, reader_id)
    })
  } else {
    rows <- store$memory$feed_groups
    rows$name[rows$reader_id == reader_id & rows$group_id == group_id] <- name
    store$memory$feed_groups <- rows
    memory_project_group_folder(store, reader_id)
  }
  invisible(group_id)
}

store_delete_group <- function(store, reader_id, group_id) {
  if (identical(store$mode, "postgres")) {
    pool::poolWithTransaction(store$pool, function(connection) {
      store_lock_group_subscriptions(connection, reader_id)
      DBI::dbExecute(
        connection,
        "DELETE FROM feed_groups WHERE reader_id = $1 AND group_id = $2",
        params = list(reader_id, group_id)
      )
      store_project_group_folder(connection, reader_id)
    })
  } else {
    rows <- store$memory$feed_groups
    store$memory$feed_groups <- rows[
      !(rows$reader_id == reader_id & rows$group_id == group_id),
      ,
      drop = FALSE
    ]
    rows <- store$memory$subscription_groups
    store$memory$subscription_groups <- rows[
      !(rows$reader_id == reader_id & rows$group_id == group_id),
      ,
      drop = FALSE
    ]
    memory_project_group_folder(store, reader_id)
  }
  invisible(group_id)
}

store_update_group_memberships <- function(
  store,
  reader_id,
  feed_ids,
  group_ids,
  action = "replace"
) {
  action <- match.arg(action, c("replace", "add", "remove"))
  for (ids in list(feed_ids, group_ids)) {
    if (!is.character(ids) || anyNA(ids) || !all(nzchar(ids))) {
      cli::cli_abort(
        "Choose valid feeds and Groups.",
        class = "rill_group_invalid"
      )
    }
  }
  feed_ids <- unique(feed_ids)
  group_ids <- unique(group_ids)
  groups <- store_list_groups(store, reader_id)
  if (!all(group_ids %in% groups$group_id)) {
    cli::cli_abort("That Group no longer exists.", class = "rill_group_missing")
  }
  if (!length(feed_ids)) {
    return(invisible(NULL))
  }
  if (identical(store$mode, "postgres")) {
    pool::poolWithTransaction(store$pool, function(connection) {
      # Lock all selected Subscriptions before changing any membership.
      for (id in sort(feed_ids)) {
        row <- DBI::dbGetQuery(
          connection,
          paste(
            "SELECT feed_id FROM subscriptions WHERE reader_id = $1",
            "AND feed_id = $2 AND status = 'active' FOR UPDATE"
          ),
          params = list(reader_id, id)
        )
        if (nrow(row) != 1L) {
          cli::cli_abort(
            "That Subscription is not active.",
            class = "rill_subscription_inactive"
          )
        }
      }
      for (id in feed_ids) {
        if (action == "replace") {
          DBI::dbExecute(
            connection,
            paste(
              "DELETE FROM subscription_groups WHERE reader_id = $1 AND feed_id = $2"
            ),
            params = list(reader_id, id)
          )
        }
        for (group in group_ids) {
          sql <- if (action == "remove") {
            paste(
              "DELETE FROM subscription_groups WHERE reader_id = $1",
              "AND feed_id = $2 AND group_id = $3"
            )
          } else {
            paste(
              "INSERT INTO subscription_groups (reader_id, feed_id, group_id)",
              "VALUES ($1, $2, $3) ON CONFLICT DO NOTHING"
            )
          }
          DBI::dbExecute(connection, sql, params = list(reader_id, id, group))
        }
      }
      store_project_group_folder(connection, reader_id, feed_ids)
    })
  } else {
    rows <- store$memory$subscriptions
    active <- rows$feed_id[
      rows$reader_id == reader_id & rows$status == "active"
    ]
    if (!all(feed_ids %in% active)) {
      cli::cli_abort(
        "That Subscription is not active.",
        class = "rill_subscription_inactive"
      )
    }
    rows <- store$memory$subscription_groups
    selected <- rows$reader_id == reader_id & rows$feed_id %in% feed_ids
    remove <- selected &
      if (action == "replace") {
        TRUE
      } else {
        action == "remove" & rows$group_id %in% group_ids
      }
    rows <- rows[!remove, , drop = FALSE]
    if (action != "remove" && length(group_ids)) {
      added <- expand.grid(
        reader_id = reader_id,
        feed_id = feed_ids,
        group_id = group_ids,
        stringsAsFactors = FALSE
      )
      rows <- unique(rbind(rows, added))
    }
    store$memory$subscription_groups <- rows
    memory_project_group_folder(store, reader_id)
  }
  invisible(feed_ids)
}

store_set_legacy_folder <- function(store, reader_id, feed_id, folder) {
  active <- store_list_feeds(store, reader_id)$feed_id
  if (!feed_id %in% active) {
    cli::cli_abort(
      "That Subscription is not active.",
      class = "rill_subscription_inactive"
    )
  }
  ids <- if (tolower(folder) == "unsorted") {
    character()
  } else {
    store_create_group(store, reader_id, folder)
  }
  store_update_group_memberships(store, reader_id, feed_id, ids)
}

store_attach_feed_groups <- function(store, reader_id, feeds) {
  groups <- store_list_groups(store, reader_id)
  members <- store_group_memberships(store, reader_id)
  feeds$group_ids <- lapply(feeds$feed_id, function(id) {
    groups$group_id[
      groups$group_id %in% members$group_id[members$feed_id == id]
    ]
  })
  feeds$groups <- lapply(feeds$group_ids, \(ids) {
    groups$name[match(ids, groups$group_id)]
  })
  feeds
}

store_group_feed_ids <- function(
  store,
  reader_id,
  group_ids,
  group_match,
  ungrouped
) {
  group_match <- match.arg(group_match, c("any", "all"))
  members <- store_group_memberships(store, reader_id)
  if (ungrouped) {
    feeds <- if (identical(store$mode, "postgres")) {
      DBI::dbGetQuery(
        store$pool,
        "SELECT feed_id FROM subscriptions WHERE reader_id = $1 AND status = 'active'",
        params = list(reader_id)
      )$feed_id
    } else {
      rows <- store$memory$subscriptions
      rows$feed_id[rows$reader_id == reader_id & rows$status == "active"]
    }
    return(setdiff(feeds, members$feed_id))
  }
  if (!length(group_ids)) {
    return(NULL)
  }
  ids <- unique(group_ids)
  matches <- members[members$group_id %in% ids, , drop = FALSE]
  if (group_match == "any") {
    return(unique(matches$feed_id))
  }
  counts <- table(matches$feed_id)
  as.character(names(counts)[counts == length(ids)])
}

group_filter_sql <- function(group_ids, group_match, ungrouped, parameters) {
  base <- "m.reader_id = $1 AND m.feed_id = e.feed_id"
  if (isTRUE(ungrouped)) {
    return(list(
      sql = paste(
        "NOT EXISTS (SELECT 1 FROM subscription_groups m WHERE",
        base,
        ")"
      ),
      parameters = parameters
    ))
  }
  if (!length(group_ids)) {
    return(list(sql = NULL, parameters = parameters))
  }
  ids <- unique(group_ids)
  slots <- seq_along(ids) + length(parameters)
  where <- paste(
    base,
    "AND m.group_id IN (",
    paste0("$", slots, collapse = ","),
    ")"
  )
  sql <- if (group_match == "all") {
    paste(
      "(SELECT COUNT(*) FROM subscription_groups m WHERE",
      where,
      ") =",
      length(ids)
    )
  } else {
    paste("EXISTS (SELECT 1 FROM subscription_groups m WHERE", where, ")")
  }
  list(sql = sql, parameters = c(parameters, as.list(ids)))
}
