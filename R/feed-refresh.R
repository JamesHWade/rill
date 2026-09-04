start_feed_refresh <- function(
  config,
  store,
  reader_id,
  feed_ids = NULL,
  failed_only = FALSE
) {
  request <- list(
    reader_id = reader_id,
    feed_ids = feed_ids,
    failed_only = failed_only
  )
  memory <- identical(store$mode, "memory")
  if (memory && isTRUE(store$memory$feed_poll_locked)) {
    return(list(
      result = list(
        status = "skipped_overlap",
        due_count = 0L,
        failed_count = 0L
      )
    ))
  }
  if (memory) {
    feeds <- store_list_feeds(store, reader_id, source_kind = "subscription")
    if (!is.null(feed_ids)) {
      feeds <- feeds[feeds$feed_id %in% feed_ids, , drop = FALSE]
    }
    if (failed_only) {
      feeds <- feeds[feeds$poll_status %in% "failed", , drop = FALSE]
    }
    request$feeds <- feeds
    request$memory <- list(
      feeds = store$memory$feeds[
        store$memory$feeds$feed_id %in% feeds$feed_id,
        ,
        drop = FALSE
      ],
      entries = store$memory$entries[
        store$memory$entries$feed_id %in% feeds$feed_id,
        ,
        drop = FALSE
      ],
      feed_poll_runs = store$memory$feed_poll_runs[FALSE, , drop = FALSE],
      feed_poll_outcomes = store$memory$feed_poll_outcomes[
        FALSE,
        ,
        drop = FALSE
      ]
    )
    store$memory$feed_poll_locked <- TRUE
  } else {
    request$database_url <- config$database_url
  }
  directory <- tempfile("rill-feed-refresh-")
  dir.create(directory, mode = "0700")
  launched <- FALSE
  on.exit(
    {
      if (!launched) {
        unlink(directory, recursive = TRUE)
        if (memory) store$memory$feed_poll_locked <- FALSE
      }
    },
    add = TRUE
  )
  package_path <- getNamespaceInfo(asNamespace("rill"), "path")
  development <- file.exists(file.path(package_path, "R", "feed-refresh.R"))
  process <- launch_feed_refresh(request, directory, package_path, development)
  launched <- TRUE
  list2env(
    list(
      process = process,
      directory = directory,
      store = store,
      started_at = Sys.time(),
      closed = FALSE
    ),
    parent = emptyenv()
  )
}

launch_feed_refresh <- function(request, directory, package_path, development) {
  callr::r_bg(
    function(request, directory, package_path, development) {
      if (development) {
        pkgload::load_all(
          package_path,
          export_all = FALSE,
          helpers = FALSE,
          quiet = TRUE
        )
      } else {
        loadNamespace("rill", lib.loc = dirname(package_path))
      }
      get("run_background_feed_refresh", asNamespace("rill"))(
        request,
        directory
      )
    },
    args = list(request, directory, package_path, development),
    supervise = TRUE,
    user_profile = FALSE,
    stdout = file.path(directory, "stdout"),
    stderr = file.path(directory, "stderr")
  )
}

run_background_feed_refresh <- function(
  request,
  directory,
  refresh = refresh_feed
) {
  if (is.null(request$memory)) {
    database_pool <- do.call(
      pool::dbPool,
      c(
        list(drv = RPostgres::Postgres()),
        postgres_connection_args(request$database_url),
        list(minSize = 1, maxSize = 3, idleTimeout = 60)
      )
    )
    store <- structure(
      list(mode = "postgres", pool = database_pool),
      class = "rill_store"
    )
    on.exit(rill_store_close(store), add = TRUE)
  } else {
    store <- structure(
      list(
        mode = "memory",
        memory = list2env(request$memory, parent = emptyenv())
      ),
      class = "rill_store"
    )
  }
  progress <- function(index, total, title) {
    path <- file.path(directory, "progress")
    saveRDS(
      list(status = "running", index = index, total = total, title = title),
      paste0(path, ".tmp")
    )
    file.rename(paste0(path, ".tmp"), path)
    invisible(NULL)
  }
  result <- if (is.null(request$memory)) {
    refresh_reader_feeds(
      store,
      request$reader_id,
      request$feed_ids,
      request$failed_only,
      progress,
      refresh
    )
  } else {
    run_feed_polling(
      store,
      select_feeds = \() request$feeds,
      failure_threshold = 1L,
      refresh = refresh,
      progress = progress
    )
  }
  if (!is.null(request$memory)) {
    entries <- store$memory$entries
    previous <- request$memory$entries
    changed <- vapply(
      seq_len(nrow(entries)),
      function(index) {
        old <- match(entries$entry_id[[index]], previous$entry_id)
        is.na(old) ||
          !identical(
            as.list(entries[index, , drop = FALSE]),
            as.list(previous[old, , drop = FALSE])
          )
      },
      logical(1)
    )
    result$acquisition <- list(
      feeds = store$memory$feeds,
      entries = entries[changed, , drop = FALSE],
      feed_poll_runs = store$memory$feed_poll_runs,
      feed_poll_outcomes = store$memory$feed_poll_outcomes
    )
  }
  result
}

poll_feed_refresh <- function(job) {
  if (!is.null(job$result)) {
    return(job$result)
  }
  if (
    job$process$is_alive() &&
      difftime(Sys.time(), job$started_at, units = "mins") < 60
  ) {
    path <- file.path(job$directory, "progress")
    if (!file.exists(path)) {
      return(list(status = "running"))
    }
    return(tryCatch(
      readRDS(path),
      error = \(error) list(status = "running")
    ))
  }
  on.exit(close_feed_refresh(job), add = TRUE)
  if (job$process$is_alive()) {
    job$result <- list(status = "error")
    return(job$result)
  }
  result <- tryCatch(job$process$get_result(), error = \(error) {
    list(status = "error")
  })
  if (!is.null(result$acquisition)) {
    apply_feed_refresh_acquisition(job$store, result$acquisition)
    result$acquisition <- NULL
  }
  job$result <- result
  result
}

apply_feed_refresh_acquisition <- function(store, acquisition) {
  for (index in seq_len(nrow(acquisition$feeds))) {
    row <- acquisition$feeds[index, , drop = FALSE]
    current <- match(row$feed_id, store$memory$feeds$feed_id)
    if (!is.na(current)) {
      row$folder <- store$memory$feeds$folder[[current]]
      store$memory$feeds[current, ] <- row
    }
  }
  store_upsert_entries(store, acquisition$entries)
  store$memory$feed_poll_runs <- rbind(
    store$memory$feed_poll_runs,
    acquisition$feed_poll_runs
  )
  store$memory$feed_poll_outcomes <- rbind(
    store$memory$feed_poll_outcomes,
    acquisition$feed_poll_outcomes
  )
  invisible(NULL)
}

close_feed_refresh <- function(job) {
  if (is.null(job$process) || isTRUE(job$closed)) {
    return(invisible(NULL))
  }
  if (job$process$is_alive()) {
    job$process$kill()
  }
  unlink(job$directory, recursive = TRUE)
  if (identical(job$store$mode, "memory")) {
    job$store$memory$feed_poll_locked <- FALSE
  }
  job$closed <- TRUE
  invisible(NULL)
}
