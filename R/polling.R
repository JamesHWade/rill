#' Poll due Feeds
#'
#' `poll_feeds()` refreshes each shared Feed with at least one active
#' Subscription when its polling interval has elapsed. Each run and per-Feed
#' outcome is recorded in the durable store. Concurrent runs are skipped, and
#' isolated Feed failures are tolerated until `RILL_POLL_FAILURE_THRESHOLD` is
#' reached.
#'
#' The due interval defaults to 60 minutes and can be changed with
#' `RILL_POLL_INTERVAL_MINUTES`. The failure threshold defaults to five Feeds.
#'
#' @return Invisibly, a polling-run summary with per-Feed outcomes.
#' @export
poll_feeds <- function() {
  config <- rill_config()
  if (config$demo_mode) {
    cli::cli_abort(c(
      "Can't poll feeds without a durable store.",
      "i" = "Set {.envvar DATABASE_URL} to a PostgreSQL connection string."
    ))
  }

  init_telemetry(config)
  store <- rill_store(config)
  on.exit(rill_store_close(store), add = TRUE)

  result <- run_due_feed_polling(
    store,
    interval_minutes = config$poll_interval_minutes,
    failure_threshold = config$poll_failure_threshold
  )
  if (identical(result$status, "skipped_overlap")) {
    cli::cli_inform(c(
      "i" = "Another Feed polling run is active; skipped."
    ))
    return(invisible(result))
  }
  if (identical(result$status, "failed")) {
    cli::cli_abort(
      c(
        "Feed polling reached the failure threshold.",
        "x" = paste0(
          result$failed_count,
          " of ",
          result$due_count,
          " due Feeds failed; threshold: ",
          result$failure_threshold,
          "."
        )
      ),
      class = "rill_feed_poll_failure_threshold"
    )
  }
  if (identical(result$status, "partial")) {
    cli::cli_warn(paste0(
      result$failed_count,
      " of ",
      result$due_count,
      " due Feeds failed; recorded for retry."
    ))
  } else {
    cli::cli_inform(c(
      "v" = "Checked {result$due_count} due Feed{?s}; all succeeded."
    ))
  }
  invisible(result)
}

run_due_feed_polling <- function(
  store,
  interval_minutes,
  failure_threshold,
  refresh = refresh_feed,
  now = utc_now()
) {
  locked <- store_with_feed_poll_lock(store, function() {
    recovered_count <- store_recover_feed_poll_runs(store, now)
    if (recovered_count) {
      telemetry_log(
        "warn",
        "feed_poll.recovered_interrupted_runs",
        list("poll.recovered_count" = recovered_count)
      )
    }
    feeds <- store_list_due_feeds(store, now, interval_minutes)
    run_id <- new_feed_poll_run_id(now)
    store_start_feed_poll_run(
      store,
      run_id,
      started_at = now,
      due_count = nrow(feeds),
      failure_threshold = failure_threshold
    )

    outcomes <- list()
    tryCatch(
      for (index in seq_len(nrow(feeds))) {
        outcomes[[index]] <- poll_one_feed(
          store,
          run_id,
          as.list(feeds[index, , drop = FALSE]),
          refresh
        )
      },
      error = function(error) {
        failed <- vapply(
          outcomes,
          \(outcome) identical(outcome$status, "failed"),
          logical(1)
        )
        succeeded_count <- length(outcomes) - sum(failed)
        try(
          store_finish_feed_poll_run(
            store,
            run_id,
            status = "failed",
            succeeded_count = succeeded_count,
            failed_count = nrow(feeds) - succeeded_count,
            error_class = class(error)[[1L]],
            error_message = conditionMessage(error)
          ),
          silent = TRUE
        )
        stop(error)
      }
    )
    failed <- vapply(
      outcomes,
      \(outcome) identical(outcome$status, "failed"),
      logical(1)
    )
    failed_count <- sum(failed)
    succeeded_count <- length(outcomes) - failed_count
    status <- if (failed_count >= failure_threshold) {
      "failed"
    } else if (failed_count) {
      "partial"
    } else {
      "succeeded"
    }
    store_finish_feed_poll_run(
      store,
      run_id,
      status,
      succeeded_count,
      failed_count
    )
    telemetry_log(
      if (identical(status, "failed")) "error" else "info",
      "feed_poll.completed",
      list(
        "poll.run_id" = run_id,
        "poll.status" = status,
        "poll.due_count" = nrow(feeds),
        "poll.succeeded_count" = succeeded_count,
        "poll.failed_count" = failed_count
      )
    )
    list(
      run_id = run_id,
      status = status,
      due_count = nrow(feeds),
      succeeded_count = succeeded_count,
      failed_count = failed_count,
      failure_threshold = failure_threshold,
      recovered_count = recovered_count,
      outcomes = outcomes
    )
  })

  if (!locked$acquired) {
    telemetry_log("info", "feed_poll.skipped_overlap")
    return(list(
      run_id = NULL,
      status = "skipped_overlap",
      due_count = 0L,
      succeeded_count = 0L,
      failed_count = 0L,
      failure_threshold = failure_threshold,
      recovered_count = 0L,
      outcomes = list()
    ))
  }
  locked$value
}

store_recover_feed_poll_runs <- function(store, recovered_at) {
  error_class <- "rill_feed_poll_interrupted"
  error_message <- "The previous polling process stopped before completion."
  if (identical(store$mode, "postgres")) {
    recovered <- DBI::dbGetQuery(
      store$pool,
      paste(
        "WITH outcome_counts AS (",
        "SELECT runs.run_id, count(outcomes.feed_id) FILTER (",
        "WHERE outcomes.status <> 'failed')::integer AS succeeded_count",
        "FROM feed_poll_runs runs LEFT JOIN feed_poll_outcomes outcomes",
        "ON outcomes.run_id = runs.run_id WHERE runs.status = 'running'",
        "GROUP BY runs.run_id",
        ") UPDATE feed_poll_runs AS runs SET status = 'failed',",
        "completed_at = $1, succeeded_count = counts.succeeded_count,",
        "failed_count = runs.due_count - counts.succeeded_count,",
        "error_class = $2, error_message = $3 FROM outcome_counts counts",
        "WHERE runs.run_id = counts.run_id RETURNING runs.run_id"
      ),
      params = list(recovered_at, error_class, error_message)
    )
    return(nrow(recovered))
  }
  running <- which(store$memory$feed_poll_runs$status == "running")
  if (!length(running)) {
    return(0L)
  }
  store$memory$feed_poll_runs$status[running] <- "failed"
  store$memory$feed_poll_runs$completed_at[running] <- recovered_at
  store$memory$feed_poll_runs$error_class[running] <- error_class
  store$memory$feed_poll_runs$error_message[running] <- error_message
  for (index in running) {
    run_id <- store$memory$feed_poll_runs$run_id[[index]]
    outcomes <- store$memory$feed_poll_outcomes[
      store$memory$feed_poll_outcomes$run_id == run_id,
      ,
      drop = FALSE
    ]
    succeeded_count <- sum(outcomes$status != "failed")
    store$memory$feed_poll_runs$succeeded_count[[index]] <- succeeded_count
    store$memory$feed_poll_runs$failed_count[[index]] <-
      store$memory$feed_poll_runs$due_count[[index]] - succeeded_count
  }
  length(running)
}

poll_one_feed <- function(store, run_id, feed, refresh) {
  started_at <- utc_now()
  outcome <- tryCatch(
    {
      result <- refresh(store, feed)
      list(
        run_id = run_id,
        feed_id = feed$feed_id,
        status = if (isTRUE(result$not_modified)) {
          "not_modified"
        } else {
          "updated"
        },
        added_count = as.integer(result$added %||% 0L),
        error_class = NA_character_,
        error_message = NA_character_
      )
    },
    error = function(error) {
      telemetry_log(
        "warn",
        "feed_poll.feed_failed",
        list(
          "poll.run_id" = run_id,
          "feed.id" = feed$feed_id,
          "error.type" = class(error)[[1L]]
        )
      )
      list(
        run_id = run_id,
        feed_id = feed$feed_id,
        status = "failed",
        added_count = 0L,
        error_class = class(error)[[1L]],
        error_message = conditionMessage(error)
      )
    }
  )
  store_record_feed_poll_outcome(
    store,
    outcome,
    started_at = started_at,
    completed_at = utc_now()
  )
  outcome
}

new_feed_poll_run_id <- function(now) {
  rill_id(
    "feed-poll-run",
    now,
    Sys.getpid(),
    Sys.time(),
    stats::runif(1)
  )
}

store_with_feed_poll_lock <- function(store, code) {
  if (!identical(store$mode, "postgres")) {
    if (isTRUE(store$memory$feed_poll_locked)) {
      return(list(acquired = FALSE, value = NULL))
    }
    store$memory$feed_poll_locked <- TRUE
    on.exit(store$memory$feed_poll_locked <- FALSE, add = TRUE)
    return(list(acquired = TRUE, value = code()))
  }

  pool::poolWithTransaction(store$pool, function(connection) {
    acquired <- isTRUE(DBI::dbGetQuery(
      connection,
      paste(
        "SELECT pg_try_advisory_xact_lock(",
        "hashtext('rill:feed-poll')) AS acquired"
      )
    )$acquired[[1L]])
    if (!acquired) {
      return(list(acquired = FALSE, value = NULL))
    }
    list(acquired = TRUE, value = code())
  })
}

store_list_due_feeds <- function(store, now, interval_minutes) {
  if (identical(store$mode, "postgres")) {
    return(DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT f.* FROM feeds f",
        "WHERE f.source_kind = 'subscription' AND EXISTS (",
        "SELECT 1 FROM subscriptions s",
        "WHERE s.feed_id = f.feed_id AND s.status = 'active'",
        ") AND (f.last_polled_at IS NULL OR f.last_polled_at <=",
        "$1::timestamptz - ($2::double precision * interval '1 minute'))",
        "ORDER BY lower(f.title), f.feed_id"
      ),
      params = list(now, interval_minutes)
    ))
  }

  feeds <- store_list_active_feeds(store)
  last_polled_at <- suppressWarnings(as.POSIXct(
    feeds$last_polled_at,
    tz = "UTC"
  ))
  due_before <- as.POSIXct(now, tz = "UTC") - interval_minutes * 60
  feeds[
    is.na(last_polled_at) | last_polled_at <= due_before,
    ,
    drop = FALSE
  ]
}

store_start_feed_poll_run <- function(
  store,
  run_id,
  started_at,
  due_count,
  failure_threshold
) {
  if (identical(store$mode, "postgres")) {
    DBI::dbExecute(
      store$pool,
      paste(
        "INSERT INTO feed_poll_runs (",
        "run_id, started_at, status, due_count, failure_threshold",
        ") VALUES ($1, $2, 'running', $3, $4)"
      ),
      params = list(run_id, started_at, due_count, failure_threshold)
    )
    return(invisible(run_id))
  }
  store$memory$feed_poll_runs <- rbind(
    store$memory$feed_poll_runs,
    data.frame(
      run_id = run_id,
      started_at = started_at,
      completed_at = NA_character_,
      status = "running",
      due_count = as.integer(due_count),
      succeeded_count = 0L,
      failed_count = 0L,
      failure_threshold = as.integer(failure_threshold),
      error_class = NA_character_,
      error_message = NA_character_,
      stringsAsFactors = FALSE
    )
  )
  invisible(run_id)
}

store_record_feed_poll_outcome <- function(
  store,
  outcome,
  started_at,
  completed_at
) {
  if (identical(store$mode, "postgres")) {
    pool::poolWithTransaction(store$pool, function(connection) {
      DBI::dbExecute(
        connection,
        paste(
          "INSERT INTO feed_poll_outcomes (",
          paste(
            "run_id, feed_id, started_at, completed_at, status, added_count,",
            "error_class, error_message"
          ),
          ") VALUES ($1, $2, $3, $4, $5, $6, $7, $8)"
        ),
        params = list(
          outcome$run_id,
          outcome$feed_id,
          started_at,
          completed_at,
          outcome$status,
          outcome$added_count,
          outcome$error_class,
          outcome$error_message
        )
      )
      DBI::dbExecute(
        connection,
        paste(
          "UPDATE feeds SET poll_status = $2, last_polled_at = $3",
          "WHERE feed_id = $1"
        ),
        params = list(outcome$feed_id, outcome$status, completed_at)
      )
    })
    return(invisible(outcome))
  }
  store$memory$feed_poll_outcomes <- rbind(
    store$memory$feed_poll_outcomes,
    data.frame(
      run_id = outcome$run_id,
      feed_id = outcome$feed_id,
      started_at = started_at,
      completed_at = completed_at,
      status = outcome$status,
      added_count = outcome$added_count,
      error_class = outcome$error_class,
      error_message = outcome$error_message,
      stringsAsFactors = FALSE
    )
  )
  index <- match(outcome$feed_id, store$memory$feeds$feed_id)
  store$memory$feeds$poll_status[[index]] <- outcome$status
  store$memory$feeds$last_polled_at[[index]] <- completed_at
  invisible(outcome)
}

store_finish_feed_poll_run <- function(
  store,
  run_id,
  status,
  succeeded_count,
  failed_count,
  error_class = NA_character_,
  error_message = NA_character_,
  completed_at = utc_now()
) {
  if (identical(store$mode, "postgres")) {
    DBI::dbExecute(
      store$pool,
      paste(
        "UPDATE feed_poll_runs SET completed_at = $2, status = $3,",
        paste(
          "succeeded_count = $4, failed_count = $5, error_class = $6,",
          "error_message = $7 WHERE run_id = $1"
        )
      ),
      params = list(
        run_id,
        completed_at,
        status,
        succeeded_count,
        failed_count,
        error_class,
        error_message
      )
    )
    return(invisible(run_id))
  }
  index <- match(run_id, store$memory$feed_poll_runs$run_id)
  store$memory$feed_poll_runs$completed_at[[index]] <- completed_at
  store$memory$feed_poll_runs$status[[index]] <- status
  store$memory$feed_poll_runs$succeeded_count[[index]] <- succeeded_count
  store$memory$feed_poll_runs$failed_count[[index]] <- failed_count
  store$memory$feed_poll_runs$error_class[[index]] <- error_class
  store$memory$feed_poll_runs$error_message[[index]] <- error_message
  invisible(run_id)
}
