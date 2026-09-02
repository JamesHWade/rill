store_start_agent_run <- function(
  store,
  reader_id,
  kind,
  request_key,
  pinned_inputs,
  requested_at = utc_now(),
  retry_of_run_id = NULL,
  worker_id = NULL
) {
  run_id <- rill_id("agent-run", reader_id, request_key)
  existing <- store_get_agent_run(store, reader_id, run_id)
  if (!is.null(existing)) {
    validate_agent_run_replay(existing, kind, pinned_inputs, retry_of_run_id)
    return(existing)
  }

  if (identical(store$mode, "postgres")) {
    tryCatch(
      DBI::dbExecute(
        store$pool,
        paste(
          "INSERT INTO agent_runs (",
          paste(
            "run_id, reader_id, kind, request_key, retry_of_run_id,",
            "status, pinned_inputs, requested_at, updated_at, worker_id"
          ),
          ") SELECT $1, $2, $3, $4, $5, 'pending', $6::jsonb, $7, $7, $8",
          paste(
            "WHERE NOT EXISTS (SELECT 1 FROM agent_runs",
            "WHERE reader_id = $2",
            "AND status IN ('pending', 'running', 'cancelling'))"
          ),
          "ON CONFLICT (reader_id, request_key) DO NOTHING"
        ),
        params = list(
          run_id,
          reader_id,
          kind,
          request_key,
          retry_of_run_id %||% NA_character_,
          agent_run_json(pinned_inputs),
          requested_at,
          worker_id %||% NA_character_
        )
      ),
      error = function(error) {
        if (
          grepl(
            "agent_runs_one_active_reader_idx",
            conditionMessage(error),
            fixed = TRUE
          )
        ) {
          cli::cli_abort(
            "Reader {.val {reader_id}} already has an active Agent Run.",
            class = "rill_agent_run_conflict",
            parent = error
          )
        }
        stop(error)
      }
    )
    run <- store_get_agent_run(store, reader_id, run_id)
    if (is.null(run)) {
      cli::cli_abort(
        "Reader {.val {reader_id}} already has an active Agent Run.",
        class = "rill_agent_run_conflict"
      )
    }
    validate_agent_run_replay(run, kind, pinned_inputs, retry_of_run_id)
    return(run)
  }

  active_runs <- Filter(
    function(run) {
      identical(run$reader_id, reader_id) &&
        run$status %in% c("pending", "running", "cancelling")
    },
    store$memory$agent_runs
  )
  if (length(active_runs)) {
    cli::cli_abort(
      "Reader {.val {reader_id}} already has an active Agent Run.",
      class = "rill_agent_run_conflict"
    )
  }

  run <- list(
    run_id = run_id,
    reader_id = reader_id,
    kind = kind,
    request_key = request_key,
    retry_of_run_id = retry_of_run_id,
    status = "pending",
    pinned_inputs = pinned_inputs,
    requested_at = requested_at,
    worker_id = worker_id
  )
  store$memory$agent_runs[[run_id]] <- run
  run
}

validate_agent_run_replay <- function(
  run,
  kind,
  pinned_inputs,
  retry_of_run_id
) {
  if (
    !identical(run$kind, kind) ||
      !identical(
        as.character(canonical_json(run$pinned_inputs)),
        as.character(canonical_json(pinned_inputs))
      ) ||
      !identical(run$retry_of_run_id %||% NULL, retry_of_run_id %||% NULL)
  ) {
    cli::cli_abort(
      "The Agent Run request key was reused with different pinned inputs.",
      class = "rill_agent_run_replay_conflict"
    )
  }
  invisible(run)
}

store_get_agent_run <- function(store, reader_id, run_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT * FROM agent_runs",
        "WHERE reader_id = $1 AND run_id = $2"
      ),
      params = list(reader_id, run_id)
    )
    return(agent_run_from_rows(rows))
  }

  run <- store$memory$agent_runs[[run_id]]
  if (is.null(run) || !identical(run$reader_id, reader_id)) {
    return(NULL)
  }
  run
}

store_claim_agent_run <- function(
  store,
  reader_id,
  run_id,
  worker_id,
  started_at = utc_now(),
  lease_expires_at
) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "UPDATE agent_runs SET",
        paste(
          "status = 'running', worker_id = $3, started_at = $4,",
          "updated_at = $4, lease_expires_at = $5"
        ),
        paste(
          "WHERE reader_id = $1 AND run_id = $2 AND status = 'pending'",
          "AND (worker_id IS NULL OR worker_id = $3)"
        ),
        "RETURNING *"
      ),
      params = list(
        reader_id,
        run_id,
        worker_id,
        started_at,
        lease_expires_at
      )
    )
    return(agent_run_from_rows(rows))
  }

  run <- store_get_agent_run(store, reader_id, run_id)
  if (
    is.null(run) ||
      !identical(run$status, "pending") ||
      (!is.null(run$worker_id) && !identical(run$worker_id, worker_id))
  ) {
    return(NULL)
  }

  run$status <- "running"
  run$worker_id <- worker_id
  run$started_at <- started_at
  run$lease_expires_at <- lease_expires_at
  run$updated_at <- started_at
  store$memory$agent_runs[[run_id]] <- run
  run
}

store_fail_unstarted_agent_run <- function(
  store,
  reader_id,
  run_id,
  worker_id,
  phase,
  terminal_reason,
  failed_at = utc_now()
) {
  phase <- match.arg(phase, c("start", "claim"))
  eligible_statuses <- if (identical(phase, "claim")) {
    c("pending", "running")
  } else {
    "pending"
  }
  statuses_sql <- if (identical(phase, "claim")) {
    "('pending', 'running')"
  } else {
    "('pending')"
  }
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "UPDATE agent_runs SET status = 'failed', partial_response = NULL,",
        "lease_expires_at = NULL, terminal_reason = $4,",
        "terminal_at = $5, updated_at = $5",
        paste(
          "WHERE reader_id = $1 AND run_id = $2 AND worker_id = $3",
          paste("AND status IN", statuses_sql)
        ),
        "RETURNING *"
      ),
      params = list(reader_id, run_id, worker_id, terminal_reason, failed_at)
    )
    return(agent_run_from_rows(rows))
  }

  run <- store_get_agent_run(store, reader_id, run_id)
  if (
    is.null(run) ||
      !run$status %in% eligible_statuses ||
      !identical(run$worker_id, worker_id)
  ) {
    return(NULL)
  }

  run$status <- "failed"
  run$partial_response <- NULL
  run$lease_expires_at <- NULL
  run$terminal_reason <- terminal_reason
  run$terminal_at <- failed_at
  run$updated_at <- failed_at
  store$memory$agent_runs[[run_id]] <- run
  run
}

store_record_agent_run_partial <- function(
  store,
  reader_id,
  run_id,
  worker_id,
  partial_response,
  updated_at = utc_now(),
  lease_expires_at
) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "UPDATE agent_runs SET partial_response = $4, updated_at = $5,",
        "lease_expires_at = $6",
        paste(
          "WHERE reader_id = $1 AND run_id = $2 AND worker_id = $3",
          "AND status = 'running'"
        ),
        "RETURNING *"
      ),
      params = list(
        reader_id,
        run_id,
        worker_id,
        partial_response,
        updated_at,
        lease_expires_at
      )
    )
    return(agent_run_from_rows(rows))
  }

  run <- store_get_agent_run(store, reader_id, run_id)
  if (
    is.null(run) ||
      !identical(run$status, "running") ||
      !identical(run$worker_id, worker_id)
  ) {
    return(NULL)
  }

  run$partial_response <- partial_response
  run$updated_at <- updated_at
  run$lease_expires_at <- lease_expires_at
  store$memory$agent_runs[[run_id]] <- run
  run
}

store_request_agent_run_cancel <- function(
  store,
  reader_id,
  run_id,
  requested_at = utc_now()
) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        paste(
          "UPDATE agent_runs SET status = CASE WHEN status = 'pending'",
          "THEN 'cancelled' ELSE 'cancelling' END,"
        ),
        paste(
          "cancel_requested_at = COALESCE(cancel_requested_at, $3),",
          "terminal_at = CASE WHEN status = 'pending' THEN $3",
          "ELSE terminal_at END,"
        ),
        paste(
          "terminal_reason = CASE WHEN status = 'pending'",
          "THEN 'cancelled_before_start' ELSE terminal_reason END,"
        ),
        paste(
          "updated_at = CASE WHEN status = 'cancelling' THEN updated_at",
          "ELSE $3 END"
        ),
        paste(
          "WHERE reader_id = $1 AND run_id = $2",
          "AND status IN ('pending', 'running', 'cancelling')"
        ),
        "RETURNING *"
      ),
      params = list(reader_id, run_id, requested_at)
    )
    return(agent_run_from_rows(rows))
  }

  run <- store_get_agent_run(store, reader_id, run_id)
  if (is.null(run)) {
    return(NULL)
  }
  if (identical(run$status, "pending")) {
    run$status <- "cancelled"
    run$cancel_requested_at <- requested_at
    run$terminal_at <- requested_at
    run$terminal_reason <- "cancelled_before_start"
    run$updated_at <- requested_at
    store$memory$agent_runs[[run_id]] <- run
    return(run)
  }
  if (identical(run$status, "cancelling")) {
    return(run)
  }
  if (!run$status %in% c("pending", "running")) {
    return(NULL)
  }

  run$status <- "cancelling"
  run$cancel_requested_at <- requested_at
  run$updated_at <- requested_at
  store$memory$agent_runs[[run_id]] <- run
  run
}

store_finish_agent_run <- function(
  store,
  reader_id,
  run_id,
  worker_id,
  status,
  usage = list(),
  terminal_reason = NULL,
  deputy_run_id = NULL,
  finished_at = utc_now()
) {
  if (!status %in% c("completed", "failed", "cancelled")) {
    cli::cli_abort(
      "{.arg status} must be completed, failed, or cancelled.",
      class = "rill_agent_run_status_invalid"
    )
  }

  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "UPDATE agent_runs SET status = $4, partial_response = NULL,",
        paste(
          "lease_expires_at = NULL, usage = $5::jsonb,",
          "terminal_reason = $6, deputy_run_id = $7, terminal_at = $8,",
          "updated_at = $8"
        ),
        paste(
          "WHERE reader_id = $1 AND run_id = $2 AND worker_id = $3",
          "AND status IN ('running', 'cancelling')"
        ),
        "AND ($4 <> 'cancelled' OR status = 'cancelling')",
        "RETURNING *"
      ),
      params = list(
        reader_id,
        run_id,
        worker_id,
        status,
        agent_run_json(usage),
        terminal_reason %||% NA_character_,
        deputy_run_id %||% NA_character_,
        finished_at
      )
    )
    return(agent_run_from_rows(rows))
  }

  run <- store_get_agent_run(store, reader_id, run_id)
  if (
    is.null(run) ||
      !run$status %in% c("running", "cancelling") ||
      !identical(run$worker_id, worker_id) ||
      (identical(status, "cancelled") &&
        !identical(run$status, "cancelling"))
  ) {
    return(NULL)
  }

  run$status <- status
  run$partial_response <- NULL
  run$lease_expires_at <- NULL
  run$usage <- usage
  run$terminal_reason <- terminal_reason
  run$deputy_run_id <- deputy_run_id
  run$terminal_at <- finished_at
  run$updated_at <- finished_at
  store$memory$agent_runs[[run_id]] <- run
  run
}

store_enrich_timed_out_agent_run <- function(
  store,
  reader_id,
  run_id,
  worker_id,
  usage,
  deputy_run_id,
  updated_at = utc_now()
) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "UPDATE agent_runs SET usage = $4::jsonb,",
        "deputy_run_id = COALESCE(deputy_run_id, $5), updated_at = $6",
        paste(
          "WHERE reader_id = $1 AND run_id = $2 AND worker_id = $3",
          "AND status = 'failed' AND terminal_reason = 'wall_time_limit'"
        ),
        "AND (deputy_run_id IS NULL OR deputy_run_id = $5)",
        "RETURNING *"
      ),
      params = list(
        reader_id,
        run_id,
        worker_id,
        agent_run_json(usage),
        deputy_run_id %||% NA_character_,
        updated_at
      )
    )
    return(agent_run_from_rows(rows))
  }

  run <- store_get_agent_run(store, reader_id, run_id)
  if (
    is.null(run) ||
      !identical(run$worker_id, worker_id) ||
      !identical(run$status, "failed") ||
      !identical(run$terminal_reason, "wall_time_limit") ||
      (!is.null(run$deputy_run_id) &&
        !identical(run$deputy_run_id, deputy_run_id))
  ) {
    return(NULL)
  }

  run$usage <- usage
  run$deputy_run_id <- deputy_run_id
  run$updated_at <- updated_at
  store$memory$agent_runs[[run_id]] <- run
  run
}

store_interrupt_agent_runs <- function(
  store,
  recovery,
  recovered_at = utc_now()
) {
  recovery <- match.arg(
    recovery,
    c("expired_lease", "process_restart")
  )
  process_restart <- identical(recovery, "process_restart")
  terminal_reason <- if (process_restart) {
    "process_restarted"
  } else {
    "lease_expired"
  }

  if (identical(store$mode, "postgres")) {
    where <- if (process_restart) {
      "WHERE status IN ('pending', 'running', 'cancelling')"
    } else {
      paste(
        "WHERE status IN ('running', 'cancelling')",
        "AND lease_expires_at <= $1"
      )
    }
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "UPDATE agent_runs SET status = 'interrupted',",
        paste(
          "partial_response = NULL, lease_expires_at = NULL,",
          "terminal_reason = $2, terminal_at = $1,",
          "updated_at = $1"
        ),
        where,
        "RETURNING *"
      ),
      params = list(recovered_at, terminal_reason)
    )
    return(agent_runs_from_rows(rows))
  }

  interrupted <- list()
  for (run_id in names(store$memory$agent_runs)) {
    run <- store$memory$agent_runs[[run_id]]
    if (
      !run$status %in% c("pending", "running", "cancelling") ||
        (!process_restart &&
          (identical(run$status, "pending") ||
            is.null(run$lease_expires_at) ||
            as.POSIXct(run$lease_expires_at, tz = "UTC") >
              as.POSIXct(recovered_at, tz = "UTC")))
    ) {
      next
    }

    run$status <- "interrupted"
    run$partial_response <- NULL
    run$lease_expires_at <- NULL
    run$terminal_reason <- terminal_reason
    run$terminal_at <- recovered_at
    run$updated_at <- recovered_at
    store$memory$agent_runs[[run_id]] <- run
    interrupted[[length(interrupted) + 1L]] <- run
  }
  interrupted
}

store_interrupt_expired_agent_runs <- function(
  store,
  recovered_at = utc_now()
) {
  store_interrupt_agent_runs(
    store,
    recovery = "expired_lease",
    recovered_at = recovered_at
  )
}

store_retry_agent_run <- function(
  store,
  reader_id,
  run_id,
  request_key,
  requested_at = utc_now(),
  worker_id = NULL
) {
  original <- store_get_agent_run(store, reader_id, run_id)
  if (
    is.null(original) ||
      !original$status %in%
        c(
          "completed",
          "failed",
          "cancelled",
          "interrupted"
        )
  ) {
    return(NULL)
  }

  store_start_agent_run(
    store,
    reader_id = reader_id,
    kind = original$kind,
    request_key = request_key,
    pinned_inputs = original$pinned_inputs,
    requested_at = requested_at,
    retry_of_run_id = original$run_id,
    worker_id = worker_id
  )
}

agent_run_json <- function(value) {
  as.character(jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  ))
}

agent_run_from_rows <- function(rows) {
  if (!nrow(rows)) {
    return(NULL)
  }
  agent_run_from_row(rows[1L, , drop = FALSE])
}

agent_runs_from_rows <- function(rows) {
  if (!nrow(rows)) {
    return(list())
  }
  lapply(seq_len(nrow(rows)), function(index) {
    agent_run_from_row(rows[index, , drop = FALSE])
  })
}

agent_run_from_row <- function(row) {
  value <- function(name) {
    item <- row[[name]][[1L]]
    if (length(item) == 0L || is.na(item)) NULL else item
  }

  run <- list(
    run_id = value("run_id"),
    reader_id = value("reader_id"),
    kind = value("kind"),
    request_key = value("request_key"),
    retry_of_run_id = value("retry_of_run_id"),
    status = value("status"),
    pinned_inputs = jsonlite::fromJSON(
      as.character(value("pinned_inputs")),
      simplifyVector = TRUE
    ),
    requested_at = value("requested_at")
  )

  optional_fields <- c(
    "started_at",
    "worker_id",
    "lease_expires_at",
    "partial_response",
    "cancel_requested_at",
    "terminal_at",
    "terminal_reason",
    "deputy_run_id"
  )
  if (!identical(run$status, "pending")) {
    optional_fields <- c(optional_fields, "updated_at")
  }
  for (name in optional_fields) {
    item <- value(name)
    if (!is.null(item)) {
      run[[name]] <- item
    }
  }

  usage <- value("usage")
  if (!is.null(usage)) {
    run$usage <- jsonlite::fromJSON(
      as.character(usage),
      simplifyVector = TRUE
    )
  }
  run
}
