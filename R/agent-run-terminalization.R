# Agent Run terminal states and recovery.

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
        paste(
          "UPDATE agent_runs SET status = 'failed',",
          "partial_response = NULL, response_text = NULL,"
        ),
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
  run$response_text <- NULL
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

store_record_agent_run_response <- function(
  store,
  reader_id,
  run_id,
  worker_id,
  response_text,
  updated_at = utc_now()
) {
  if (
    !is.character(response_text) ||
      length(response_text) != 1L ||
      is.na(response_text) ||
      !nzchar(response_text)
  ) {
    return(NULL)
  }
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "UPDATE agent_runs SET response_text = $4, updated_at = $5",
        paste(
          "WHERE reader_id = $1 AND run_id = $2 AND worker_id = $3",
          "AND status IN ('running', 'completed')"
        ),
        "RETURNING *"
      ),
      params = list(
        reader_id,
        run_id,
        worker_id,
        response_text,
        updated_at
      )
    )
    return(agent_run_from_rows(rows))
  }

  run <- store_get_agent_run(store, reader_id, run_id)
  if (
    is.null(run) ||
      !run$status %in% c("running", "completed") ||
      !identical(run$worker_id, worker_id)
  ) {
    return(NULL)
  }
  run$response_text <- response_text
  run$updated_at <- updated_at
  store$memory$agent_runs[[run_id]] <- run
  run
}

store_renew_agent_run_lease <- function(
  store,
  reader_id,
  run_id,
  worker_id,
  lease_expires_at,
  updated_at = utc_now()
) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "UPDATE agent_runs SET updated_at = $5, lease_expires_at = $4",
        paste(
          "WHERE reader_id = $1 AND run_id = $2 AND worker_id = $3",
          "AND status IN ('running', 'cancelling')"
        ),
        "RETURNING *"
      ),
      params = list(
        reader_id,
        run_id,
        worker_id,
        lease_expires_at,
        updated_at
      )
    )
    return(agent_run_from_rows(rows))
  }

  run <- store_get_agent_run(store, reader_id, run_id)
  if (
    is.null(run) ||
      !run$status %in% c("running", "cancelling") ||
      !identical(run$worker_id, worker_id)
  ) {
    return(NULL)
  }
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
          "lease_expires_at = CASE WHEN status = 'pending' THEN NULL",
          "ELSE lease_expires_at END,",
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
    run$lease_expires_at <- NULL
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

store_prioritize_reader_question <- function(
  store,
  reader_id,
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
          "lease_expires_at = CASE WHEN status = 'pending' THEN NULL",
          "ELSE lease_expires_at END,",
          "cancel_requested_at = COALESCE(cancel_requested_at, $2),"
        ),
        paste(
          "terminal_at = CASE WHEN status = 'pending' THEN $2",
          "ELSE terminal_at END,",
          "terminal_reason = CASE WHEN status = 'pending'",
          "THEN 'reader_question' ELSE terminal_reason END,"
        ),
        paste(
          "updated_at = CASE WHEN status = 'cancelling' THEN updated_at",
          "ELSE $2 END"
        ),
        paste(
          "WHERE reader_id = $1 AND kind = 'orientation'",
          "AND status IN ('pending', 'running', 'cancelling')"
        ),
        "RETURNING *"
      ),
      params = list(reader_id, requested_at)
    )
    return(agent_run_from_rows(rows))
  }

  for (run_id in names(store$memory$agent_runs)) {
    run <- store$memory$agent_runs[[run_id]]
    if (
      !identical(run$reader_id, reader_id) ||
        !identical(run$kind, "orientation") ||
        !run$status %in% c("pending", "running", "cancelling")
    ) {
      next
    }

    run$cancel_requested_at <- run$cancel_requested_at %||% requested_at
    if (identical(run$status, "pending")) {
      run$status <- "cancelled"
      run$lease_expires_at <- NULL
      run$terminal_at <- requested_at
      run$terminal_reason <- "reader_question"
      run$updated_at <- requested_at
    } else if (!identical(run$status, "cancelling")) {
      run$status <- "cancelling"
      run$updated_at <- requested_at
    }
    store$memory$agent_runs[[run_id]] <- run
    return(run)
  }
  NULL
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
          "response_text = CASE WHEN $4 = 'completed' THEN",
          "COALESCE(response_text, partial_response) ELSE NULL END,",
          "lease_expires_at = NULL,",
          "usage = $5::jsonb,",
          "terminal_reason = $6, deputy_run_id = $7, terminal_at = $8,",
          "updated_at = $8"
        ),
        paste(
          "WHERE reader_id = $1 AND run_id = $2 AND worker_id = $3",
          "AND status IN ('running', 'cancelling')"
        ),
        "AND ($4 <> 'cancelled' OR status = 'cancelling')",
        paste(
          "AND ($4 <> 'completed' OR status = 'running'",
          "OR kind <> 'question')"
        ),
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
        !identical(run$status, "cancelling")) ||
      (identical(status, "completed") &&
        identical(run$kind, "question") &&
        !identical(run$status, "running"))
  ) {
    return(NULL)
  }

  run$status <- status
  if (
    identical(status, "completed") &&
      is.null(run$response_text) &&
      nzchar(run$partial_response %||% "")
  ) {
    run$response_text <- run$partial_response
  } else if (!identical(status, "completed")) {
    run$response_text <- NULL
  }
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

store_interrupt_agent_run <- function(
  store,
  reader_id,
  run_id,
  worker_id,
  terminal_reason,
  interrupted_at = utc_now()
) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "UPDATE agent_runs SET status = 'interrupted',",
        paste(
          "partial_response = NULL, response_text = NULL,",
          "lease_expires_at = NULL,",
          "terminal_reason = $4, terminal_at = $5, updated_at = $5"
        ),
        paste(
          "WHERE reader_id = $1 AND run_id = $2 AND worker_id = $3",
          "AND status IN ('running', 'cancelling')"
        ),
        "RETURNING *"
      ),
      params = list(
        reader_id,
        run_id,
        worker_id,
        terminal_reason,
        interrupted_at
      )
    )
    return(agent_run_from_rows(rows))
  }

  run <- store_get_agent_run(store, reader_id, run_id)
  if (
    is.null(run) ||
      !identical(run$worker_id, worker_id) ||
      !run$status %in% c("running", "cancelling")
  ) {
    return(NULL)
  }
  run$status <- "interrupted"
  run$partial_response <- NULL
  run$response_text <- NULL
  run$lease_expires_at <- NULL
  run$terminal_reason <- terminal_reason
  run$terminal_at <- interrupted_at
  run$updated_at <- interrupted_at
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

store_enrich_terminal_agent_run <- function(
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
          "AND status IN ('failed', 'cancelled', 'interrupted')"
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
      !run$status %in% c("failed", "cancelled", "interrupted") ||
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
        "WHERE status IN ('pending', 'running', 'cancelling')",
        "AND lease_expires_at <= $1"
      )
    }
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "UPDATE agent_runs SET status = 'interrupted',",
        paste(
          "partial_response = NULL, response_text = NULL,",
          "lease_expires_at = NULL,",
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
          (is.null(run$lease_expires_at) ||
            as.POSIXct(run$lease_expires_at, tz = "UTC") >
              as.POSIXct(recovered_at, tz = "UTC")))
    ) {
      next
    }

    run$status <- "interrupted"
    run$partial_response <- NULL
    run$response_text <- NULL
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
