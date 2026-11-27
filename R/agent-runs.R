# Agent Run identity, replay, and ownership.

agent_run_pending_lease_seconds <- function() {
  30
}

agent_run_interrupt_retry_limit <- function() {
  3L
}

agent_run_interrupt_retry_delay <- function(attempt) {
  min(0.25 * 2^(max(1L, as.integer(attempt)) - 1L), 2)
}

agent_run_interrupt_confirmation_seconds <- function() {
  30
}

store_with_agent_run_reader_lock <- function(store, reader_id, code) {
  if (
    identical(store$mode, "postgres") &&
      inherits(store$pool, "Pool")
  ) {
    return(pool::poolWithTransaction(store$pool, function(connection) {
      DBI::dbGetQuery(
        connection,
        paste(
          "SELECT pg_advisory_xact_lock(",
          "hashtextextended($1::text, 20260902))"
        ),
        params = list(reader_id)
      )
      transaction_store <- structure(
        list(mode = "postgres", pool = connection),
        class = "rill_store"
      )
      code(transaction_store)
    }))
  }
  code(store)
}

store_start_agent_run <- function(
  store,
  reader_id,
  kind,
  request_key,
  pinned_inputs,
  requested_at = utc_now(),
  retry_of_run_id = NULL,
  worker_id = NULL,
  lease_at = requested_at
) {
  if (
    identical(store$mode, "postgres") &&
      inherits(store$pool, "Pool")
  ) {
    return(store_with_agent_run_reader_lock(
      store,
      reader_id,
      function(transaction_store) {
        store_start_agent_run(
          transaction_store,
          reader_id = reader_id,
          kind = kind,
          request_key = request_key,
          pinned_inputs = pinned_inputs,
          requested_at = requested_at,
          retry_of_run_id = retry_of_run_id,
          worker_id = worker_id,
          lease_at = lease_at
        )
      }
    ))
  }
  store_interrupt_expired_agent_runs(store, recovered_at = lease_at)
  run_id <- rill_id("agent-run", reader_id, request_key)
  existing <- store_get_agent_run(store, reader_id, run_id)
  if (!is.null(existing)) {
    validate_agent_run_replay(existing, kind, pinned_inputs, retry_of_run_id)
    return(existing)
  }
  lease_expires_at <- as.POSIXct(lease_at, tz = "UTC") +
    agent_run_pending_lease_seconds()

  if (identical(store$mode, "postgres")) {
    tryCatch(
      DBI::dbExecute(
        store$pool,
        paste(
          "INSERT INTO agent_runs (",
          paste(
            "run_id, reader_id, kind, request_key, retry_of_run_id,",
            "status, pinned_inputs, requested_at, updated_at, worker_id,",
            "lease_expires_at"
          ),
          paste(
            ") SELECT $1, $2, $3, $4, $5, 'pending', $6::jsonb,",
            "$7, $7, $8, $9"
          ),
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
          worker_id %||% NA_character_,
          lease_expires_at
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
            "Rill is already answering a question or updating Orientation. Try again when it finishes.",
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
        "Rill is already answering a question or updating Orientation. Try again when it finishes.",
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
      "Rill is already answering a question or updating Orientation. Try again when it finishes.",
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
    worker_id = worker_id,
    lease_expires_at = lease_expires_at
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

store_get_active_agent_run <- function(store, reader_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT * FROM agent_runs WHERE reader_id = $1",
        "AND status IN ('pending', 'running', 'cancelling')",
        "ORDER BY requested_at LIMIT 1"
      ),
      params = list(reader_id)
    )
    return(agent_run_from_rows(rows))
  }
  active <- Filter(
    function(run) {
      identical(run$reader_id, reader_id) &&
        run$status %in% c("pending", "running", "cancelling")
    },
    store$memory$agent_runs
  )
  if (!length(active)) NULL else active[[1L]]
}

store_get_latest_question_agent_run <- function(
  store,
  reader_id,
  status = NULL
) {
  if (identical(store$mode, "postgres")) {
    params <- list(reader_id)
    if (!is.null(status)) {
      params <- c(params, list(status))
    }
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT * FROM agent_runs",
        "WHERE reader_id = $1 AND kind = 'question'",
        if (!is.null(status)) "AND status = $2" else "",
        "ORDER BY requested_at DESC LIMIT 1"
      ),
      params = params
    )
    return(agent_run_from_rows(rows))
  }

  questions <- Filter(
    \(run) {
      identical(run$reader_id, reader_id) &&
        identical(run$kind, "question") &&
        (is.null(status) || identical(run$status, status))
    },
    store$memory$agent_runs
  )
  if (!length(questions)) {
    return(NULL)
  }
  requested_at <- vapply(
    questions,
    \(run) as.numeric(as.POSIXct(run$requested_at, tz = "UTC")),
    numeric(1)
  )
  questions[[which.max(requested_at)]]
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

store_get_agent_run_by_request_key <- function(store, reader_id, request_key) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT * FROM agent_runs",
        "WHERE reader_id = $1 AND request_key = $2"
      ),
      params = list(reader_id, request_key)
    )
    return(agent_run_from_rows(rows))
  }

  matches <- Filter(
    \(run) {
      identical(run$reader_id, reader_id) &&
        identical(run$request_key, request_key)
    },
    store$memory$agent_runs
  )
  if (!length(matches)) {
    return(NULL)
  }
  matches[[1L]]
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


store_retry_agent_run <- function(
  store,
  reader_id,
  run_id,
  request_key,
  requested_at = utc_now(),
  worker_id = NULL,
  lease_at = requested_at
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
    worker_id = worker_id,
    lease_at = lease_at
  )
}
