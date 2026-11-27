# Deferred Reader questions and question handoff.

completed_response_may_arrive <- function(run) {
  if (
    !identical(run$status, "completed") ||
      is.null(run$terminal_at)
  ) {
    return(FALSE)
  }
  terminal_at <- tryCatch(
    as.POSIXct(run$terminal_at, tz = "UTC"),
    error = \(error) as.POSIXct(NA, tz = "UTC")
  )
  length(terminal_at) == 1L &&
    !is.na(terminal_at) &&
    Sys.time() < terminal_at + 2
}

# A fresh visit reopens the story behind an answer that is still running, or
# one that failed within the last few minutes so that Retry stays in view after
# a reload. Cancelled answers and older failures leave Orientation in charge.
question_run_reopens <- function(run, now = Sys.time(), window = 10 * 60) {
  if (run$status %in% c("pending", "running", "cancelling")) {
    return(TRUE)
  }
  if (!run$status %in% c("failed", "interrupted")) {
    return(FALSE)
  }
  terminal_at <- tryCatch(
    as.POSIXct(run$terminal_at %||% NA_character_, tz = "UTC"),
    error = \(error) as.POSIXct(NA, tz = "UTC")
  )
  length(terminal_at) == 1L &&
    !is.na(terminal_at) &&
    difftime(now, terminal_at, units = "secs") <= window
}

deferred_reader_question_from_rows <- function(rows) {
  if (!nrow(rows)) {
    return(NULL)
  }
  row <- rows[1L, , drop = FALSE]
  pinned_inputs <- row$pinned_inputs[[1L]]
  if (is.character(pinned_inputs)) {
    pinned_inputs <- jsonlite::fromJSON(
      pinned_inputs,
      simplifyVector = FALSE
    )
  }
  list(
    reader_id = row$reader_id[[1L]],
    request_key = row$request_key[[1L]],
    pinned_inputs = pinned_inputs,
    retry_of_run_id = if (is.na(row$retry_of_run_id[[1L]])) {
      NULL
    } else {
      row$retry_of_run_id[[1L]]
    },
    requested_at = row$requested_at[[1L]],
    updated_at = row$updated_at[[1L]]
  )
}

store_get_deferred_reader_question <- function(store, reader_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      "SELECT * FROM deferred_reader_questions WHERE reader_id = $1",
      params = list(reader_id)
    )
    return(deferred_reader_question_from_rows(rows))
  }
  store$memory$deferred_reader_questions[[reader_id]] %||% NULL
}

validate_deferred_reader_question_replay <- function(
  request,
  request_key,
  pinned_inputs,
  retry_of_run_id
) {
  if (
    !identical(request$request_key, request_key) ||
      !identical(
        as.character(canonical_json(request$pinned_inputs)),
        as.character(canonical_json(pinned_inputs))
      ) ||
      !identical(
        request$retry_of_run_id %||% NULL,
        retry_of_run_id %||% NULL
      )
  ) {
    cli::cli_abort(
      "Rill is already holding another question until Orientation stops.",
      class = "rill_agent_run_draining"
    )
  }
  request
}

store_save_deferred_reader_question <- function(
  store,
  reader_id,
  request_key,
  pinned_inputs,
  retry_of_run_id = NULL,
  requested_at = utc_now()
) {
  existing <- store_get_deferred_reader_question(store, reader_id)
  if (!is.null(existing)) {
    return(validate_deferred_reader_question_replay(
      existing,
      request_key,
      pinned_inputs,
      retry_of_run_id
    ))
  }
  if (identical(store$mode, "postgres")) {
    tryCatch(
      DBI::dbExecute(
        store$pool,
        paste(
          "INSERT INTO deferred_reader_questions (",
          paste(
            "reader_id, request_key, pinned_inputs, retry_of_run_id,",
            "requested_at, updated_at"
          ),
          ") VALUES ($1, $2, $3::jsonb, $4, $5, $5)"
        ),
        params = list(
          reader_id,
          request_key,
          agent_run_json(pinned_inputs),
          retry_of_run_id %||% NA_character_,
          requested_at
        )
      ),
      error = function(error) {
        existing <- store_get_deferred_reader_question(store, reader_id)
        if (!is.null(existing)) {
          return(validate_deferred_reader_question_replay(
            existing,
            request_key,
            pinned_inputs,
            retry_of_run_id
          ))
        }
        stop(error)
      }
    )
    return(store_get_deferred_reader_question(store, reader_id))
  }
  request <- list(
    reader_id = reader_id,
    request_key = request_key,
    pinned_inputs = pinned_inputs,
    retry_of_run_id = retry_of_run_id,
    requested_at = requested_at,
    updated_at = requested_at
  )
  store$memory$deferred_reader_questions[[reader_id]] <- request
  request
}

store_delete_deferred_reader_question <- function(
  store,
  reader_id,
  request_key
) {
  if (identical(store$mode, "postgres")) {
    deleted <- DBI::dbExecute(
      store$pool,
      paste(
        "DELETE FROM deferred_reader_questions",
        "WHERE reader_id = $1 AND request_key = $2"
      ),
      params = list(reader_id, request_key)
    )
    return(invisible(identical(as.integer(deleted), 1L)))
  }
  existing <- store_get_deferred_reader_question(store, reader_id)
  if (is.null(existing) || !identical(existing$request_key, request_key)) {
    return(invisible(FALSE))
  }
  store$memory$deferred_reader_questions[[reader_id]] <- NULL
  invisible(TRUE)
}

store_start_prioritized_reader_question <- function(
  store,
  reader_id,
  request_key,
  pinned_inputs = NULL,
  retry_of = NULL,
  requested_at = utc_now(),
  worker_id = NULL,
  transition_at = requested_at
) {
  result <- store_with_agent_run_reader_lock(
    store,
    reader_id,
    function(transaction_store) {
      store_interrupt_expired_agent_runs(
        transaction_store,
        recovered_at = transition_at
      )
      retry_of_run_id <- retry_of$run_id %||% NULL
      expected_inputs <- pinned_inputs
      if (!is.null(retry_of)) {
        original <- store_get_agent_run(
          transaction_store,
          reader_id,
          retry_of_run_id
        )
        if (
          is.null(original) ||
            !identical(original$kind, "question") ||
            !original$status %in%
              c("completed", "failed", "cancelled", "interrupted")
        ) {
          return(list(run = NULL, preempted = NULL))
        }
        expected_inputs <- original$pinned_inputs
      }
      existing <- store_get_agent_run_by_request_key(
        transaction_store,
        reader_id,
        request_key
      )
      if (!is.null(existing)) {
        validate_agent_run_replay(
          existing,
          kind = "question",
          pinned_inputs = expected_inputs,
          retry_of_run_id = retry_of_run_id
        )
        store_delete_deferred_reader_question(
          transaction_store,
          reader_id,
          request_key
        )
        return(list(run = existing, preempted = NULL, deferred = NULL))
      }
      deferred <- store_get_deferred_reader_question(
        transaction_store,
        reader_id
      )
      if (!is.null(deferred)) {
        validate_deferred_reader_question_replay(
          deferred,
          request_key,
          expected_inputs,
          retry_of_run_id
        )
        requested_at <- deferred$requested_at
      }
      preempted <- store_prioritize_reader_question(
        transaction_store,
        reader_id,
        requested_at = requested_at
      )
      if (
        !is.null(preempted) &&
          preempted$status %in% c("running", "cancelling")
      ) {
        deferred <- store_save_deferred_reader_question(
          transaction_store,
          reader_id = reader_id,
          request_key = request_key,
          pinned_inputs = expected_inputs,
          retry_of_run_id = retry_of_run_id,
          requested_at = requested_at
        )
        return(list(
          run = NULL,
          preempted = preempted,
          deferred = deferred
        ))
      }
      run <- if (is.null(retry_of)) {
        store_start_agent_run(
          transaction_store,
          reader_id = reader_id,
          kind = "question",
          request_key = request_key,
          pinned_inputs = pinned_inputs,
          requested_at = requested_at,
          worker_id = worker_id,
          lease_at = transition_at
        )
      } else {
        store_retry_agent_run(
          transaction_store,
          reader_id = reader_id,
          run_id = retry_of$run_id,
          request_key = request_key,
          requested_at = requested_at,
          worker_id = worker_id,
          lease_at = transition_at
        )
      }
      if (is.null(run)) {
        cli::cli_abort(
          "The Agent Run is not available to retry.",
          class = "rill_agent_run_retry_unavailable"
        )
      }
      store_delete_deferred_reader_question(
        transaction_store,
        reader_id,
        request_key
      )
      list(run = run, preempted = preempted, deferred = NULL)
    }
  )
  result$orientation_signalled <- if (
    is.null(result$preempted) ||
      !identical(result$preempted$status, "cancelling")
  ) {
    FALSE
  } else {
    rill_signal_orientation_interrupt(
      result$preempted$run_id,
      "reader_question"
    )
  }
  result
}
