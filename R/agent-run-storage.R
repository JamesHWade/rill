# Agent Run persistence conversion helpers.

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
      simplifyVector = TRUE,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ),
    requested_at = value("requested_at")
  )

  optional_fields <- c(
    "started_at",
    "worker_id",
    "lease_expires_at",
    "partial_response",
    "response_text",
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
