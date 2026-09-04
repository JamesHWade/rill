#' List Reader access requests
#'
#' `list_reader_admissions()` lists access requests recorded when an
#' authenticated person opens Hosted Rill without an attached Reader. The
#' returned request ID is safe to use with [approve_reader_admission()]
#' without copying the provider's external subject identifier.
#' Unlinked requests expire 30 days after their most recent sign-in.
#'
#' @param status Admission status to list: `"pending"`, `"approved"`, or
#'   `"rejected"`.
#'
#' @return A data frame with request, profile, status, and timing metadata. It
#'   does not expose external identity subjects.
#' @export
list_reader_admissions <- function(status = "pending") {
  status <- reader_admission_status_arg(status)
  store <- reader_admission_operator_store()
  on.exit(rill_store_close(store), add = TRUE)

  store_list_reader_admissions(store, status) |>
    reader_admission_summary()
}

#' Approve a Reader access request
#'
#' `approve_reader_admission()` attaches the authenticated external identity
#' behind a request to a new, isolated Reader. Repeating an approval is
#' idempotent and keeps the existing Reader binding. The approval is recorded
#' in the Reader Identity audit log.
#'
#' @param request_id A request ID returned by [list_reader_admissions()].
#' @param responsible_id A stable identifier for the operator granting access,
#'   such as `"operator:james"`.
#' @param reason A concise audit reason for granting access.
#'
#' @return Invisibly, a list containing the request ID, Reader ID, approval
#'   status, and mutable profile metadata.
#' @export
approve_reader_admission <- function(
  request_id,
  responsible_id,
  reason = "invitation approved"
) {
  reader_admission_string_arg(request_id, "request_id")
  reader_admission_string_arg(responsible_id, "responsible_id")
  reader_admission_string_arg(reason, "reason")
  store <- reader_admission_operator_store()
  on.exit(rill_store_close(store), add = TRUE)

  result <- store_approve_reader_admission(
    store,
    request_id = request_id,
    responsible_id = responsible_id,
    reason = reason
  )
  who <- result$display_name
  if (is.na(who) || !nzchar(who)) {
    who <- result$email
  }
  if (is.na(who) || !nzchar(who)) {
    who <- result$request_id
  }
  cli::cli_inform(c(
    "v" = "Approved an isolated Reader for {.val {who}}.",
    "i" = "They can reload Rill to open their Library."
  ))
  invisible(result)
}

reader_admission_status_arg <- function(status) {
  choices <- c("pending", "approved", "rejected")
  if (
    !is.character(status) ||
      length(status) != 1L ||
      is.na(status) ||
      !status %in% choices
  ) {
    cli::cli_abort(
      "{.arg status} must be one of {.or {.val {choices}}}.",
      class = "rill_reader_admission_status_invalid"
    )
  }
  status
}

reader_admission_string_arg <- function(value, argument) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value))
  ) {
    cli::cli_abort(
      "{.arg {argument}} must be a single non-empty string.",
      class = "rill_reader_admission_input_invalid"
    )
  }
  invisible(NULL)
}

reader_admission_operator_store <- function() {
  config <- rill_config()
  if (config$demo_mode) {
    cli::cli_abort(
      c(
        "Can't manage Reader admissions without a durable store.",
        "i" = "Set {.envvar DATABASE_URL} to a PostgreSQL connection string."
      ),
      class = "rill_reader_admission_store_required"
    )
  }
  rill_store(config)
}
