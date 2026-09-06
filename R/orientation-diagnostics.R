orientation_known_error_classes <- function() {
  c(
    "condition",
    "error",
    "simpleError",
    "rlang_error",
    "rlib_error_3_0",
    "httr2_error",
    "httr2_failure",
    "httr2_http",
    paste0("httr2_http_", 100:599),
    "curl_error",
    "deputy_error",
    "deputy_run_active",
    "deputy_run_stopped",
    "deputy_provider",
    "deputy_tool",
    "deputy_tool_execution",
    "deputy_permission",
    "deputy_permission_denied",
    "deputy_budget",
    "deputy_budget_exceeded",
    "deputy_cost_unavailable",
    "deputy_request_limit",
    "deputy_compaction_error",
    "rill_deputy_api_incompatible",
    "rill_agent_url_invalid",
    "rill_agent_run_conflict",
    "rill_agent_run_claim_failed",
    "rill_agent_run_draining",
    "rill_agent_run_replay_conflict",
    "rill_agent_run_retry_unavailable",
    "rill_agent_run_status_invalid",
    "rill_orientation_agent_stopped",
    "rill_orientation_boundary_changed",
    "rill_orientation_confirmation_required",
    "rill_orientation_destination_disabled",
    "rill_orientation_destination_invalid",
    "rill_orientation_duplicate_submission",
    "rill_orientation_endpoint_required",
    "rill_orientation_invalid",
    "rill_orientation_policy_required",
    "rill_orientation_publication_rejected",
    "rill_orientation_retry_inputs_changed",
    "rill_orientation_retry_invalid",
    "rill_orientation_source_not_inspected",
    "rill_orientation_unavailable"
  )
}

orientation_error_attributes <- function(error) {
  known_classes <- orientation_known_error_classes()
  classes <- character()
  types <- character()
  depth <- 0L
  while (inherits(error, "condition") && depth < 8L) {
    current <- unname(utils::head(class(error), 8L))
    current[!current %in% known_classes] <- "unknown"
    types <- c(types, current[[1L]])
    classes <- union(classes, current)
    depth <- depth + 1L
    error <- error[["parent", exact = TRUE]]
  }
  http <- classes[grepl("^httr2_http_[1-5][0-9]{2}$", classes)]
  list(
    "error.type" = if (length(types)) types[[1L]] else "unknown",
    "error.classes" = classes,
    "error.root_type" = if (length(types) && !inherits(error, "condition")) {
      utils::tail(types, 1L)
    },
    "error.chain_depth" = depth,
    "error.chain_truncated" = inherits(error, "condition"),
    "http.response.status_code" = if (length(http)) {
      as.integer(sub("httr2_http_", "", utils::tail(http, 1L)))
    }
  )
}

orientation_diagnostics <- function() {
  stage <- "candidate_selection"
  failure <- NULL
  tool_state <- NULL
  capture <- function(error) {
    if (is.null(failure)) {
      failure <<- c(
        list("orientation.failure_stage" = stage),
        orientation_error_attributes(error)
      )
    }
    invisible(NULL)
  }
  list(
    at = function(value) {
      stage <<- value
      invisible(NULL)
    },
    agent = function(agent) {
      tool_state <<- rill_orientation_agent_tool_state(agent)
      invisible(NULL)
    },
    capture = capture,
    attributes = function(error = NULL) {
      if (!is.null(error)) {
        capture(error)
      }
      attributes <- failure %||% list()
      if (is.environment(tool_state)) {
        for (field in c(
          "source_calls",
          "submission_attempts",
          "submission_calls"
        )) {
          value <- tool_state[[field]]
          if (
            is.numeric(value) &&
              length(value) == 1L &&
              is.finite(value) &&
              value >= 0 &&
              value <= .Machine$integer.max
          ) {
            attributes[[paste0("orientation.", field)]] <- as.integer(value)
          }
        }
      }
      attributes
    }
  )
}
