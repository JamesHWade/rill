orientation_error_attributes <- function(error) {
  classes <- character()
  types <- character()
  depth <- 0L
  while (inherits(error, "condition") && depth < 8L) {
    current <- utils::head(class(error), 8L)
    current[!grepl("^[a-zA-Z0-9_.]{1,80}$", current)] <- "unknown"
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
