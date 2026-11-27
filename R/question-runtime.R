rill_question_interrupt_registry <- new.env(parent = emptyenv())
rill_question_terminal_intents <- new.env(parent = emptyenv())
rill_question_drain_registry <- new.env(parent = emptyenv())

rill_register_question_interrupt <- function(run_id, interrupt) {
  rill_question_interrupt_registry[[run_id]] <- interrupt
  invisible(NULL)
}

rill_unregister_question_interrupt <- function(run_id) {
  rill_question_interrupt_registry[[run_id]] <- NULL
  drain <- rill_question_drain_registry[[run_id]]
  if (!is.null(drain) && is.function(drain$cancel)) {
    try(drain$cancel(), silent = TRUE)
  }
  rill_question_drain_registry[[run_id]] <- NULL
  invisible(NULL)
}

rill_register_question_drain <- function(run_id, on_result, cancel) {
  rill_question_drain_registry[[run_id]] <- list(
    on_result = on_result,
    cancel = cancel
  )
  invisible(NULL)
}

rill_signal_question_interrupt <- function(run_id, reason, fallback = NULL) {
  interrupt <- rill_question_interrupt_registry[[run_id]]
  if (!is.function(interrupt)) {
    interrupt <- fallback
  }
  result <- if (is.function(interrupt)) {
    tryCatch(interrupt(reason), error = \(error) NA)
  } else {
    NULL
  }
  drain <- rill_question_drain_registry[[run_id]]
  if (!is.null(drain) && is.function(drain$on_result)) {
    drain$on_result(reason, result)
  }
  result
}

rill_question_cancellation_confirmed <- function(
  reason,
  requested_reason = NULL
) {
  is.character(reason) &&
    length(reason) == 1L &&
    !is.na(reason) &&
    reason %in% unique(c("cancelled", "reader_cancelled", requested_reason))
}

rill_assert_question_runtime_identity <- function(pinned_inputs, runtime) {
  if (is.null(pinned_inputs)) {
    return(invisible(runtime))
  }
  expected <- list(
    model = pinned_inputs$model %||% NULL,
    data_destination = pinned_inputs$data_destination %||% NULL,
    data_destination_id = pinned_inputs$data_destination_id %||% NULL
  )
  actual <- list(
    model = runtime$model %||% NULL,
    data_destination = runtime$data_destination %||% NULL,
    data_destination_id = runtime$data_destination_id %||% NULL
  )
  if (!identical(expected, actual)) {
    cli::cli_abort(
      c(
        "Rill's model changed before it could answer.",
        "i" = "Ask again to send your question to the new model."
      ),
      class = "rill_agent_runtime_identity_changed"
    )
  }
  invisible(runtime)
}

# `later` and `onFlushed` callbacks run outside any reactive context, so
# reading a reactive value there fails, and an uncaught error in a `later`
