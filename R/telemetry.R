# Operational telemetry is deliberately low-cardinality and content-free.
# Reading behavior is product data and is written to the events table instead.

init_telemetry <- function(config) {
  if (!config$telemetry_enabled) {
    return(invisible(FALSE))
  }

  loaded <- tryCatch(
    {
      loadNamespace("otelsdk")
      TRUE
    },
    error = function(error) {
      cli::cli_warn(
        "OpenTelemetry could not be initialized.",
        parent = error
      )
      FALSE
    }
  )

  if (loaded) {
    telemetry_log("info", "telemetry.ready", list(environment = config$app_env))
  }

  invisible(loaded)
}

telemetry_log <- function(
  level = c("info", "warn", "error"),
  message,
  attributes = list()
) {
  level <- match.arg(level)
  if (!requireNamespace("otel", quietly = TRUE)) {
    return(invisible(NULL))
  }

  logger <- switch(
    level,
    info = otel::log_info,
    warn = otel::log_warn,
    error = otel::log_error
  )

  tryCatch(
    logger(message, attributes = attributes, logger = "rill"),
    error = function(error) invisible(NULL)
  )
}
