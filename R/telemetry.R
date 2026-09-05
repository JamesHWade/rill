# Operational telemetry is deliberately low-cardinality and content-free.
# Reading behavior is product data and is written to the events table instead.

telemetry_start <- function(name, attributes = list(), parent = NULL) {
  if (!requireNamespace("otel", quietly = TRUE)) {
    return(NULL)
  }
  if (!otel::is_tracing_enabled()) {
    return(NULL)
  }
  tryCatch(
    {
      if (is.character(parent)) {
        parent <- otel::extract_http_context(parent)
      }
      otel::start_span(
        name,
        attributes = attributes,
        options = list(parent = parent),
        tracer = "rill"
      )
    },
    error = \(error) NULL
  )
}

telemetry_activate <- function(span, env = parent.frame()) {
  if (is.null(span)) {
    return(invisible(NULL))
  }
  tryCatch(
    otel::local_active_span(span, activation_scope = env, end_on_exit = FALSE),
    error = \(error) NULL
  )
  invisible(NULL)
}

telemetry_attributes <- function(span, attributes) {
  if (is.null(span)) {
    return(invisible(NULL))
  }
  tryCatch(
    {
      for (name in names(attributes)) {
        if (!is.null(attributes[[name]]) && !anyNA(attributes[[name]])) {
          span$set_attribute(name, attributes[[name]])
        }
      }
    },
    error = \(error) NULL
  )
  invisible(NULL)
}

telemetry_end <- function(span, status = "unset", attributes = list()) {
  if (is.null(span)) {
    return(invisible(NULL))
  }
  telemetry_attributes(span, attributes)
  tryCatch(span$end(status_code = status), error = \(error) NULL)
  invisible(NULL)
}

telemetry_finalizer <- function(span) {
  force(span)
  function(status = "unset", attributes = list()) {
    current <- span
    span <<- NULL
    telemetry_end(current, status, attributes)
  }
}

# Automatic exception recording can include source URLs, tokens, and call data.
# Activate without automatic ending; explicitly end without recording errors.
telemetry_local_span <- function(
  name,
  attributes = list(),
  env = parent.frame()
) {
  span <- telemetry_start(name, attributes)
  telemetry_activate(span, env)
  withr::defer(telemetry_end(span), envir = env)
  span
}

telemetry_span <- function(name, code, attributes = list(), parent = NULL) {
  span <- telemetry_start(name, attributes, parent)
  if (is.null(span)) {
    return(force(code))
  }
  status <- "unset"
  ending_attributes <- list()
  on.exit(telemetry_end(span, status, ending_attributes), add = TRUE)
  result <- tryCatch(
    withVisible(otel::with_active_span(span, code)),
    error = function(error) {
      type <- class(error)[[1L]]
      if (!grepl("^[a-zA-Z0-9_.]{1,80}$", type)) {
        type <- "unknown"
      }
      status <<- "error"
      ending_attributes <<- list("error.type" = type)
      stop(error)
    }
  )
  status <- "ok"
  if (result$visible) result$value else invisible(result$value)
}

telemetry_context <- function(span = NULL) {
  if (!requireNamespace("otel", quietly = TRUE)) {
    return(NULL)
  }
  tryCatch(
    if (is.null(span)) {
      otel::pack_http_context()
    } else {
      span$get_context()$to_http_headers()
    },
    error = \(error) NULL
  )
}

telemetry_flush <- function() {
  if (!requireNamespace("otel", quietly = TRUE)) {
    return(invisible(NULL))
  }
  tryCatch(otel::get_default_tracer_provider()$flush(), error = \(error) NULL)
  tryCatch(otel::get_default_logger_provider()$flush(), error = \(error) NULL)
  invisible(NULL)
}

reading_telemetry <- function(enabled) {
  state <- new.env(parent = emptyenv())
  state$id <- state$span <- NULL
  finish <- function(outcome, elapsed_ms = NULL) {
    telemetry_end(
      state$span,
      if (identical(outcome, "visible")) "ok" else "unset",
      list("reading.outcome" = outcome, "reading.first_text_ms" = elapsed_ms)
    )
    state$span <- NULL
  }
  list(
    begin = function(id, surface) {
      finish("superseded")
      state$id <- NULL
      if (
        !isTRUE(enabled) ||
          !is.character(id) ||
          length(id) != 1L ||
          is.na(id) ||
          !grepl("^[a-f0-9]{32}$", id)
      ) {
        return(invisible(NULL))
      }
      state$id <- id
      state$began_at <- Sys.time()
      state$flushed <- FALSE
      state$span <- telemetry_start(
        "article.open",
        list("reading.surface" = surface),
        parent = NA
      )
      if (!is.null(state$span)) {
        shiny::withReactiveDomain(
          NULL,
          later::later(
            function() {
              if (identical(state$id, id)) finish("unconfirmed")
            },
            120
          )
        )
      }
      invisible(NULL)
    },
    flushed = function(id) {
      if (
        is.null(state$span) || !identical(id, state$id) || isTRUE(state$flushed)
      ) {
        return(invisible(NULL))
      }
      state$flushed <- TRUE
      telemetry_attributes(
        state$span,
        list(
          "reading.server_flush_ms" = as.numeric(
            difftime(Sys.time(), state$began_at, units = "secs")
          ) *
            1000
        )
      )
    },
    complete = function(id, elapsed_ms, dom_ready_ms = NULL) {
      if (
        is.null(state$span) ||
          !identical(id, state$id) ||
          !is.numeric(elapsed_ms) ||
          length(elapsed_ms) != 1L ||
          !is.finite(elapsed_ms) ||
          elapsed_ms < 0 ||
          elapsed_ms > 120000
      ) {
        return(invisible(NULL))
      }
      if (
        is.numeric(dom_ready_ms) &&
          length(dom_ready_ms) == 1L &&
          is.finite(dom_ready_ms) &&
          dom_ready_ms >= 0 &&
          dom_ready_ms <= elapsed_ms
      ) {
        telemetry_attributes(
          state$span,
          list(
            "reading.dom_ready_ms" = dom_ready_ms,
            "reading.paint_delay_ms" = elapsed_ms - dom_ready_ms
          )
        )
      }
      finish("visible", elapsed_ms)
    },
    activate = function(env = parent.frame()) {
      telemetry_activate(state$span, env)
    },
    annotate = \(attributes) telemetry_attributes(state$span, attributes),
    id = \() state$id,
    finish = finish
  )
}

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
    telemetry_log(
      "info",
      "telemetry.ready",
      c(
        list(environment = config$app_env),
        extractor_runtime_attributes()
      )
    )
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


extractor_runtime_attributes <- function(
  commands = Sys.which(c("node", "npm", "deno", "quarto"))
) {
  stats::setNames(
    as.list(nzchar(unname(commands))),
    paste0("runtime.", names(commands), ".available")
  )
}
