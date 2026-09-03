#' Create the Rill application
#'
#' `rill_app()` creates the Shiny application using configuration read from
#' environment variables. With no `DATABASE_URL`, it uses bundled demo data.
#' The production web container enables a private OIDC proxy gate. Its Reader
#' Identity adapter resolves exact issuer and `sub` pairs to durable internal
#' Readers. Configured `RILL_ALLOWED_OIDC_SUBJECTS` values bootstrap the private
#' Reader in `RILL_ACTOR_ID`; unknown identities remain denied with one pending
#' admission record. Email and other profile claims are mutable metadata, not
#' identity keys. When `RILL_CAPTURE_TOKEN` is set, the same application binds
#' that credential to `RILL_ACTOR_ID` and accepts authenticated browser
#' Documents at `/api/v1/captures`. Captures and reading-copy selection remain
#' private to that Reader. `RILL_AGENT_MODEL` selects the
#' [ellmer][ellmer::chat()] model used for source-grounded questions and
#' Orientation. Its provider credential must also be available.
#' `RILL_AGENT_BASE_URL` selects a custom provider endpoint and is required
#' when Rill cannot resolve the effective endpoint itself.
#' `RILL_ORIENTATION_ENABLED=true` makes automatic Orientation available; each
#' Reader must still confirm the configured Data Destination in the app before
#' bounded reading copies are sent. External destinations also require an
#' inspectable provider-policy link in `RILL_AGENT_POLICY_URL`.
#'
#' @return A `shiny.appobj` object suitable for [shiny::runApp()].
#' @export
#'
#' @examples
#' if (interactive()) {
#'   shiny::runApp(rill_app())
#' }
rill_app <- function() {
  config <- rill_config()
  init_telemetry(config)
  store <- rill_store(config)
  store_interrupt_agent_runs(store, recovery = "process_restart")
  identity <- reader_identity_adapter(config, store)
  shiny::addResourcePath(
    "rill-assets",
    rill_package_file("app", "www")
  )

  shiny::onStop(function() rill_store_close(store))

  app <- shiny::shinyApp(
    ui = rill_ui(config),
    server = identity_server_handler(rill_server(config, store), identity)
  )
  app$httpHandler <- identity_http_handler(app$httpHandler, identity)
  app$httpHandler <- capture_http_handler(app$httpHandler, store, config)
  app
}

#' Poll every active Feed
#'
#' `poll_feeds()` refreshes each shared Feed with at least one active
#' Subscription once. It is intended for scheduled jobs and reports progress
#' with structured cli output.
#'
#' @return Invisibly, a list containing one refresh result per feed.
#' @export
poll_feeds <- function() {
  config <- rill_config()
  if (config$demo_mode) {
    cli::cli_abort(c(
      "Can't poll feeds without a durable store.",
      "i" = "Set {.envvar DATABASE_URL} to a PostgreSQL connection string."
    ))
  }

  init_telemetry(config)
  store <- rill_store(config)
  on.exit(rill_store_close(store), add = TRUE)

  results <- refresh_all_feeds(store)
  failed <- vapply(results, function(result) !is.null(result$error), logical(1))

  if (any(failed)) {
    failure_details <- vapply(
      results[failed],
      function(result) paste0(result$feed_id, ": ", result$error),
      character(1)
    )
    cli::cli_abort(c(
      "Failed to refresh {sum(failed)} feed{?s}.",
      "x" = "{failure_details}"
    ))
  }

  cli::cli_inform(c(
    "v" = "Checked {length(results)} feed{?s}; all succeeded."
  ))
  invisible(results)
}

#' Prepare today's reading copies
#'
#' `prepare_today()` extracts and caches clean reading copies for articles
#' published during the current local calendar day. Existing documents are
#' preserved, and failed extractions remain uncached so a later run can retry.
#' It is intended for interactive use or scheduled jobs.
#'
#' @return Invisibly, a list with counts for total, cached, prepared, and failed
#'   articles, plus named extraction errors.
#' @export
prepare_today <- function() {
  config <- rill_config()
  if (config$demo_mode) {
    cli::cli_abort(c(
      "Can't prepare today's articles without a durable store.",
      "i" = "Set {.envvar DATABASE_URL} to a PostgreSQL connection string."
    ))
  }

  init_telemetry(config)
  store <- rill_store(config)
  on.exit(rill_store_close(store), add = TRUE)
  progress_id <- NULL
  result <- prepare_today_documents(
    store,
    config,
    progress = function(index, total, title) {
      if (is.null(progress_id)) {
        progress_id <<- cli::cli_progress_bar(
          "Preparing today's reading copies",
          total = total,
          clear = FALSE,
          auto_terminate = FALSE,
          .auto_close = FALSE
        )
      }
      cli::cli_progress_update(
        id = progress_id,
        set = index,
        status = title
      )
    }
  )
  if (!is.null(progress_id)) {
    cli::cli_progress_done(id = progress_id)
  }

  status <- format_prepare_today_status(result)
  if (result$failed > 0L) {
    cli::cli_warn(status)
  } else {
    cli::cli_inform(c("v" = status))
  }
  invisible(result)
}
