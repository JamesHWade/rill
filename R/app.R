#' Create the Rill application
#'
#' `rill_app()` creates the Shiny application using configuration read from
#' environment variables. With no `DATABASE_URL`, it uses bundled demo data.
#' `RILL_IDENTITY_MODE=auth0` enables an in-app Auth0 gate for hosts such as
#' Posit Connect Cloud, while `oidc_proxy` keeps the production container's
#' upstream proxy gate. Both Reader Identity adapters resolve exact issuer and
#' `sub` pairs to durable internal Readers. Configured
#' `RILL_ALLOWED_OIDC_SUBJECTS` values bootstrap the private Reader in
#' `RILL_ACTOR_ID`. Both adapters record one pending admission for a verified
#' identity without a Reader binding while denying Library access. Email and
#' other profile claims are mutable metadata, not identity keys. When
#' `RILL_CAPTURE_TOKEN` is set, the same application binds that credential to
#' `RILL_ACTOR_ID` and accepts authenticated browser Documents at
#' `/api/v1/captures`. Captures and reading-copy selection remain private to
#' that Reader. `RILL_AGENT_MODEL` selects the
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
  initialized <- FALSE
  on.exit(
    {
      if (!initialized) {
        rill_store_close(store)
      }
    },
    add = TRUE
  )
  store_interrupt_agent_runs(store, recovery = "process_restart")
  identity <- reader_identity_adapter(config, store)
  shiny::addResourcePath(
    "rill-assets",
    rill_package_file("app", "www")
  )

  app <- shiny::shinyApp(
    ui = rill_ui(config),
    server = identity_server_handler(
      rill_server(config, store),
      identity,
      on_authenticated = function(resolution, session) {
        access_requests_server(
          "access_requests",
          store,
          identity,
          resolution,
          session = session
        )
      }
    )
  )
  app$httpHandler <- identity_http_handler(app$httpHandler, identity)
  app$httpHandler <- capture_http_handler(app$httpHandler, store, config)
  shiny::onStop(function() rill_store_close(store))
  initialized <- TRUE
  app
}

#' Prepare today's reading copies
#'
#' `prepare_today()` extracts and caches clean reading copies for articles
#' published during the current local calendar day. Existing documents are
#' preserved, and failed extractions remain uncached so a later run can retry.
#' It is intended for interactive use or scheduled jobs.
#'
#' @return Invisibly, a list with counts for total, cached, prepared, and failed
#'   Feed Entries, named safe error summaries, and per-Entry `failures` with the
#'   extraction or storage stage, diagnostic code, HTTP status when available,
#'   and a reference matching the content-free server log. Raw error messages,
#'   request URLs, and credentials are not included in diagnostics.
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
