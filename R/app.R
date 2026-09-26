#' Create the Rill application
#'
#' `rill_app()` builds the Rill Shiny app from settings in environment
#' variables. Without `DATABASE_URL`, it runs an in-memory demo with six
#' bundled stories.
#'
#' @section Configuration:
#' These settings cover most installations:
#'
#' * `DATABASE_URL`: a PostgreSQL connection URL. Rill applies its schema
#'   migrations when the app starts.
#' * `RILL_AGENT_MODEL`: the model used by Ask Rill and Orientation, in any
#'   form that [ellmer::chat()] accepts. Set the provider's API key as well.
#' * `RILL_IDENTITY_MODE`: `local` (the default) for a single reader, `auth0`
#'   for sign-in inside the app, or `oidc_proxy` behind an OpenID Connect
#'   proxy.
#' * `RILL_CAPTURE_TOKEN`: turns on `POST /api/v1/captures` for pages captured
#'   in a browser.
#'
#' The
#' [configuration article](https://jameshwade.github.io/rill/articles/configuration.html)
#' lists every setting.
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
#' `prepare_today()` fetches and saves full reading copies for articles
#' published today, in the R process's time zone. Existing copies are kept.
#' Articles that fail stay without a full copy, so a later run can retry them.
#' It needs `DATABASE_URL` and suits interactive use or a scheduled job.
#'
#' @return Invisibly, a list with the number of `total`, `cached`, `prepared`,
#'   and `failed` articles; `errors`, a short message for each failed article,
#'   named by its entry ID; and `failures`, a list with one diagnostic per
#'   failure. Each diagnostic names the stage (extraction or storage), a
#'   diagnostic code, the HTTP status when known, and a reference that matches
#'   the server log. Neither includes raw error messages, request URLs, or
#'   credentials.
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
