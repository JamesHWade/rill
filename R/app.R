#' Create the Rill application
#'
#' `rill_app()` creates the Shiny application using configuration read from
#' environment variables. With no `DATABASE_URL`, it uses bundled demo data.
#' When `RILL_CAPTURE_TOKEN` is set, the same application accepts authenticated
#' browser documents at `/api/v1/captures`.
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
  shiny::addResourcePath(
    "rill-assets",
    rill_package_file("app", "www")
  )

  shiny::onStop(function() rill_store_close(store))

  app <- shiny::shinyApp(
    ui = rill_ui(config),
    server = rill_server(config, store)
  )
  app$httpHandler <- capture_http_handler(app$httpHandler, store, config)
  app
}

#' Poll every configured feed
#'
#' `poll_feeds()` refreshes every feed in the durable PostgreSQL store. It is
#' intended for scheduled jobs and reports progress with structured cli output.
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

  results <- refresh_all_feeds(store, config$actor_id)
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
