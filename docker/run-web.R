port_text <- Sys.getenv("RILL_SHINY_PORT", unset = "3838")
port <- suppressWarnings(as.integer(port_text))

if (
  length(port) != 1L ||
    is.na(port) ||
    !identical(as.character(port), port_text) ||
    port < 1L ||
    port > 65535L
) {
  stop("RILL_SHINY_PORT must be an integer from 1 to 65535.", call. = FALSE)
}

shiny::runApp(
  rill::rill_app(),
  host = "127.0.0.1",
  port = port,
  launch.browser = FALSE
)
