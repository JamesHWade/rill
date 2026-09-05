local_telemetry_http <- function(env = parent.frame()) {
  testthat::skip_if_not_installed("processx")
  python <- Sys.which("python3")
  testthat::skip_if(!nzchar(python), "Python 3 is not available")
  process <- processx::process$new(
    python,
    testthat::test_path("fixtures", "telemetry-http.py"),
    stdout = "|",
    stderr = "|",
    supervise = TRUE
  )
  withr::defer(process$kill(), envir = env)
  deadline <- Sys.time() + 30
  output <- character()
  while (process$is_alive() && Sys.time() < deadline) {
    process$poll_io(100)
    output <- c(output, process$read_output_lines())
    port <- trimws(output)
    port <- port[grepl("^[0-9]+$", port)]
    if (length(port) == 1L) {
      return(paste0("http://127.0.0.1:", port))
    }
  }
  stop(
    "Telemetry HTTP fixture did not start: ",
    paste(c(output, process$read_error_lines()), collapse = "\n"),
    " (alive: ",
    process$is_alive(),
    ")"
  )
}

read_telemetry_spans <- function(path) {
  spans <- list()
  for (line in readLines(path)) {
    record <- jsonlite::fromJSON(line, simplifyVector = FALSE)
    for (resource in record$resourceSpans) {
      for (scope in resource$scopeSpans) {
        spans <- c(spans, scope$spans)
      }
    }
  }
  stats::setNames(spans, vapply(spans, `[[`, character(1), "name"))
}
local_telemetry_ends <- function(env = parent.frame()) {
  records <- list()
  original <- telemetry_end
  testthat::local_mocked_bindings(
    telemetry_end = function(span, status = "unset", attributes = list()) {
      if (!is.null(span)) {
        records[[length(records) + 1L]] <<- list(
          context = telemetry_context(span),
          status = status,
          attributes = attributes
        )
      }
      original(span, status, attributes)
    },
    .env = env
  )
  function() records
}
