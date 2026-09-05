local_telemetry_http <- function(env = parent.frame()) {
  python <- Sys.which("python3")
  testthat::skip_if(!nzchar(python), "Python 3 is not available")
  process <- callr::r_bg(
    function(python, script) system2(python, shQuote(script)),
    args = list(python, testthat::test_path("fixtures", "telemetry-http.py")),
    stdout = "|",
    stderr = "|",
    supervise = TRUE
  )
  withr::defer(process$kill_tree(), envir = env)
  deadline <- Sys.time() + 10
  while (process$is_alive() && Sys.time() < deadline) {
    process$poll_io(100)
    port <- process$read_output_lines()
    if (length(port) == 1L && grepl("^[0-9]+$", port)) {
      return(paste0("http://127.0.0.1:", port))
    }
  }
  stop(
    "Telemetry HTTP fixture did not start: ",
    paste(process$read_error_lines(), collapse = "\n")
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
