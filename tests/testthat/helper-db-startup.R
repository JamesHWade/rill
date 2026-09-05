local_startup_database_url <- function(env = parent.frame()) {
  open_store <- local_reader_recovery_database(env)
  store <- open_store(env)
  schema <- DBI::dbGetQuery(store$pool, "SELECT current_schema() AS name")$name
  parsed <- httr2::url_parse(Sys.getenv("RILL_TEST_DATABASE_URL"))
  parsed$query$options <- paste0("-csearch_path=", schema)
  httr2::url_build(parsed)
}

local_postgres_cancel_proxy <- function(database_url, env = parent.frame()) {
  parsed <- httr2::url_parse(database_url)
  testthat::skip_if_not(
    parsed$hostname %in% c("127.0.0.1", "localhost"),
    "The cancellation wire test requires a loopback PostgreSQL database"
  )
  python <- Sys.which("python3")
  testthat::skip_if(!nzchar(python), "Python 3 is not available")
  proxy <- callr::r_bg(
    function(python, script, port) {
      system2(python, shQuote(c(script, "--backend-port", port)))
    },
    args = list(
      python = python,
      script = testthat::test_path("fixtures", "postgres-cancel-proxy.py"),
      port = parsed$port %||% "5432"
    ),
    stdout = "|",
    stderr = "|",
    supervise = TRUE
  )
  withr::defer(proxy$kill_tree(), envir = env)
  deadline <- Sys.time() + 10
  ready <- character()
  while (proxy$is_alive() && Sys.time() < deadline) {
    proxy$poll_io(100)
    ready <- proxy$read_output_lines()
    if (length(ready)) {
      break
    }
  }
  if (length(ready) != 1L || !grepl("^[0-9]+$", ready)) {
    proxy$kill_tree()
    stop(
      "PostgreSQL cancellation proxy did not start: ",
      proxy$read_all_error()
    )
  }
  parsed$hostname <- "127.0.0.1"
  parsed$port <- ready[[1L]]
  parsed$query$sslmode <- "disable"
  list(url = httr2::url_build(parsed), process = proxy)
}
