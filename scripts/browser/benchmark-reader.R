args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1L]] else "."
Sys.unsetenv(c("DATABASE_URL", "OPENAI_API_KEY", "ANTHROPIC_API_KEY"))
Sys.setenv(RILL_IDENTITY_MODE = "local")
pkgload::load_all(root, quiet = TRUE)
fixture <- parse(file.path(root, "scripts/browser/app.R"))
for (expression in fixture) {
  if (
    is.call(expression) &&
      identical(expression[[1L]], as.name("<-")) &&
      identical(expression[[2L]], as.name("stress_store"))
  ) {
    eval(expression)
  }
}
config <- rill_config()
store <- stress_store(rill_store(config))
profile <- tempfile(fileext = ".Rprof")
later::with_temp_loop(shiny::testServer(rill_server(config, store), {
  session$setInputs(view = "all", reader_timezone = "UTC")
  session$setInputs(select_entry = list(id = "audit-entry-1"))
  Rprof(profile, interval = 0.001)
  opening <- vapply(
    2:7,
    function(index) {
      system.time(session$setInputs(
        select_entry = list(id = paste0("audit-entry-", index))
      ))[[3L]] *
        1000
    },
    numeric(1)
  )
  marking <- vapply(
    8:13,
    function(index) {
      system.time(session$setInputs(
        queue_action = list(
          id = paste0("benchmark-", index),
          entry_id = paste0("audit-entry-", index),
          action = "mark_read"
        )
      ))[[3L]] *
        1000
    },
    numeric(1)
  )
  Rprof(NULL)
  cat(jsonlite::toJSON(
    list(open_ms = opening, mark_read_ms = marking),
    auto_unbox = TRUE,
    pretty = TRUE
  ))
}))
print(utils::head(summaryRprof(profile)$by.total, 60L))
unlink(profile)
