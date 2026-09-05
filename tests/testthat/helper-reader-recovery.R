local_reader_recovery_database <- function(env = parent.frame()) {
  database_url <- Sys.getenv("RILL_TEST_DATABASE_URL", unset = "")
  testthat::skip_if(
    !nzchar(database_url),
    "RILL_TEST_DATABASE_URL is not configured"
  )
  connection_args <- postgres_connection_args(database_url)
  admin <- do.call(
    DBI::dbConnect,
    c(list(drv = RPostgres::Postgres()), connection_args)
  )
  schema_name <- paste0(
    "rill_reader_recovery_",
    substr(rill_id(Sys.getpid(), Sys.time(), stats::runif(1)), 1L, 16L)
  )
  schema_identifier <- DBI::dbQuoteIdentifier(admin, schema_name)
  withr::defer(
    {
      DBI::dbExecute(
        admin,
        paste("DROP SCHEMA IF EXISTS", schema_identifier, "CASCADE")
      )
      DBI::dbDisconnect(admin)
    },
    envir = env
  )
  DBI::dbExecute(admin, paste("CREATE SCHEMA", schema_identifier))
  connection_args$options <- paste0("-csearch_path=", schema_name)

  function(env = parent.frame()) {
    database_pool <- do.call(
      pool::dbPool,
      c(
        list(drv = RPostgres::Postgres()),
        connection_args,
        list(minSize = 1, maxSize = 2, idleTimeout = 60)
      )
    )
    store <- structure(
      list(
        mode = "postgres",
        pool = database_pool,
        private_reader_id = "reader-one"
      ),
      class = "rill_store"
    )
    withr::defer(
      {
        if (database_pool$valid) {
          rill_store_close(store)
        }
      },
      envir = env
    )
    store
  }
}
