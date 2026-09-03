testthat::test_that("the Reader Identity migration is bundled", {
  migration_ids <- vapply(
    schema_migration_files(),
    `[[`,
    character(1),
    "migration_id"
  )

  testthat::expect_in("008_reader_identities", migration_ids)
})

testthat::test_that("PostgreSQL resolves durable Reader identities", {
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
    "rill_reader_identity_",
    substr(rill_id(Sys.getpid(), Sys.time(), stats::runif(1)), 1L, 16L)
  )
  schema_identifier <- DBI::dbQuoteIdentifier(admin, schema_name)
  DBI::dbExecute(admin, paste("CREATE SCHEMA", schema_identifier))
  withr::defer({
    DBI::dbExecute(
      admin,
      paste("DROP SCHEMA", schema_identifier, "CASCADE")
    )
    DBI::dbDisconnect(admin)
  })

  connection_args$options <- paste0("-csearch_path=", schema_name)
  database_pool <- do.call(
    pool::dbPool,
    c(
      list(drv = RPostgres::Postgres()),
      connection_args,
      list(minSize = 1, maxSize = 2, idleTimeout = 60)
    )
  )
  store <- structure(
    list(mode = "postgres", pool = database_pool),
    class = "rill_store"
  )
  withr::defer(rill_store_close(store))
  store_apply_schema(store)
  config <- list(
    identity_mode = "oidc_proxy",
    actor_id = "reader-one",
    oidc_issuer = "https://reader.us.auth0.com/",
    allowed_oidc_subjects = "github|reader"
  )
  adapter <- reader_identity_adapter(config, store)

  active <- reader_identity_resolve(
    adapter,
    identity_test_request(
      "github|reader",
      email = "reader@example.com",
      display_name = "Reader"
    )
  )
  pending <- reader_identity_resolve(
    adapter,
    identity_test_request("google-oauth2|unknown")
  )
  pending_again <- reader_identity_resolve(
    adapter,
    identity_test_request("google-oauth2|unknown")
  )
  admission <- store_get_reader_admission(
    store,
    config$oidc_issuer,
    "google-oauth2|unknown"
  )
  identity <- store_get_reader_identity(
    store,
    config$oidc_issuer,
    "github|reader"
  )

  testthat::expect_identical(
    active[c("status", "reader_id")],
    list(status = "active", reader_id = "reader-one")
  )
  testthat::expect_identical(
    c(pending$status, pending_again$status),
    c("pending", "pending")
  )
  testthat::expect_identical(admission$attempt_count, 2L)
  testthat::expect_identical(
    identity[c("email", "display_name")],
    list(email = "reader@example.com", display_name = "Reader")
  )

  store_admit_reader_identity(
    store,
    issuer = config$oidc_issuer,
    subject = "google-oauth2|unknown",
    reader_id = "reader-two",
    responsible_id = "operator:james",
    reason = "invitation approved"
  )
  admitted <- reader_identity_resolve(
    adapter,
    identity_test_request("google-oauth2|unknown")
  )
  testthat::expect_identical(
    admitted[c("status", "reader_id")],
    list(status = "active", reader_id = "reader-two")
  )
  testthat::expect_identical(
    store_get_reader_admission(
      store,
      config$oidc_issuer,
      "google-oauth2|unknown"
    )$status,
    "approved"
  )

  store_disable_reader(
    store,
    "reader-one",
    responsible_id = "operator:james",
    reason = "access revoked"
  )
  disabled <- reader_identity_resolve(
    adapter,
    identity_test_request("github|reader")
  )
  testthat::expect_identical(
    disabled[c("status", "reader_id")],
    list(status = "disabled", reader_id = NULL)
  )

  local_adapter <- reader_identity_adapter(
    list(identity_mode = "local", actor_id = "reader-one"),
    store
  )
  local_disabled <- reader_identity_resolve(
    local_adapter,
    identity_test_request("forged-subject")
  )
  testthat::expect_identical(
    local_disabled[c("status", "reader_id")],
    list(status = "disabled", reader_id = NULL)
  )

  happened_at <- "2026-09-03 12:00:00 UTC"
  store_admit_reader_identity(
    store,
    issuer = config$oidc_issuer,
    subject = "github|audit",
    reader_id = "reader-audit",
    responsible_id = "operator:james",
    reason = "invitation approved",
    now = happened_at
  )
  store_disable_reader(
    store,
    "reader-audit",
    responsible_id = "operator:james",
    reason = "access revoked",
    now = happened_at
  )
  testthat::expect_identical(
    store_list_reader_identity_events(store, "reader-audit")$action,
    c("identity_attached", "reader_disabled")
  )
})
