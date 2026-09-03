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
  config <- list(
    identity_mode = "oidc_proxy",
    actor_id = "reader-one",
    oidc_issuer = "https://reader.us.auth0.com/",
    allowed_oidc_subjects = "github|reader"
  )
  store <- structure(
    list(
      mode = "postgres",
      pool = database_pool,
      private_reader_id = config$actor_id
    ),
    class = "rill_store"
  )
  withr::defer(rill_store_close(store))
  store_apply_schema(store)
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

  DBI::dbExecute(
    store$pool,
    paste(
      "UPDATE reader_external_identities SET revoked_at = $3",
      "WHERE issuer = $1 AND subject = $2"
    ),
    params = list(
      config$oidc_issuer,
      "github|reader",
      "2026-09-03 12:00:00 UTC"
    )
  )
  revoked <- adapter$session_status(active)
  testthat::expect_identical(
    revoked[c("status", "reader_id")],
    list(status = "revoked", reader_id = NULL)
  )
  DBI::dbExecute(
    store$pool,
    paste(
      "UPDATE reader_external_identities SET revoked_at = NULL",
      "WHERE issuer = $1 AND subject = $2"
    ),
    params = list(config$oidc_issuer, "github|reader")
  )

  testthat::expect_error(
    store_admit_reader_identity(
      store,
      issuer = config$oidc_issuer,
      subject = "google-oauth2|second-reader",
      reader_id = "reader-two",
      responsible_id = "operator:james",
      reason = "unsafe admission"
    ),
    class = "rill_reader_isolation_incomplete"
  )
  reader_count <- DBI::dbGetQuery(
    store$pool,
    "SELECT COUNT(*) AS count FROM readers WHERE reader_id = 'reader-two'"
  )
  testthat::expect_identical(as.integer(reader_count$count[[1L]]), 0L)

  store_ensure_reader(store, "other-reader")
  DBI::dbExecute(
    store$pool,
    paste(
      "INSERT INTO reader_external_identities (",
      "issuer, subject, reader_id, created_at, updated_at",
      ") VALUES ($1, $2, $3, $4, $4)"
    ),
    params = list(
      config$oidc_issuer,
      "github|conflict",
      "other-reader",
      "2026-09-03 12:00:00 UTC"
    )
  )
  testthat::expect_error(
    store_admit_reader_identity(
      store,
      issuer = config$oidc_issuer,
      subject = "github|conflict",
      reader_id = "reader-one",
      responsible_id = "operator:james",
      reason = "unsafe relink"
    ),
    class = "rill_reader_identity_conflict"
  )
  conflicting <- store_get_reader_identity(
    store,
    config$oidc_issuer,
    "github|conflict"
  )
  testthat::expect_identical(conflicting$reader_id, "other-reader")

  store_admit_reader_identity(
    store,
    issuer = config$oidc_issuer,
    subject = "google-oauth2|unknown",
    reader_id = "reader-one",
    responsible_id = "operator:james",
    reason = "invitation approved",
    now = "2026-09-03 12:00:00 UTC"
  )
  store_admit_reader_identity(
    store,
    issuer = config$oidc_issuer,
    subject = "google-oauth2|unknown",
    reader_id = "reader-one",
    responsible_id = "operator:james",
    reason = "idempotent retry",
    now = "2026-09-04 12:00:00 UTC"
  )
  admitted <- reader_identity_resolve(
    adapter,
    identity_test_request("google-oauth2|unknown")
  )
  testthat::expect_identical(
    admitted[c("status", "reader_id")],
    list(status = "active", reader_id = "reader-one")
  )
  approved_admission <- store_get_reader_admission(
    store,
    config$oidc_issuer,
    "google-oauth2|unknown"
  )
  testthat::expect_identical(
    approved_admission[c("status", "decided_at")],
    list(
      status = "approved",
      decided_at = as.POSIXct("2026-09-03 12:00:00", tz = "UTC")
    )
  )

  reattach_subject <- "github|reattached"
  reattach_time <- "2026-09-03 12:00:00 UTC"
  store_admit_reader_identity(
    store,
    issuer = config$oidc_issuer,
    subject = reattach_subject,
    reader_id = "reader-one",
    responsible_id = "operator:james",
    reason = "initial attachment",
    now = reattach_time
  )
  DBI::dbExecute(
    store$pool,
    paste(
      "DELETE FROM reader_external_identities",
      "WHERE issuer = $1 AND subject = $2"
    ),
    params = list(config$oidc_issuer, reattach_subject)
  )
  store_admit_reader_identity(
    store,
    issuer = config$oidc_issuer,
    subject = reattach_subject,
    reader_id = "reader-one",
    responsible_id = "operator:james",
    reason = "reattachment",
    now = reattach_time
  )
  reattach_events <- DBI::dbGetQuery(
    store$pool,
    paste(
      "SELECT event_id FROM reader_identity_events",
      "WHERE issuer = $1 AND subject = $2",
      "ORDER BY event_sequence"
    ),
    params = list(config$oidc_issuer, reattach_subject)
  )
  testthat::expect_identical(nrow(reattach_events), 2L)
  testthat::expect_identical(
    length(unique(reattach_events$event_id)),
    2L
  )

  race_subject <- "google-oauth2|approval-race"
  blocker <- do.call(
    DBI::dbConnect,
    c(list(drv = RPostgres::Postgres()), connection_args)
  )
  blocker_active <- TRUE
  withr::defer({
    if (blocker_active) {
      try(DBI::dbRollback(blocker), silent = TRUE)
    }
    DBI::dbDisconnect(blocker)
  })
  DBI::dbBegin(blocker)
  DBI::dbGetQuery(
    blocker,
    paste(
      "SELECT pg_advisory_xact_lock(",
      "hashtext($1::text), hashtext($2::text))"
    ),
    params = list(config$oidc_issuer, race_subject)
  )

  package_path <- normalizePath(
    testthat::test_path("..", ".."),
    mustWork = FALSE
  )
  if (!file.exists(file.path(package_path, "DESCRIPTION"))) {
    package_path <- NULL
  }
  application_names <- paste0(
    "rill_identity_race_",
    Sys.getpid(),
    "_",
    c("resolve", "admit")
  )
  identity_race_worker <- function(
    package_path,
    connection_args,
    application_name,
    operation,
    issuer,
    subject
  ) {
    if (is.null(package_path)) {
      loadNamespace("rill")
    } else {
      pkgload::load_all(package_path, quiet = TRUE)
    }
    connection_args$application_name <- application_name
    database_pool <- do.call(
      pool::dbPool,
      c(
        list(drv = RPostgres::Postgres()),
        connection_args,
        list(minSize = 1, maxSize = 1, idleTimeout = 60)
      )
    )
    on.exit(pool::poolClose(database_pool), add = TRUE)
    worker_store <- structure(
      list(
        mode = "postgres",
        pool = database_pool,
        private_reader_id = "reader-one"
      ),
      class = "rill_store"
    )
    if (identical(operation, "resolve")) {
      principal <- list(
        issuer = issuer,
        subject = subject,
        email = NULL,
        display_name = NULL
      )
      return(rill:::store_resolve_reader_identity(worker_store, principal))
    }
    rill:::store_admit_reader_identity(
      worker_store,
      issuer = issuer,
      subject = subject,
      reader_id = "reader-one",
      responsible_id = "operator:james",
      reason = "proactive approval"
    )
    invisible(NULL)
  }
  workers <- Map(
    function(application_name, operation) {
      callr::r_bg(
        identity_race_worker,
        args = list(
          package_path = package_path,
          connection_args = connection_args,
          application_name = application_name,
          operation = operation,
          issuer = config$oidc_issuer,
          subject = race_subject
        ),
        stdout = "|",
        stderr = "|",
        supervise = TRUE
      )
    },
    application_names,
    c("resolve", "admit")
  )
  withr::defer({
    for (worker in workers) {
      if (worker$is_alive()) {
        worker$kill_tree()
      }
    }
  })

  blocked <- character()
  deadline <- Sys.time() + 20
  while (Sys.time() < deadline) {
    activity <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT application_name, wait_event_type FROM pg_stat_activity",
        "WHERE application_name IN ($1, $2)"
      ),
      params = as.list(application_names)
    )
    blocked <- activity$application_name[
      activity$wait_event_type == "Lock"
    ]
    if (setequal(blocked, application_names)) {
      break
    }
    Sys.sleep(0.05)
  }
  testthat::expect_setequal(blocked, application_names)

  DBI::dbCommit(blocker)
  blocker_active <- FALSE
  for (worker in workers) {
    worker$wait(timeout = 20000)
    testthat::expect_identical(worker$is_alive(), FALSE)
    worker$get_result()
  }
  workers <- list()
  raced_identity <- store_get_reader_identity(
    store,
    config$oidc_issuer,
    race_subject
  )
  raced_admission <- store_get_reader_admission(
    store,
    config$oidc_issuer,
    race_subject
  )
  admission_status <- if (is.null(raced_admission)) {
    "missing"
  } else {
    raced_admission$status
  }
  testthat::expect_identical(raced_identity$reader_id, "reader-one")
  testthat::expect_in(admission_status, c("missing", "approved"))

  happened_at <- "2099-09-03 12:00:00 UTC"
  store_admit_reader_identity(
    store,
    issuer = config$oidc_issuer,
    subject = "github|audit",
    reader_id = "reader-one",
    responsible_id = "operator:james",
    reason = "audit order",
    now = happened_at
  )
  store_disable_reader(
    store,
    "reader-one",
    responsible_id = "operator:james",
    reason = "access revoked",
    now = happened_at
  )
  audit_events <- store_list_reader_identity_events(store, "reader-one")
  testthat::expect_identical(
    utils::tail(audit_events$action, 2L),
    c("identity_attached", "reader_disabled")
  )
  disabled <- reader_identity_resolve(
    adapter,
    identity_test_request("github|reader")
  )
  capture_handler <- capture_http_handler(
    function(request) NULL,
    store,
    list(actor_id = "reader-one", capture_token = "test-secret")
  )
  capture_response <- capture_handler(list2env(
    list(
      PATH_INFO = capture_endpoint_path,
      REQUEST_METHOD = "POST",
      HTTP_AUTHORIZATION = "Bearer test-secret"
    ),
    parent = emptyenv()
  ))
  testthat::expect_identical(
    disabled[c("status", "reader_id")],
    list(status = "disabled", reader_id = NULL)
  )
  testthat::expect_identical(capture_response$status, 403L)

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
})
