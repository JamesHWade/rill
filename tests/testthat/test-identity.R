testthat::test_that("the private gate denies an unapproved proxy identity", {
  local_proxy_identity(subjects = "google-oauth2|approved")
  app <- rill_app()

  response <- app$httpHandler(identity_test_request("github|unapproved"))

  testthat::expect_identical(response$status, 403L)
  testthat::expect_no_match(
    response$content,
    "github|unapproved",
    fixed = TRUE
  )
  testthat::expect_match(
    response$content,
    "Sign out and try another identity",
    fixed = TRUE
  )
  testthat::expect_match(
    response$content,
    paste0(
      "/oauth2/sign_out?rd=https%3A%2F%2Freader.us.auth0.com",
      "%2Fv2%2Flogout%3Fclient_id%3Dtest-client%26returnTo%3D",
      "https%253A%252F%252Freader.example%252F"
    ),
    fixed = TRUE
  )
})

testthat::test_that("the private gate fails closed without proxy identity", {
  local_proxy_identity()
  app <- rill_app()

  response <- app$httpHandler(identity_test_request())

  testthat::expect_identical(response$status, 403L)
})

testthat::test_that("approved proxy identities open the same private Library", {
  local_proxy_identity(paste(
    "google-oauth2|reader",
    "github|reader",
    sep = ","
  ))
  app <- rill_app()
  responses <- lapply(
    c("google-oauth2|reader", "github|reader"),
    \(subject) app$httpHandler(identity_test_request(subject))
  )

  testthat::expect_identical(
    vapply(responses, `[[`, numeric(1), "status"),
    c(200, 200)
  )
})

testthat::test_that("an unapproved WebSocket session closes before server use", {
  local_proxy_identity(subjects = "google-oauth2|approved")
  app <- rill_app()
  closed <- FALSE
  session <- list(
    request = identity_test_request("github|unapproved"),
    close = function() {
      closed <<- TRUE
      invisible(NULL)
    }
  )

  app$serverFuncSource()(list(), list(), session)

  testthat::expect_identical(closed, TRUE)
})

testthat::test_that("the private gate exposes only a minimal internal health check", {
  local_proxy_identity()
  app <- rill_app()

  response <- app$httpHandler(identity_test_request(path = "/_rill/health"))

  testthat::expect_identical(
    list(status = response$status, content = response$content),
    list(status = 200L, content = "ok\n")
  )
})

testthat::test_that("capture authentication remains independent of Reader login", {
  local_proxy_identity(capture_token = "capture-secret")
  app <- rill_app()
  request <- identity_test_request(
    path = capture_endpoint_path,
    method = "POST",
    authorization = "Bearer wrong-secret"
  )

  response <- app$httpHandler(request)

  testthat::expect_identical(response$status, 401L)
})

testthat::test_that("an Auth0 token resolves through exact issuer and subject", {
  local_auth0_identity()
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)

  resolution <- reader_identity_resolve(
    adapter,
    identity_test_auth0_token(
      "auth0|reader",
      email = "reader@example.com",
      display_name = "Reader"
    )
  )

  testthat::expect_identical(
    resolution[c("status", "reader_id")],
    list(status = "active", reader_id = "private-reader")
  )
  identity <- store_get_reader_identity(
    store,
    "https://reader.us.auth0.com/",
    "auth0|reader"
  )
  testthat::expect_identical(identity$email, "reader@example.com")
  testthat::expect_identical(identity$display_name, "Reader")
})

testthat::test_that("Auth0 discovery uses the exact configured issuer", {
  config <- list(
    auth0_domain = "reader.us.auth0.com",
    auth0_client_id = "reader-client",
    auth0_client_secret = "reader-secret",
    auth0_redirect_uri = "https://reader.example/",
    oidc_issuer = "https://reader.us.auth0.com/"
  )
  provider_args <- NULL
  testthat::local_mocked_bindings(
    identity_oidc_provider = function(...) {
      provider_args <<- list(...)
      "test-provider"
    },
    identity_oauth_client = \(...) list(...)
  )

  identity_auth0_client(config)

  testthat::expect_identical(
    provider_args[c("issuer", "name")],
    list(issuer = config$oidc_issuer, name = "auth0")
  )
})

testthat::test_that("the in-app gate starts Rill only after authentication", {
  local_auth0_identity()
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  auth <- shiny::reactiveValues(authenticated = FALSE, token = NULL)
  started_reader <- NULL
  testthat::local_mocked_bindings(
    identity_oauth_module_server = function(...) auth
  )
  server <- identity_server_handler(
    function(input, output, session, reader_id) {
      started_reader <<- reader_id
    },
    adapter
  )

  shiny::testServer(server, {
    testthat::expect_null(started_reader)
    auth$token <- identity_test_auth0_token("auth0|reader")
    auth$authenticated <- TRUE
    session$flushReact()
    testthat::expect_identical(started_reader, "private-reader")

    auth$authenticated <- FALSE
    session$flushReact()
    testthat::expect_identical(session$isClosed(), TRUE)
  })
})

testthat::test_that("the in-app gate denies unknown subjects before DB access", {
  local_auth0_identity(subjects = "auth0|approved")
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  auth <- shiny::reactiveValues(authenticated = FALSE, token = NULL)
  started <- FALSE
  testthat::local_mocked_bindings(
    identity_oauth_module_server = function(...) auth
  )
  server <- identity_server_handler(
    function(...) started <<- TRUE,
    adapter
  )

  shiny::testServer(server, {
    auth$token <- identity_test_auth0_token("auth0|unknown")
    auth$authenticated <- TRUE
    session$flushReact()
    testthat::expect_identical(started, FALSE)
    testthat::expect_null(store_get_reader_admission(
      store,
      config$oidc_issuer,
      "auth0|unknown"
    ))
  })
})

testthat::test_that("the in-app gate leaves only the static HTTP shell public", {
  local_auth0_identity()
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  handler <- identity_http_handler(
    \(request) identity_health_response(),
    adapter
  )

  response <- handler(identity_test_request(remote_addr = "203.0.113.10"))

  testthat::expect_identical(response$status, 200L)
  testthat::expect_identical(response$content, "ok\n")
})

testthat::test_that("the local identity adapter ignores forwarded claims", {
  withr::local_envvar(c(
    DATABASE_URL = "",
    RILL_ACTOR_ID = "local-reader",
    RILL_IDENTITY_MODE = "local"
  ))
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)

  resolution <- reader_identity_resolve(
    adapter,
    identity_test_request("forged-subject")
  )

  testthat::expect_identical(
    resolution[c("status", "reader_id")],
    list(status = "active", reader_id = "local-reader")
  )
})

testthat::test_that("the proxy adapter ignores non-loopback forwarded claims", {
  local_proxy_identity(subjects = "github|reader")
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)

  resolution <- reader_identity_resolve(
    adapter,
    identity_test_request(
      "github|forged",
      remote_addr = "203.0.113.10"
    )
  )

  testthat::expect_identical(
    resolution[c("status", "reader_id")],
    list(status = "missing", reader_id = NULL)
  )
  testthat::expect_null(store_get_reader_admission(
    store,
    config$oidc_issuer,
    "github|forged"
  ))
})

testthat::test_that("the local identity adapter denies a disabled Reader", {
  withr::local_envvar(c(
    DATABASE_URL = "",
    RILL_ACTOR_ID = "local-reader",
    RILL_IDENTITY_MODE = "local"
  ))
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  store_disable_reader(
    store,
    "local-reader",
    responsible_id = "operator:james",
    reason = "access revoked"
  )

  resolution <- reader_identity_resolve(
    adapter,
    identity_test_request("forged-subject")
  )

  testthat::expect_identical(
    resolution[c("status", "reader_id")],
    list(status = "disabled", reader_id = NULL)
  )
})

testthat::test_that("the local adapter uses the shared server wrapper", {
  withr::local_envvar(c(
    DATABASE_URL = "",
    RILL_ACTOR_ID = "local-reader",
    RILL_IDENTITY_MODE = "local"
  ))
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  resolved_reader_id <- NULL
  base_server <- function(input, output, session, reader_id) {
    resolved_reader_id <<- reader_id
  }
  session <- list(
    request = identity_test_request("forged-subject"),
    close = \() testthat::fail("an active Reader session was closed")
  )

  identity_server_handler(base_server, adapter)(list(), list(), session)

  testthat::expect_identical(resolved_reader_id, "local-reader")
})

testthat::test_that("configured OIDC identities resolve to the fixed Reader", {
  local_proxy_identity(paste(
    "google-oauth2|reader",
    "github|reader",
    sep = ","
  ))
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)

  resolutions <- lapply(
    c("google-oauth2|reader", "github|reader"),
    function(subject) {
      reader_identity_resolve(
        adapter,
        identity_test_request(subject)
      )
    }
  )

  testthat::expect_identical(
    vapply(resolutions, `[[`, character(1), "reader_id"),
    c("private-reader", "private-reader")
  )
  testthat::expect_identical(
    vapply(resolutions, `[[`, character(1), "status"),
    c("active", "active")
  )
})

testthat::test_that("unknown OIDC identities create one pending admission", {
  local_proxy_identity()
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  request <- identity_test_request(
    "github|unknown",
    email = "old@example.com",
    display_name = "Old name"
  )

  first <- reader_identity_resolve(adapter, request)
  second <- reader_identity_resolve(
    adapter,
    identity_test_request(
      "github|unknown",
      email = "new@example.com",
      display_name = "New name"
    )
  )
  admission <- store_get_reader_admission(
    store,
    config$oidc_issuer,
    "github|unknown"
  )

  testthat::expect_identical(
    c(first$status, second$status),
    c("pending", "pending")
  )
  testthat::expect_identical(
    admission[c("status", "email", "display_name", "attempt_count")],
    list(
      status = "pending",
      email = "new@example.com",
      display_name = "New name",
      attempt_count = 2L
    )
  )
})

testthat::test_that("operator approval attaches an identity to one Reader", {
  local_proxy_identity()
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  request <- identity_test_request("google-oauth2|invited")

  pending <- reader_identity_resolve(adapter, request)
  store_admit_reader_identity(
    store,
    issuer = config$oidc_issuer,
    subject = "google-oauth2|invited",
    reader_id = "private-reader",
    responsible_id = "operator:james",
    reason = "invitation approved",
    now = "2026-09-03 12:00:00 UTC"
  )
  store_admit_reader_identity(
    store,
    issuer = config$oidc_issuer,
    subject = "google-oauth2|invited",
    reader_id = "private-reader",
    responsible_id = "operator:james",
    reason = "idempotent retry",
    now = "2026-09-04 12:00:00 UTC"
  )
  active <- reader_identity_resolve(adapter, request)
  admission <- store_get_reader_admission(
    store,
    config$oidc_issuer,
    "google-oauth2|invited"
  )
  events <- store_list_reader_identity_events(store, "private-reader")

  testthat::expect_identical(pending$status, "pending")
  testthat::expect_identical(
    active[c("status", "reader_id")],
    list(status = "active", reader_id = "private-reader")
  )
  testthat::expect_identical(admission$status, "approved")
  testthat::expect_identical(
    admission$decided_at,
    "2026-09-03 12:00:00 UTC"
  )
  operator_event <- events[events$responsible_id == "operator:james", ]
  testthat::expect_identical(
    operator_event$action,
    "identity_attached"
  )
  testthat::expect_identical(
    operator_event$responsible_id,
    "operator:james"
  )
  testthat::expect_identical(
    operator_event$reason,
    "invitation approved"
  )
})

testthat::test_that("reattaching an identity creates a distinct audit event", {
  local_proxy_identity()
  config <- rill_config()
  store <- rill_store(config)
  reader_identity_adapter(config, store)
  happened_at <- "2026-09-03 12:00:00 UTC"

  store_admit_reader_identity(
    store,
    issuer = config$oidc_issuer,
    subject = "github|reattached",
    reader_id = "private-reader",
    responsible_id = "operator:james",
    reason = "initial attachment",
    now = happened_at
  )
  identities <- store$memory$reader_identities
  store$memory$reader_identities <- identities[
    identities$subject != "github|reattached",
  ]
  store_admit_reader_identity(
    store,
    issuer = config$oidc_issuer,
    subject = "github|reattached",
    reader_id = "private-reader",
    responsible_id = "operator:james",
    reason = "reattachment",
    now = happened_at
  )

  events <- store$memory$reader_identity_events
  events <- events[
    events$reason %in% c("initial attachment", "reattachment"),
  ]
  testthat::expect_identical(nrow(events), 2L)
  testthat::expect_identical(length(unique(events$event_id)), 2L)
})

testthat::test_that("admission creates an isolated second Reader", {
  local_proxy_identity(subjects = "github|reader")
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)

  store_admit_reader_identity(
    store,
    issuer = config$oidc_issuer,
    subject = "github|second-reader",
    reader_id = "another-reader",
    responsible_id = "operator:james",
    reason = "invitation approved"
  )
  admitted <- reader_identity_resolve(
    adapter,
    identity_test_request("github|second-reader")
  )

  testthat::expect_identical(
    admitted[c("status", "reader_id")],
    list(status = "active", reader_id = "another-reader")
  )
  testthat::expect_identical(
    nrow(store_list_feeds(store, "another-reader")),
    0L
  )
  testthat::expect_gt(nrow(store_list_feeds(store, "private-reader")), 0L)
})

testthat::test_that("an existing external identity cannot be relinked", {
  local_proxy_identity(subjects = "github|reader")
  config <- rill_config()
  store <- rill_store(config)
  reader_identity_adapter(config, store)
  store_ensure_reader(store, "other-reader")
  conflicting <- store$memory$reader_identities[1L, , drop = FALSE]
  conflicting$subject <- "github|conflict"
  conflicting$reader_id <- "other-reader"
  store$memory$reader_identities <- rbind(
    store$memory$reader_identities,
    conflicting
  )

  testthat::expect_error(
    store_admit_reader_identity(
      store,
      issuer = config$oidc_issuer,
      subject = "github|conflict",
      reader_id = "private-reader",
      responsible_id = "operator:james",
      reason = "unsafe relink"
    ),
    class = "rill_reader_identity_conflict"
  )
  identity <- store_get_reader_identity(
    store,
    config$oidc_issuer,
    "github|conflict"
  )
  testthat::expect_identical(identity$reader_id, "other-reader")
})

testthat::test_that("repeat OIDC resolution updates mutable profile metadata", {
  local_proxy_identity(subjects = "github|reader")
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)

  first <- reader_identity_resolve(
    adapter,
    identity_test_request(
      "github|reader",
      email = "old@example.com",
      display_name = "Old name"
    )
  )
  second <- reader_identity_resolve(
    adapter,
    identity_test_request(
      "github|reader",
      email = "new@example.com",
      display_name = "New name"
    )
  )
  third <- reader_identity_resolve(
    adapter,
    identity_test_request("github|reader")
  )
  identity <- store_get_reader_identity(
    store,
    config$oidc_issuer,
    "github|reader"
  )

  testthat::expect_identical(
    vapply(list(first, second, third), `[[`, character(1), "reader_id"),
    rep("private-reader", 3L)
  )
  testthat::expect_identical(
    identity[c("email", "display_name")],
    list(email = "new@example.com", display_name = "New name")
  )
})

testthat::test_that("disabled Readers fail closed at identity resolution", {
  local_proxy_identity(subjects = "github|reader")
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  store_disable_reader(
    store,
    "private-reader",
    responsible_id = "operator:james",
    reason = "access revoked"
  )

  resolution <- reader_identity_resolve(
    adapter,
    identity_test_request("github|reader")
  )
  events <- store_list_reader_identity_events(store, "private-reader")

  testthat::expect_identical(
    resolution[c("status", "reader_id")],
    list(status = "disabled", reader_id = NULL)
  )
  testthat::expect_identical(
    events$action,
    c("identity_attached", "reader_disabled")
  )
  testthat::expect_identical(
    events$responsible_id,
    c("configuration", "operator:james")
  )
})

testthat::test_that("identity audit events preserve tied insertion order", {
  withr::local_envvar(c(DATABASE_URL = "", RILL_IDENTITY_MODE = "local"))
  config <- rill_config()
  store <- rill_store(config)
  happened_at <- "2026-09-03 12:00:00 UTC"
  store_admit_reader_identity(
    store,
    issuer = "https://reader.example/",
    subject = "github|reader",
    reader_id = config$actor_id,
    responsible_id = "operator:james",
    reason = "invitation approved",
    now = happened_at
  )
  store_disable_reader(
    store,
    config$actor_id,
    responsible_id = "operator:james",
    reason = "access revoked",
    now = happened_at
  )

  events <- store_list_reader_identity_events(store, config$actor_id)

  testthat::expect_identical(
    events$action,
    c("identity_attached", "reader_disabled")
  )
})

testthat::test_that("the server receives the Reader resolved by its adapter", {
  local_proxy_identity(subjects = "github|reader")
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  resolved_reader_id <- NULL
  base_server <- function(input, output, session, reader_id) {
    resolved_reader_id <<- reader_id
  }
  session <- list(
    request = identity_test_request("github|reader"),
    close = \() testthat::fail("an active Reader session was closed")
  )

  identity_server_handler(base_server, adapter)(list(), list(), session)

  testthat::expect_identical(resolved_reader_id, "private-reader")
})

testthat::test_that("an active session closes when its Reader is disabled", {
  withr::local_envvar(c(
    DATABASE_URL = "",
    RILL_ACTOR_ID = "local-reader",
    RILL_IDENTITY_MODE = "local"
  ))
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  session <- shiny::MockShinySession$new()
  server <- identity_server_handler(
    function(input, output, session, reader_id) invisible(NULL),
    adapter
  )
  server(list(), list(), session)
  session$flushReact()

  store_disable_reader(
    store,
    "local-reader",
    responsible_id = "operator:james",
    reason = "access revoked"
  )
  session$elapse(1000)

  testthat::expect_identical(session$isClosed(), TRUE)
})

testthat::test_that("an active session closes when its identity is revoked", {
  local_proxy_identity(subjects = "github|reader")
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  resolution <- reader_identity_resolve(
    adapter,
    identity_test_request("github|reader")
  )
  session <- shiny::MockShinySession$new()
  reader_identity_guard_session(adapter, resolution, session)
  session$flushReact()

  identity_index <- which(
    store$memory$reader_identities$issuer == config$oidc_issuer &
      store$memory$reader_identities$subject == "github|reader"
  )
  store$memory$reader_identities$revoked_at[[identity_index]] <- utc_now()
  session$elapse(1000)
  revoked <- reader_identity_resolve(
    adapter,
    identity_test_request("github|reader")
  )

  testthat::expect_identical(session$isClosed(), TRUE)
  testthat::expect_identical(
    revoked[c("status", "reader_id")],
    list(status = "revoked", reader_id = NULL)
  )
})

testthat::test_that("an active session closes when reauthorization errors", {
  withr::local_envvar(c(
    DATABASE_URL = "",
    RILL_ACTOR_ID = "local-reader",
    RILL_IDENTITY_MODE = "local"
  ))
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  resolution <- reader_identity_resolve(
    adapter,
    identity_test_request()
  )
  adapter$session_status <- function(resolution) {
    stop("the identity store is unavailable")
  }
  session <- shiny::MockShinySession$new()

  reader_identity_guard_session(adapter, resolution, session)
  session$flushReact()

  testthat::expect_identical(session$isClosed(), TRUE)
})
