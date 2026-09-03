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
    \(subject) {
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
    reader_id = "reader-invited",
    responsible_id = "operator:james",
    reason = "invitation approved"
  )
  active <- reader_identity_resolve(adapter, request)
  admission <- store_get_reader_admission(
    store,
    config$oidc_issuer,
    "google-oauth2|invited"
  )
  events <- store_list_reader_identity_events(store, "reader-invited")

  testthat::expect_identical(pending$status, "pending")
  testthat::expect_identical(
    active[c("status", "reader_id")],
    list(status = "active", reader_id = "reader-invited")
  )
  testthat::expect_identical(admission$status, "approved")
  testthat::expect_identical(
    events[c("action", "responsible_id", "reason")],
    data.frame(
      action = "identity_attached",
      responsible_id = "operator:james",
      reason = "invitation approved",
      stringsAsFactors = FALSE
    )
  )
})

testthat::test_that("an external identity cannot be relinked implicitly", {
  local_proxy_identity(subjects = "github|reader")
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)

  testthat::expect_error(
    store_admit_reader_identity(
      store,
      issuer = config$oidc_issuer,
      subject = "github|reader",
      reader_id = "another-reader",
      responsible_id = "operator:james",
      reason = "unsafe relink"
    ),
    class = "rill_reader_identity_conflict"
  )
  resolution <- reader_identity_resolve(
    adapter,
    identity_test_request("github|reader")
  )
  testthat::expect_identical(resolution$reader_id, "private-reader")
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
