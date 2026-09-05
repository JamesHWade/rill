testthat::test_that("owner approvals create an audited, isolated Library", {
  local_proxy_identity()
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  resolution <- reader_identity_resolve(
    adapter,
    identity_test_request("auth0|reader")
  )
  invited <- identity_test_request(
    "auth0|invited",
    "invited@example.com",
    "Invited Reader"
  )
  reader_identity_resolve(adapter, invited)
  before <- store_list_feeds(store, config$actor_id)
  request_id <- store_list_reader_admissions(store)$request_id[[1L]]

  shiny::testServer(
    access_requests_server,
    args = list(
      store = store,
      adapter = adapter,
      resolution = resolution
    ),
    {
      testthat::expect_match(
        output$launcher$html,
        "Access requests",
        fixed = TRUE
      )
      session$setInputs(open = 1L)
      testthat::expect_match(
        output$pending$html,
        "Invited Reader",
        fixed = TRUE
      )
      testthat::expect_no_match(
        output$pending$html,
        "auth0|invited",
        fixed = TRUE
      )
      session$setInputs(request = request_id)
      testthat::expect_match(
        output$selection$html,
        "invited@example.com",
        fixed = TRUE
      )
      session$setInputs(approve = 1L)
      testthat::expect_match(
        output$status$html,
        "Access approved",
        fixed = TRUE
      )
      testthat::expect_match(
        output$pending$html,
        "No pending access requests",
        fixed = TRUE
      )
      session$setInputs(approve = 2L)
    }
  )

  admitted <- reader_identity_resolve(adapter, invited)
  testthat::expect_identical(admitted$status, "active")
  testthat::expect_length(
    store_list_feeds(store, admitted$reader_id)$feed_id,
    0L
  )
  testthat::expect_identical(store_list_feeds(store, config$actor_id), before)
  events <- store_list_reader_identity_events(store, admitted$reader_id)
  testthat::expect_identical(events$action, "identity_attached")
  testthat::expect_identical(events$responsible_id, config$actor_id)
  testthat::expect_identical(
    events$reason,
    "Invitation approved in Access requests"
  )
})

testthat::test_that("the panel persists approval and denies a disabled owner in PostgreSQL", {
  local_proxy_identity()
  config <- rill_config()
  store <- local_orientation_backend_store("postgres", config$actor_id)
  adapter <- reader_identity_adapter(config, store)
  resolution <- reader_identity_resolve(
    adapter,
    identity_test_request("auth0|reader")
  )
  invited <- identity_test_request("auth0|invited", "invited@example.com")
  reader_identity_resolve(adapter, invited)
  request_id <- store_list_reader_admissions(store)$request_id[[1L]]
  shiny::testServer(
    access_requests_server,
    args = list(
      store = store,
      adapter = adapter,
      resolution = resolution
    ),
    {
      session$setInputs(open = 1L, request = request_id)
      session$setInputs(approve = 1L)
      testthat::expect_identical(
        status(),
        "Access approved. The Reader can reload Rill to open their Library."
      )
      store_disable_reader(
        store,
        config$actor_id,
        "operator",
        "test disabled owner"
      )
      session$setInputs(refresh = 1L)
      testthat::expect_null(requests())
    }
  )
  admitted <- reader_identity_resolve(adapter, invited)
  testthat::expect_identical(admitted$status, "active")
  testthat::expect_length(
    store_list_feeds(store, admitted$reader_id)$feed_id,
    0L
  )
  events <- store_list_reader_identity_events(store, admitted$reader_id)
  testthat::expect_identical(events$responsible_id, config$actor_id)
})

testthat::test_that("other Readers cannot list or approve with forged inputs", {
  local_proxy_identity()
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  store_admit_reader_identity(
    store,
    config$oidc_issuer,
    "auth0|other",
    reader_id = "other-reader",
    responsible_id = "operator",
    reason = "invited"
  )
  resolution <- reader_identity_resolve(
    adapter,
    identity_test_request("auth0|other")
  )
  calls <- character()
  testthat::local_mocked_bindings(
    store_list_reader_admissions = function(...) {
      calls <<- c(calls, "list")
      stop("Unauthorized list")
    },
    store_approve_reader_admission = function(...) {
      calls <<- c(calls, "approve")
      stop("Unauthorized approval")
    }
  )
  shiny::testServer(
    access_requests_server,
    args = list(
      store = store,
      adapter = adapter,
      resolution = resolution
    ),
    {
      session$setInputs(
        open = 1L,
        refresh = 1L,
        request = "forged",
        approve = 1L
      )
      testthat::expect_null(output$launcher)
      testthat::expect_null(output$pending)
      testthat::expect_null(output$selection)
      testthat::expect_null(requests())
      testthat::expect_length(calls, 0L)
    }
  )
})

testthat::test_that("owner authorization fails closed for local and stale identities", {
  local_proxy_identity()
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  resolution <- reader_identity_resolve(
    adapter,
    identity_test_request("auth0|reader")
  )
  testthat::expect_identical(access_requests_owner(adapter, resolution), TRUE)
  local <- adapter
  local$kind <- "local"
  testthat::expect_identical(access_requests_owner(local, resolution), FALSE)
  for (current in list(
    reader_identity_resolution(NULL, "disabled"),
    reader_identity_resolution("another-reader"),
    NULL
  )) {
    stale <- adapter
    stale$session_status <- function(...) current
    testthat::expect_identical(access_requests_owner(stale, resolution), FALSE)
  }
  adapter$session_status <- function(...) stop("Database unavailable")
  testthat::expect_identical(access_requests_owner(adapter, resolution), FALSE)
})

testthat::test_that("owner revocation blocks an already open approval panel", {
  local_proxy_identity()
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  resolution <- reader_identity_resolve(
    adapter,
    identity_test_request("auth0|reader")
  )
  reader_identity_resolve(adapter, identity_test_request("auth0|invited"))
  request_id <- store_list_reader_admissions(store)$request_id[[1L]]
  shiny::testServer(
    access_requests_server,
    args = list(
      store = store,
      adapter = adapter,
      resolution = resolution
    ),
    {
      session$setInputs(open = 1L, request = request_id)
      store$memory$reader_identities$revoked_at <- utc_now()
      session$setInputs(approve = 1L)
      testthat::expect_null(requests())
      testthat::expect_identical(opened(), FALSE)
    }
  )
  testthat::expect_identical(
    store_list_reader_admissions(store)$status,
    "pending"
  )
  testthat::expect_null(store_get_reader_identity(
    store,
    config$oidc_issuer,
    "auth0|invited"
  ))
})

testthat::test_that("refresh and failed approvals keep requests recoverable", {
  local_proxy_identity()
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  resolution <- reader_identity_resolve(
    adapter,
    identity_test_request("auth0|reader")
  )
  shiny::testServer(
    access_requests_server,
    args = list(
      store = store,
      adapter = adapter,
      resolution = resolution
    ),
    {
      session$setInputs(open = 1L)
      testthat::expect_match(output$pending$html, "No pending", fixed = TRUE)
      reader_identity_resolve(adapter, identity_test_request("auth0|invited"))
      session$setInputs(refresh = 1L)
      testthat::expect_match(
        output$pending$html,
        "Unnamed Reader",
        fixed = TRUE
      )
      request_id <- requests()$request_id[[1L]]
      session$setInputs(request = "unknown", approve = 1L)
      testthat::expect_null(output$selection)
      testthat::expect_identical(
        store_list_reader_admissions(store)$status,
        "pending"
      )
      session$setInputs(request = request_id)
      store$memory$reader_admissions <- store$memory$reader_admissions[FALSE, ]
      session$setInputs(approve = 2L)
      testthat::expect_match(
        output$status$html,
        "could not be approved",
        fixed = TRUE
      )
      testthat::expect_match(output$pending$html, "No pending", fixed = TRUE)
    }
  )
})

testthat::test_that("profile claims are escaped in the Access requests panel", {
  local_proxy_identity()
  config <- rill_config()
  store <- rill_store(config)
  adapter <- reader_identity_adapter(config, store)
  resolution <- reader_identity_resolve(
    adapter,
    identity_test_request("auth0|reader")
  )
  reader_identity_resolve(
    adapter,
    identity_test_request(
      "auth0|invited",
      "<img src=x onerror=alert(1)>",
      "<script>alert(1)</script>"
    )
  )
  request_id <- store_list_reader_admissions(store)$request_id[[1L]]
  shiny::testServer(
    access_requests_server,
    args = list(
      store = store,
      adapter = adapter,
      resolution = resolution
    ),
    {
      session$setInputs(open = 1L, request = request_id)
      testthat::expect_match(
        output$selection$html,
        "&lt;script&gt;",
        fixed = TRUE
      )
      testthat::expect_no_match(output$selection$html, "<script>", fixed = TRUE)
      testthat::expect_no_match(output$pending$html, "<img", fixed = TRUE)
    }
  )
})

testthat::test_that("the app initializes Access requests after Auth0 authentication", {
  local_auth0_identity()
  auth <- shiny::reactiveValues(authenticated = FALSE, token = NULL)
  testthat::local_mocked_bindings(
    identity_oauth_module_server = function(...) auth,
    rill_server = function(...) function(input, output, session, reader_id) NULL
  )
  app <- rill_app()
  shiny::testServer(app$serverFuncSource(), {
    auth$token <- identity_test_auth0_token("auth0|reader")
    auth$authenticated <- TRUE
    session$flushReact()
    testthat::expect_match(
      output[["access_requests-launcher"]]$html,
      "Access requests",
      fixed = TRUE
    )
    session$setInputs(`access_requests-open` = 1L)
    testthat::expect_match(
      output[["access_requests-pending"]]$html,
      "No pending",
      fixed = TRUE
    )
  })
})
