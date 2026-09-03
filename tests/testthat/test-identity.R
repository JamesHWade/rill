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
