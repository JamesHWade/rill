testthat::test_that("the package exposes a focused application API", {
  testthat::expect_setequal(
    getNamespaceExports("rill"),
    c(
      "poll_feeds",
      "prepare_today",
      "read_opml",
      "rill_app",
      "write_opml"
    )
  )
})

testthat::test_that("rill_app creates a Shiny application in demo mode", {
  withr::local_envvar(DATABASE_URL = "")

  app <- rill_app()

  testthat::expect_s3_class(app, "shiny.appobj")
  testthat::expect_type(app$serverFuncSource, "closure")
})

testthat::test_that("rill_app mounts the capture route", {
  withr::local_envvar(c(
    DATABASE_URL = "",
    RILL_CAPTURE_TOKEN = "test-secret"
  ))
  app <- rill_app()
  request <- list2env(
    list(
      PATH_INFO = capture_endpoint_path,
      REQUEST_METHOD = "POST",
      HTTP_AUTHORIZATION = "Bearer wrong-secret"
    ),
    parent = emptyenv()
  )

  response <- app$httpHandler(request)

  testthat::expect_identical(response$status, 401L)
})

testthat::test_that("installed runtime assets are available", {
  assets <- c(
    rill_package_file("app", "_brand.yml"),
    rill_package_file("app", "www", "app.js"),
    rill_package_file("app", "www", "rill-duck-dark.png"),
    rill_package_file("app", "www", "rill-duck.png"),
    rill_package_file("app", "www", "styles.css"),
    rill_package_file("sql", "001_init.sql")
  )

  testthat::expect_length(assets, 6L)
  testthat::expect_identical(file.exists(assets), rep(TRUE, 6L))
})

testthat::test_that("polling requires durable configuration", {
  withr::local_envvar(DATABASE_URL = "")

  testthat::expect_snapshot(poll_feeds(), error = TRUE)
})

testthat::test_that("preparing today requires durable configuration", {
  withr::local_envvar(DATABASE_URL = "")

  testthat::expect_snapshot(prepare_today(), error = TRUE)
})
