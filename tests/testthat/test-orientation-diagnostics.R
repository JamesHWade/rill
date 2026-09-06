testthat::test_that("Orientation diagnostics retain nested classes without exception payloads", {
  parent <- rlang::error_cnd(
    "httr2_http_401",
    message = "sk-secret https://private.example/document Reader text",
    call = quote(request("sk-secret")),
    response = list(body = "Reader text", status_code = 503L)
  )
  error <- rlang::error_cnd("rlib_error_3_0", parent = parent)
  attributes <- orientation_error_attributes(error)

  testthat::expect_identical(attributes$error.type, "rlib_error_3_0")
  testthat::expect_identical(attributes$error.root_type, "httr2_http_401")
  testthat::expect_identical(attributes$error.chain_depth, 2L)
  testthat::expect_identical(attributes$error.chain_truncated, FALSE)
  testthat::expect_identical(attributes$http.response.status_code, 401L)
  testthat::expect_setequal(
    attributes$error.classes,
    c("rlib_error_3_0", "rlang_error", "error", "condition", "httr2_http_401")
  )
  testthat::expect_no_match(
    as.character(canonical_json(attributes)),
    "sk-secret|private.example|Reader text|request|503"
  )
})

testthat::test_that("Orientation diagnostic chains are bounded and reject malformed labels", {
  error <- simpleError("Private source text")
  for (index in seq_len(10L)) {
    error <- rlang::error_cnd("rlib_error_3_0", parent = error)
  }
  attributes <- orientation_error_attributes(error)
  testthat::expect_identical(attributes$error.chain_depth, 8L)
  testthat::expect_identical(attributes$error.chain_truncated, TRUE)
  testthat::expect_null(attributes$error.root_type)

  class(error) <- c("https://private.example", "error", "condition")
  attributes <- orientation_error_attributes(error)
  testthat::expect_identical(attributes$error.type, "unknown")
  testthat::expect_no_match(
    as.character(canonical_json(attributes)),
    "private.example"
  )
  testthat::expect_identical(
    orientation_error_attributes("private")$error.type,
    "unknown"
  )
})

testthat::test_that("Orientation diagnostics preserve the original failure through cleanup", {
  diagnostics <- orientation_diagnostics()
  agent <- new.env(parent = emptyenv())
  state <- orientation_test_tool_state(agent, source_calls = 1L)
  state$submission_attempts <- 1L
  diagnostics$agent(agent)
  diagnostics$at("output_validation")
  original <- rlang::error_cnd(
    "rill_orientation_invalid",
    message = "Reader text"
  )
  diagnostics$capture(original)
  diagnostics$at("publication")
  attributes <- diagnostics$attributes(simpleError("Database password"))

  testthat::expect_identical(
    attributes$orientation.failure_stage,
    "output_validation"
  )
  testthat::expect_identical(attributes$error.type, "rill_orientation_invalid")
  testthat::expect_identical(attributes$orientation.source_calls, 1L)
  testthat::expect_identical(attributes$orientation.submission_attempts, 1L)
  testthat::expect_identical(attributes$orientation.submission_calls, 0L)
  testthat::expect_no_match(
    as.character(canonical_json(attributes)),
    "Reader text|password"
  )
})

testthat::test_that("Orientation exports only known class identifiers at every depth", {
  private_classes <- c(
    "sk_secret",
    "request_123456789",
    "rill_orientation_sk_secret",
    "httr2_http_429_sk_secret"
  )
  parent <- rlang::error_cnd(private_classes, message = "Private source")
  wrapper <- rlang::error_cnd("rlib_error_3_0", parent = parent)
  for (error in list(parent, wrapper)) {
    attributes <- orientation_error_attributes(error)
    testthat::expect_identical(attributes$error.root_type, "unknown")
    testthat::expect_null(attributes$http.response.status_code)
    testthat::expect_setequal(
      attributes$error.classes,
      c(
        "unknown",
        "rlang_error",
        "error",
        "condition",
        if (identical(error, wrapper)) "rlib_error_3_0"
      )
    )
    testthat::expect_no_match(
      as.character(canonical_json(attributes)),
      "sk_secret|request_123456789|Private source"
    )
  }
})
