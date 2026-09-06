test_that("Orientation diagnostics retain nested classes without exception payloads", {
  parent <- rlang::error_cnd(
    "httr2_http_401",
    message = "sk-secret https://private.example/document Reader text",
    call = quote(request("sk-secret")),
    response = list(body = "Reader text", status_code = 503L)
  )
  error <- rlang::error_cnd("rlib_error_3_0", parent = parent)
  attributes <- orientation_error_attributes(error)

  expect_identical(attributes$error.type, "rlib_error_3_0")
  expect_identical(attributes$error.root_type, "httr2_http_401")
  expect_identical(attributes$error.chain_depth, 2L)
  expect_identical(attributes$error.chain_truncated, FALSE)
  expect_identical(attributes$http.response.status_code, 401L)
  expect_setequal(
    attributes$error.classes,
    c("rlib_error_3_0", "rlang_error", "error", "condition", "httr2_http_401")
  )
  expect_no_match(
    as.character(canonical_json(attributes)),
    "sk-secret|private.example|Reader text|request|503"
  )
})

test_that("Orientation diagnostic chains are bounded and reject malformed labels", {
  error <- simpleError("Private source text")
  for (index in seq_len(10L)) {
    error <- rlang::error_cnd("rlib_error_3_0", parent = error)
  }
  attributes <- orientation_error_attributes(error)
  expect_identical(attributes$error.chain_depth, 8L)
  expect_identical(attributes$error.chain_truncated, TRUE)
  expect_null(attributes$error.root_type)

  class(error) <- c("https://private.example", "error", "condition")
  attributes <- orientation_error_attributes(error)
  expect_identical(attributes$error.type, "unknown")
  expect_no_match(as.character(canonical_json(attributes)), "private.example")
  expect_identical(
    orientation_error_attributes("private")$error.type,
    "unknown"
  )
})

test_that("Orientation diagnostics preserve the original failure through cleanup", {
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

  expect_identical(attributes$orientation.failure_stage, "output_validation")
  expect_identical(attributes$error.type, "rill_orientation_invalid")
  expect_identical(attributes$orientation.source_calls, 1L)
  expect_identical(attributes$orientation.submission_attempts, 1L)
  expect_identical(attributes$orientation.submission_calls, 0L)
  expect_no_match(
    as.character(canonical_json(attributes)),
    "Reader text|password"
  )
})
