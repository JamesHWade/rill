testthat::test_that("configuration defaults to the bundled demo", {
  withr::local_envvar(c(
    DATABASE_URL = "",
    RILL_ACTOR_ID = NA,
    RILL_ENV = NA,
    DEFUDDLE_BACKEND = NA,
    DEFUDDLE_COMMAND = NA,
    RILL_AGENT_MODEL = NA,
    RILL_CAPTURE_TOKEN = NA,
    RILL_REFRESH_ON_START = NA,
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))

  config <- rill_config()

  testthat::expect_identical(config$app_name, "Rill")
  testthat::expect_identical(config$app_env, "development")
  testthat::expect_identical(config$actor_id, "reader")
  testthat::expect_identical(config$demo_mode, TRUE)
  testthat::expect_identical(config$defuddle_backend, "hosted")
  testthat::expect_identical(config$defuddle_command, "defuddle")
  testthat::expect_identical(config$agent_model, "openai")
  testthat::expect_identical(config$capture_token, "")
  testthat::expect_identical(config$refresh_on_start, FALSE)
})

testthat::test_that("configuration reads explicit environment settings", {
  withr::local_envvar(c(
    DATABASE_URL = "postgresql://example.test/rill",
    RILL_ACTOR_ID = "james",
    RILL_ENV = "test",
    DEFUDDLE_BACKEND = "LOCAL",
    DEFUDDLE_COMMAND = "/opt/defuddle/bin/defuddle",
    RILL_AGENT_MODEL = "anthropic/claude-sonnet-4-5-20250929",
    RILL_CAPTURE_TOKEN = "capture-secret",
    RILL_REFRESH_ON_START = "yes",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = "false"
  ))

  config <- rill_config()

  testthat::expect_identical(config$actor_id, "james")
  testthat::expect_identical(config$app_env, "test")
  testthat::expect_identical(config$demo_mode, FALSE)
  testthat::expect_identical(config$defuddle_backend, "local")
  testthat::expect_identical(
    config$defuddle_command,
    "/opt/defuddle/bin/defuddle"
  )
  testthat::expect_identical(
    config$agent_model,
    "anthropic/claude-sonnet-4-5-20250929"
  )
  testthat::expect_identical(config$capture_token, "capture-secret")
  testthat::expect_identical(config$refresh_on_start, TRUE)
})

testthat::test_that("a blank Agent model uses the default", {
  withr::local_envvar(c(
    RILL_AGENT_MODEL = "   ",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))

  testthat::expect_identical(rill_config()$agent_model, "openai")
})

testthat::test_that("configuration rejects content-bearing AI telemetry", {
  withr::local_envvar(
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = "true"
  )

  testthat::expect_error(
    rill_config(),
    class = "rill_unsafe_telemetry_config"
  )
})

testthat::test_that("configuration rejects an unknown Defuddle backend", {
  withr::local_envvar(DEFUDDLE_BACKEND = "browser")

  testthat::expect_snapshot(rill_config(), error = TRUE)
})

testthat::test_that("the user agent uses package metadata", {
  testthat::expect_match(rill_user_agent(), "^rill/[0-9]+\\.[0-9]+")
})
