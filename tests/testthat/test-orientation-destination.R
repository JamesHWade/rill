expect_orientation_destination_contract <- function(store, reader_id) {
  config <- orientation_destination_test_config()
  initial <- orientation_destination_state(store, reader_id, config)

  testthat::expect_identical(initial$available, TRUE)
  testthat::expect_identical(initial$enabled, FALSE)
  testthat::expect_identical(initial$confirmed, FALSE)
  testthat::expect_identical(initial$needs_confirmation, TRUE)
  testthat::expect_identical(initial$destination$provider_id, "openai")
  testthat::expect_identical(
    initial$destination$endpoint,
    "https://api.openai.com/v1"
  )
  testthat::expect_identical(initial$destination$kind, "external")

  confirmed_at <- as.POSIXct("2026-09-02 16:00:00", tz = "UTC")
  confirmed <- confirm_orientation_destination(
    store,
    reader_id,
    config,
    confirmed_at = confirmed_at
  )
  testthat::expect_identical(confirmed$enabled, TRUE)
  testthat::expect_identical(confirmed$confirmed, TRUE)
  testthat::expect_identical(confirmed$confirmed_at, confirmed_at)

  disabled <- set_orientation_enabled(
    store,
    reader_id,
    enabled = FALSE,
    config = config
  )
  testthat::expect_identical(disabled$enabled, FALSE)
  testthat::expect_identical(disabled$confirmed, TRUE)
  testthat::expect_identical(disabled$confirmed_at, confirmed_at)

  enabled <- set_orientation_enabled(
    store,
    reader_id,
    enabled = TRUE,
    config = config
  )
  testthat::expect_identical(enabled$enabled, TRUE)
  testthat::expect_identical(enabled$confirmed_at, confirmed_at)

  changed <- orientation_destination_state(
    store,
    reader_id,
    orientation_destination_test_config("anthropic/claude-test")
  )
  testthat::expect_identical(changed$enabled, FALSE)
  testthat::expect_identical(changed$confirmed, FALSE)
  testthat::expect_identical(changed$needs_confirmation, TRUE)

  upgraded <- orientation_destination_state(
    store,
    reader_id,
    orientation_destination_test_config("openai/gpt-newer")
  )
  testthat::expect_identical(upgraded$enabled, TRUE)
  testthat::expect_identical(upgraded$confirmed, TRUE)

  other <- orientation_destination_state(store, "other-reader", config)
  testthat::expect_identical(other$enabled, FALSE)
  testthat::expect_identical(other$confirmed, FALSE)
}

testthat::test_that("memory persists Orientation destination consent", {
  store <- local_orientation_backend_store("memory", "reader-destination")

  expect_orientation_destination_contract(store, "reader-destination")
})

testthat::test_that("PostgreSQL persists Orientation destination consent", {
  store <- local_orientation_backend_store("postgres", "reader-destination")

  expect_orientation_destination_contract(store, "reader-destination")
})

testthat::test_that("installation-local Orientation needs no external consent", {
  store <- local_orientation_backend_store("memory", "reader-local")
  config <- orientation_destination_test_config("ollama/llama3.3")

  enabled <- set_orientation_enabled(
    store,
    "reader-local",
    enabled = TRUE,
    config = config
  )

  testthat::expect_identical(enabled$enabled, TRUE)
  testthat::expect_identical(enabled$confirmed, TRUE)
  testthat::expect_identical(enabled$needs_confirmation, FALSE)
  testthat::expect_identical(enabled$destination$kind, "installation")
  testthat::expect_null(enabled$confirmed_at)
})

testthat::test_that("external Orientation cannot bypass confirmation", {
  store <- local_orientation_backend_store("memory", "reader-external")
  config <- orientation_destination_test_config()

  testthat::expect_error(
    set_orientation_enabled(
      store,
      "reader-external",
      enabled = TRUE,
      config = config
    ),
    class = "rill_orientation_confirmation_required"
  )
})

testthat::test_that("destination endpoint changes require confirmation", {
  store <- local_orientation_backend_store("memory", "reader-endpoint")
  config <- orientation_destination_test_config(
    base_url = "https://gateway-one.example/v1"
  )
  confirmed <- confirm_orientation_destination(
    store,
    "reader-endpoint",
    config
  )

  changed <- orientation_destination_state(
    store,
    "reader-endpoint",
    orientation_destination_test_config(
      base_url = "https://gateway-two.example/v1"
    )
  )

  testthat::expect_identical(confirmed$enabled, TRUE)
  testthat::expect_identical(changed$enabled, FALSE)
  testthat::expect_identical(changed$confirmed, FALSE)
  testthat::expect_identical(changed$needs_confirmation, TRUE)
})

testthat::test_that("memory consent is bound to normalized provider terms", {
  store <- local_orientation_backend_store("memory", "reader-policy-change")

  expect_orientation_policy_consent_contract(store, "reader-policy-change")
})

testthat::test_that("PostgreSQL consent is bound to normalized provider terms", {
  store <- local_orientation_backend_store("postgres", "reader-policy-change")

  expect_orientation_policy_consent_contract(store, "reader-policy-change")
})

testthat::test_that("remote Ollama is an external Data Destination", {
  store <- local_orientation_backend_store("memory", "reader-remote-ollama")
  config <- orientation_destination_test_config(
    model = "ollama/llama3.3",
    base_url = "http://ollama.example:11434"
  )
  state <- orientation_destination_state(store, "reader-remote-ollama", config)

  testthat::expect_identical(state$destination$kind, "external")
  testthat::expect_identical(state$needs_confirmation, TRUE)
  testthat::expect_identical(state$enabled, FALSE)
})

testthat::test_that("unresolved provider-native endpoints fail closed", {
  store <- local_orientation_backend_store("memory", "reader-aws")
  config <- orientation_destination_test_config(
    model = "aws_bedrock/anthropic.claude-test"
  )

  state <- orientation_destination_state(store, "reader-aws", config)

  testthat::expect_identical(state$endpoint_ready, FALSE)
  testthat::expect_identical(state$needs_endpoint_configuration, TRUE)
  testthat::expect_identical(state$enabled, FALSE)
  testthat::expect_identical(state$needs_confirmation, FALSE)
  testthat::expect_error(
    confirm_orientation_destination(store, "reader-aws", config),
    class = "rill_orientation_endpoint_required"
  )
})

testthat::test_that("external Orientation requires inspectable provider terms", {
  store <- local_orientation_backend_store("memory", "reader-policy")
  config <- orientation_destination_test_config(policy_url = "")
  state <- orientation_destination_state(store, "reader-policy", config)

  testthat::expect_identical(state$policy_ready, FALSE)
  testthat::expect_identical(state$needs_configuration, TRUE)
  testthat::expect_identical(state$needs_confirmation, FALSE)
  testthat::expect_error(
    confirm_orientation_destination(store, "reader-policy", config),
    class = "rill_orientation_policy_required"
  )
})
