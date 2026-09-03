testthat::test_that("configuration defaults to the bundled demo", {
  withr::local_envvar(c(
    DATABASE_URL = "",
    RILL_ACTOR_ID = NA,
    RILL_IDENTITY_MODE = NA,
    RILL_ALLOWED_OIDC_SUBJECTS = NA,
    OAUTH2_PROXY_CLIENT_ID = NA,
    OAUTH2_PROXY_OIDC_ISSUER_URL = NA,
    OAUTH2_PROXY_REDIRECT_URL = NA,
    RILL_ENV = NA,
    DEFUDDLE_BACKEND = NA,
    DEFUDDLE_COMMAND = NA,
    RILL_AGENT_MODEL = NA,
    RILL_AGENT_BASE_URL = NA,
    OPENAI_BASE_URL = NA,
    OLLAMA_BASE_URL = NA,
    RILL_AGENT_POLICY_URL = NA,
    RILL_CAPTURE_TOKEN = NA,
    RILL_POLL_INTERVAL_MINUTES = NA,
    RILL_POLL_FAILURE_THRESHOLD = NA,
    RILL_ORIENTATION_ENABLED = NA,
    RILL_REFRESH_ON_START = NA,
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))

  config <- rill_config()

  testthat::expect_identical(config$app_name, "Rill")
  testthat::expect_identical(config$app_env, "development")
  testthat::expect_identical(config$actor_id, "reader")
  testthat::expect_identical(config$identity_mode, "local")
  testthat::expect_identical(config$oidc_client_id, "")
  testthat::expect_identical(config$oidc_issuer, "")
  testthat::expect_identical(config$oidc_logout_redirect_url, "")
  testthat::expect_identical(config$allowed_oidc_subjects, character())
  testthat::expect_identical(config$demo_mode, TRUE)
  testthat::expect_identical(config$defuddle_backend, "hosted")
  testthat::expect_identical(config$defuddle_command, "defuddle")
  testthat::expect_identical(config$agent_model, "openai")
  testthat::expect_identical(config$agent_base_url, "")
  testthat::expect_identical(config$agent_policy_url, "")
  testthat::expect_identical(config$capture_token, "")
  testthat::expect_identical(config$poll_interval_minutes, 60L)
  testthat::expect_identical(config$poll_failure_threshold, 5L)
  testthat::expect_identical(config$orientation_enabled, FALSE)
  testthat::expect_identical(config$refresh_on_start, FALSE)
})

testthat::test_that("configuration reads explicit environment settings", {
  withr::local_envvar(c(
    DATABASE_URL = "postgresql://example.test/rill",
    RILL_ACTOR_ID = "james",
    RILL_IDENTITY_MODE = "oidc_proxy",
    RILL_ALLOWED_OIDC_SUBJECTS = " google-oauth2|reader,github|reader ",
    OAUTH2_PROXY_CLIENT_ID = "private-reader-client",
    OAUTH2_PROXY_OIDC_ISSUER_URL = "https://reader.us.auth0.com/",
    OAUTH2_PROXY_REDIRECT_URL = "https://reader.example/oauth2/callback",
    RILL_ENV = "test",
    DEFUDDLE_BACKEND = "LOCAL",
    DEFUDDLE_COMMAND = "/opt/defuddle/bin/defuddle",
    RILL_AGENT_MODEL = "anthropic/claude-sonnet-4-5-20250929",
    RILL_AGENT_BASE_URL = "https://gateway.example/v1/",
    RILL_AGENT_POLICY_URL = "https://provider.example/terms",
    RILL_CAPTURE_TOKEN = "capture-secret",
    RILL_POLL_INTERVAL_MINUTES = "15",
    RILL_POLL_FAILURE_THRESHOLD = "3",
    RILL_ORIENTATION_ENABLED = "true",
    RILL_REFRESH_ON_START = "yes",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = "false"
  ))

  config <- rill_config()

  testthat::expect_identical(config$actor_id, "james")
  testthat::expect_identical(config$identity_mode, "oidc_proxy")
  testthat::expect_identical(config$oidc_client_id, "private-reader-client")
  testthat::expect_identical(
    config$allowed_oidc_subjects,
    c("google-oauth2|reader", "github|reader")
  )
  testthat::expect_identical(
    config$oidc_issuer,
    "https://reader.us.auth0.com/"
  )
  testthat::expect_identical(
    config$oidc_logout_redirect_url,
    "https://reader.example/"
  )
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
  testthat::expect_identical(
    config$agent_base_url,
    "https://gateway.example/v1"
  )
  testthat::expect_identical(
    config$agent_policy_url,
    "https://provider.example/terms"
  )
  testthat::expect_identical(config$capture_token, "capture-secret")
  testthat::expect_identical(config$poll_interval_minutes, 15L)
  testthat::expect_identical(config$poll_failure_threshold, 3L)
  testthat::expect_identical(config$orientation_enabled, TRUE)
  testthat::expect_identical(config$refresh_on_start, TRUE)
})

testthat::test_that("polling configuration requires positive whole numbers", {
  withr::local_envvar(c(
    RILL_POLL_INTERVAL_MINUTES = "0",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))
  testthat::expect_error(rill_config(), class = "rill_config_invalid")

  withr::local_envvar(c(
    RILL_POLL_INTERVAL_MINUTES = "15",
    RILL_POLL_FAILURE_THRESHOLD = "1.5",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))
  testthat::expect_error(rill_config(), class = "rill_config_invalid")
})

testthat::test_that("the proxy gate requires a safe HTTPS issuer", {
  withr::local_envvar(c(
    RILL_IDENTITY_MODE = "oidc_proxy",
    RILL_ALLOWED_OIDC_SUBJECTS = "auth0|reader",
    OAUTH2_PROXY_OIDC_ISSUER_URL = "http://reader.us.auth0.com/",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))

  testthat::expect_error(
    rill_config(),
    class = "rill_identity_config_invalid"
  )
})

testthat::test_that("the private gate requires a stable Reader identifier", {
  withr::local_envvar(c(
    RILL_ACTOR_ID = "   ",
    RILL_IDENTITY_MODE = "oidc_proxy",
    RILL_ALLOWED_OIDC_SUBJECTS = "auth0|reader",
    OAUTH2_PROXY_OIDC_ISSUER_URL = "https://reader.us.auth0.com/",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))

  testthat::expect_error(
    rill_config(),
    class = "rill_identity_config_invalid"
  )
})

testthat::test_that("the proxy gate requires an explicit subject allowlist", {
  withr::local_envvar(c(
    RILL_IDENTITY_MODE = "oidc_proxy",
    RILL_ALLOWED_OIDC_SUBJECTS = "  ",
    OAUTH2_PROXY_OIDC_ISSUER_URL = "https://reader.us.auth0.com/",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))

  testthat::expect_error(
    rill_config(),
    class = "rill_identity_config_invalid"
  )
})

testthat::test_that("the proxy gate requires complete logout configuration", {
  withr::local_envvar(c(
    RILL_IDENTITY_MODE = "oidc_proxy",
    RILL_ALLOWED_OIDC_SUBJECTS = "auth0|reader",
    OAUTH2_PROXY_CLIENT_ID = "",
    OAUTH2_PROXY_OIDC_ISSUER_URL = "https://reader.us.auth0.com/",
    OAUTH2_PROXY_REDIRECT_URL = "https://reader.example/oauth2/callback",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))

  testthat::expect_error(
    rill_config(),
    class = "rill_identity_config_invalid"
  )
})

testthat::test_that("configuration rejects an unknown identity mode", {
  withr::local_envvar(c(
    RILL_IDENTITY_MODE = "auth0",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))

  testthat::expect_error(
    rill_config(),
    class = "rill_identity_config_invalid"
  )
})

testthat::test_that("a blank Agent model uses the default", {
  withr::local_envvar(c(
    RILL_AGENT_MODEL = "   ",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))

  testthat::expect_identical(rill_config()$agent_model, "openai")
})

testthat::test_that("configuration reads provider endpoint overrides", {
  withr::local_envvar(c(
    RILL_AGENT_MODEL = "ollama/llama3.3",
    RILL_AGENT_BASE_URL = NA,
    OLLAMA_BASE_URL = "http://model-host.example:11434/",
    RILL_AGENT_POLICY_URL = "https://model-host.example/privacy",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))

  testthat::expect_identical(
    rill_config()$agent_base_url,
    "http://model-host.example:11434"
  )

  withr::local_envvar(c(
    RILL_AGENT_MODEL = "azure_openai/gpt-5",
    RILL_AGENT_BASE_URL = NA,
    AZURE_OPENAI_ENDPOINT = "https://reader.openai.azure.com/",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))
  testthat::expect_identical(
    rill_config()$agent_base_url,
    "https://reader.openai.azure.com"
  )
})

testthat::test_that("Azure consent identity follows its provider endpoint", {
  destination_id <- function(endpoint) {
    withr::with_envvar(
      c(
        RILL_AGENT_MODEL = "azure_openai/gpt-5",
        RILL_AGENT_BASE_URL = NA,
        AZURE_OPENAI_ENDPOINT = endpoint,
        RILL_AGENT_POLICY_URL = "https://provider.example/privacy",
        OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
      ),
      {
        config <- rill_config()
        rill_agent_data_destination_details(
          config$agent_model,
          base_url = config$agent_base_url,
          policy_url = config$agent_policy_url
        )$id
      }
    )
  }

  first <- destination_id("https://first.openai.azure.com")
  second <- destination_id("https://second.openai.azure.com")

  testthat::expect_identical(identical(first, second), FALSE)
})

testthat::test_that("configuration rejects unsafe Agent URLs", {
  withr::local_envvar(c(
    RILL_AGENT_POLICY_URL = "javascript:alert(1)",
    RILL_AGENT_BASE_URL = NA,
    OPENAI_BASE_URL = NA,
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))
  testthat::expect_error(rill_config(), class = "rill_agent_url_invalid")

  withr::local_envvar(c(
    RILL_AGENT_POLICY_URL = "https://reader:secret@provider.example/privacy",
    RILL_AGENT_BASE_URL = NA,
    OPENAI_BASE_URL = NA,
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))
  testthat::expect_error(rill_config(), class = "rill_agent_url_invalid")

  withr::local_envvar(c(
    RILL_AGENT_POLICY_URL = "https://provider.example/privacy",
    RILL_AGENT_BASE_URL = "https://secret@example.com/v1",
    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = NA
  ))
  testthat::expect_error(rill_config(), class = "rill_agent_url_invalid")
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
