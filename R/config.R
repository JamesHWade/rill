`%||%` <- function(x, y) {
  missing <- is.null(x) || length(x) == 0L || all(is.na(x))
  blank <- is.character(x) && length(x) == 1L && !is.na(x) && !nzchar(x)
  if (missing || blank) y else x
}

env_flag <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, unset = as.character(default)))
  value %in% c("1", "true", "yes", "on")
}

env_positive_integer <- function(name, default) {
  value <- Sys.getenv(name, unset = as.character(default))
  parsed <- suppressWarnings(as.numeric(value))
  if (
    length(parsed) != 1L ||
      is.na(parsed) ||
      !is.finite(parsed) ||
      parsed < 1 ||
      parsed != floor(parsed) ||
      parsed > .Machine$integer.max
  ) {
    cli::cli_abort(
      "{.envvar {name}} must be a positive whole number.",
      class = "rill_config_invalid"
    )
  }
  as.integer(parsed)
}

normalize_defuddle_backend <- function(value) {
  value <- tolower(trimws(value %||% "hosted"))
  if (length(value) != 1L || is.na(value) || !value %in% c("hosted", "local")) {
    cli::cli_abort(c(
      "Invalid Defuddle backend {.val {value}}.",
      "i" = "Set {.envvar DEFUDDLE_BACKEND} to {.val hosted} or {.val local}."
    ))
  }
  value
}

normalize_identity_mode <- function(value) {
  value <- tolower(trimws(value %||% "local"))
  if (
    length(value) != 1L ||
      is.na(value) ||
      !value %in% c("local", "oidc_proxy")
  ) {
    cli::cli_abort(
      c(
        "Invalid Reader identity mode {.val {value}}.",
        "i" = paste(
          "Set {.envvar RILL_IDENTITY_MODE} to {.val local} or",
          "{.val oidc_proxy}."
        )
      ),
      class = "rill_identity_config_invalid"
    )
  }
  value
}

parse_oidc_subjects <- function(value) {
  values <- trimws(strsplit(value %||% "", ",", fixed = TRUE)[[1L]])
  unique(values[nzchar(values)])
}

normalize_oidc_issuer <- function(value) {
  value <- trimws(value %||% "")
  if (!nzchar(value)) {
    return("")
  }
  parsed <- tryCatch(
    httr2::url_parse(value),
    error = \(error) NULL
  )
  if (
    is.null(parsed) ||
      !identical(tolower(parsed$scheme %||% ""), "https") ||
      !nzchar(parsed$hostname %||% "") ||
      nzchar(parsed$username %||% "") ||
      nzchar(parsed$password %||% "") ||
      length(parsed$query %||% list()) ||
      nzchar(parsed$fragment %||% "")
  ) {
    cli::cli_abort(
      paste(
        "{.envvar OAUTH2_PROXY_OIDC_ISSUER_URL} must be a complete HTTPS URL",
        "without credentials, a query, or a fragment."
      ),
      class = "rill_identity_config_invalid"
    )
  }
  httr2::url_build(parsed)
}

normalize_oidc_logout_redirect_url <- function(value) {
  value <- trimws(value %||% "")
  if (!nzchar(value)) {
    return("")
  }
  parsed <- tryCatch(
    httr2::url_parse(value),
    error = \(error) NULL
  )
  scheme <- tolower(parsed$scheme %||% "")
  hostname <- tolower(parsed$hostname %||% "")
  local_http <- identical(scheme, "http") &&
    hostname %in% c("127.0.0.1", "localhost", "::1")
  if (
    is.null(parsed) ||
      (!identical(scheme, "https") && !local_http) ||
      !nzchar(hostname) ||
      nzchar(parsed$username %||% "") ||
      nzchar(parsed$password %||% "") ||
      length(parsed$query %||% list()) ||
      nzchar(parsed$fragment %||% "")
  ) {
    cli::cli_abort(
      paste(
        "{.envvar OAUTH2_PROXY_REDIRECT_URL} must be a complete HTTPS URL",
        "without credentials, a query, or a fragment. Loopback HTTP is",
        "permitted for local development."
      ),
      class = "rill_identity_config_invalid"
    )
  }
  parsed$path <- "/"
  httr2::url_build(parsed)
}

rill_config <- function() {
  if (env_flag("OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT", FALSE)) {
    cli::cli_abort(
      c(
        "Rill does not permit content-bearing generative AI telemetry.",
        "i" = paste(
          "Unset {.envvar OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT}",
          "or set it to {.val false}."
        )
      ),
      class = "rill_unsafe_telemetry_config"
    )
  }

  database_url <- trimws(Sys.getenv("DATABASE_URL", unset = ""))
  defuddle_command <- trimws(Sys.getenv(
    "DEFUDDLE_COMMAND",
    unset = "defuddle"
  ))
  if (!nzchar(defuddle_command)) {
    defuddle_command <- "defuddle"
  }
  agent_model <- trimws(Sys.getenv("RILL_AGENT_MODEL", unset = "openai"))
  if (!nzchar(agent_model)) {
    agent_model <- "openai"
  }
  agent_provider <- rill_agent_provider(agent_model)
  agent_base_url <- trimws(Sys.getenv("RILL_AGENT_BASE_URL", unset = ""))
  if (!nzchar(agent_base_url)) {
    agent_base_url <- switch(
      agent_provider,
      openai = trimws(Sys.getenv("OPENAI_BASE_URL", unset = "")),
      azure_openai = trimws(Sys.getenv("AZURE_OPENAI_ENDPOINT", unset = "")),
      ollama = trimws(Sys.getenv("OLLAMA_BASE_URL", unset = "")),
      ""
    )
  }
  if (nzchar(agent_base_url)) {
    agent_base_url <- rill_agent_base_url(agent_model, agent_base_url)
  }
  agent_policy_url <- trimws(Sys.getenv(
    "RILL_AGENT_POLICY_URL",
    unset = ""
  ))
  if (nzchar(agent_policy_url)) {
    agent_policy_url <- rill_agent_http_url(
      agent_policy_url,
      "RILL_AGENT_POLICY_URL"
    )
  }
  identity_mode <- normalize_identity_mode(Sys.getenv(
    "RILL_IDENTITY_MODE",
    unset = "local"
  ))
  oidc_issuer <- normalize_oidc_issuer(Sys.getenv(
    "OAUTH2_PROXY_OIDC_ISSUER_URL",
    unset = ""
  ))
  oidc_client_id <- trimws(Sys.getenv(
    "OAUTH2_PROXY_CLIENT_ID",
    unset = ""
  ))
  oidc_logout_redirect_url <- normalize_oidc_logout_redirect_url(Sys.getenv(
    "OAUTH2_PROXY_REDIRECT_URL",
    unset = ""
  ))
  allowed_oidc_subjects <- parse_oidc_subjects(Sys.getenv(
    "RILL_ALLOWED_OIDC_SUBJECTS",
    unset = ""
  ))
  actor_id <- Sys.getenv("RILL_ACTOR_ID", unset = "reader")
  if (
    identical(identity_mode, "oidc_proxy") &&
      (!nzchar(trimws(actor_id)) ||
        !nzchar(oidc_issuer) ||
        !nzchar(oidc_client_id) ||
        !nzchar(oidc_logout_redirect_url) ||
        !length(allowed_oidc_subjects))
  ) {
    cli::cli_abort(
      c(
        "The OIDC proxy identity gate is incomplete.",
        "i" = paste(
          "Set {.envvar RILL_ACTOR_ID},",
          "{.envvar OAUTH2_PROXY_CLIENT_ID},",
          "{.envvar OAUTH2_PROXY_OIDC_ISSUER_URL},",
          "{.envvar OAUTH2_PROXY_REDIRECT_URL}, and",
          "{.envvar RILL_ALLOWED_OIDC_SUBJECTS}."
        )
      ),
      class = "rill_identity_config_invalid"
    )
  }
  if (identical(identity_mode, "oidc_proxy")) {
    actor_id <- trimws(actor_id)
  }

  list(
    app_name = "Rill",
    app_env = Sys.getenv("RILL_ENV", unset = "development"),
    actor_id = actor_id,
    identity_mode = identity_mode,
    oidc_client_id = oidc_client_id,
    oidc_issuer = oidc_issuer,
    oidc_logout_redirect_url = oidc_logout_redirect_url,
    allowed_oidc_subjects = allowed_oidc_subjects,
    database_url = database_url,
    demo_mode = identical(database_url, ""),
    defuddle_backend = normalize_defuddle_backend(Sys.getenv(
      "DEFUDDLE_BACKEND",
      unset = "hosted"
    )),
    defuddle_command = defuddle_command,
    agent_model = agent_model,
    agent_base_url = agent_base_url,
    agent_policy_url = agent_policy_url,
    defuddle_api_key = trimws(Sys.getenv("DEFUDDLE_API_KEY", unset = "")),
    defuddle_base_url = Sys.getenv(
      "DEFUDDLE_BASE_URL",
      unset = "https://defuddle.md"
    ),
    capture_token = Sys.getenv("RILL_CAPTURE_TOKEN", unset = ""),
    poll_interval_minutes = env_positive_integer(
      "RILL_POLL_INTERVAL_MINUTES",
      60L
    ),
    poll_failure_threshold = env_positive_integer(
      "RILL_POLL_FAILURE_THRESHOLD",
      5L
    ),
    orientation_enabled = env_flag("RILL_ORIENTATION_ENABLED", FALSE),
    telemetry_enabled = nzchar(Sys.getenv(
      "OTEL_EXPORTER_OTLP_ENDPOINT",
      unset = ""
    )),
    refresh_on_start = env_flag("RILL_REFRESH_ON_START", FALSE)
  )
}

rill_id <- function(...) {
  digest::digest(paste(..., sep = "\u241f"), algo = "sha256", serialize = FALSE)
}

rill_package_file <- function(...) {
  system.file(..., package = "rill", mustWork = TRUE)
}

rill_user_agent <- function() {
  version <- as.character(utils::packageVersion("rill"))
  paste0("rill/", version, " (+personal feed reader)")
}

utc_now <- function() {
  format(Sys.time(), tz = "UTC", usetz = TRUE)
}
