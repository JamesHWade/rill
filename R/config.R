`%||%` <- function(x, y) {
  missing <- is.null(x) || length(x) == 0L || all(is.na(x))
  blank <- is.character(x) && length(x) == 1L && !is.na(x) && !nzchar(x)
  if (missing || blank) y else x
}

env_flag <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, unset = as.character(default)))
  value %in% c("1", "true", "yes", "on")
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

  list(
    app_name = "Rill",
    app_env = Sys.getenv("RILL_ENV", unset = "development"),
    actor_id = Sys.getenv("RILL_ACTOR_ID", unset = "reader"),
    database_url = database_url,
    demo_mode = identical(database_url, ""),
    defuddle_backend = normalize_defuddle_backend(Sys.getenv(
      "DEFUDDLE_BACKEND",
      unset = "hosted"
    )),
    defuddle_command = defuddle_command,
    agent_model = agent_model,
    defuddle_api_key = trimws(Sys.getenv("DEFUDDLE_API_KEY", unset = "")),
    defuddle_base_url = Sys.getenv(
      "DEFUDDLE_BASE_URL",
      unset = "https://defuddle.md"
    ),
    capture_token = Sys.getenv("RILL_CAPTURE_TOKEN", unset = ""),
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
