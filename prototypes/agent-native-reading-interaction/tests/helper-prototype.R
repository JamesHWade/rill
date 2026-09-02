prototype_dir <- normalizePath(
  Sys.getenv("RILL_PROTOTYPE_TEST_DIR"),
  mustWork = TRUE
)
package_root <- normalizePath(
  file.path(prototype_dir, "..", ".."),
  mustWork = TRUE
)

Sys.setenv(
  DATABASE_URL = "",
  OTEL_EXPORTER_OTLP_ENDPOINT = "",
  OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT = "false",
  RILL_AGENT_MODEL = "openai",
  RILL_CAPTURE_TOKEN = "",
  RILL_REFRESH_ON_START = "false"
)
pkgload::load_all(
  package_root,
  export_all = FALSE,
  helpers = FALSE,
  quiet = TRUE
)

prototype_env <- new.env(parent = asNamespace("rill"))
options(
  rill.prototype.root = prototype_dir,
  rill.prototype.data_destination = "OpenAI"
)
sys.source(
  file.path(prototype_dir, "prototype.R"),
  envir = prototype_env
)

prototype_config <- get("rill_config", envir = asNamespace("rill"))()
prototype_rendered <- htmltools::renderTags(
  prototype_env$prototype_ui(prototype_config)
)
prototype_head <- prototype_rendered$head
prototype_html <- prototype_rendered$html

fixed_count <- function(text, pattern) {
  length(strsplit(text, pattern, fixed = TRUE)[[1]]) - 1L
}
