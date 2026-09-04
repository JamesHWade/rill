supported_r_version <- "4.6.0"

rsconnect::writeManifest(
  appDir = ".",
  appPrimaryDoc = "app.R",
  appMode = "shiny",
  dependencyResolution = "library",
  quiet = FALSE
)

manifest <- readLines("manifest.json", warn = FALSE)
platform_line <- grep('^  "platform": ', manifest)
if (length(platform_line) != 1L) {
  cli::cli_abort("The generated manifest has no single R platform field.")
}
manifest[[platform_line]] <- paste0(
  '  "platform": "',
  supported_r_version,
  '",'
)
writeLines(manifest, "manifest.json", useBytes = TRUE)

cli::cli_alert_success(
  "Wrote {.file manifest.json} for R {supported_r_version}."
)
