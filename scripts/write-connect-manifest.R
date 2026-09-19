supported_r_version <- "4.6.0"

rsconnect::writeManifest(
  appDir = ".",
  appPrimaryDoc = "app.R",
  appMode = "shiny",
  dependencyResolution = "library",
  quiet = FALSE
)

manifest <- jsonlite::read_json("manifest.json")
manifest$platform <- supported_r_version
# writeManifest() temporarily changes .Rbuildignore while resolving the bundle.
# Its returned inventory must describe the files left in the checkout.
for (path in names(manifest$files)) {
  if (!file.exists(path)) {
    cli::cli_abort("The manifest references a missing file: {.file {path}}.")
  }
  manifest$files[[path]]$checksum <- unname(tools::md5sum(path))
}
jsonlite::write_json(
  manifest,
  "manifest.json",
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)

cli::cli_alert_success(
  "Wrote {.file manifest.json} for R {supported_r_version}."
)
