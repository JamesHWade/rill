if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}

pak::local_install_dev_deps(upgrade = FALSE, ask = FALSE)

if (!requireNamespace("renv", quietly = TRUE)) {
  pak::pkg_install("renv", upgrade = FALSE, ask = FALSE)
}

if (!file.exists("renv.lock")) {
  renv::init(bare = TRUE)
}
renv::snapshot(prompt = FALSE)

cli::cli_alert_success("Dependencies installed and {.file renv.lock} updated.")
