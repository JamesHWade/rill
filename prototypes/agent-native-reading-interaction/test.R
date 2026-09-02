#!/usr/bin/env Rscript

script_argument <- grep(
  "^--file=",
  commandArgs(trailingOnly = FALSE),
  value = TRUE
)
if (length(script_argument) != 1L) {
  stop("Run this test launcher with Rscript.", call. = FALSE)
}

prototype_dir <- dirname(normalizePath(
  sub("^--file=", "", script_argument),
  mustWork = TRUE
))
Sys.setenv(RILL_PROTOTYPE_TEST_DIR = prototype_dir)

testthat::test_dir(
  file.path(prototype_dir, "tests"),
  reporter = "summary",
  stop_on_failure = TRUE,
  stop_on_warning = TRUE,
  load_package = "none"
)
