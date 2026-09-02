#!/usr/bin/env Rscript

script_argument <- grep(
  "^--file=",
  commandArgs(trailingOnly = FALSE),
  value = TRUE
)
if (length(script_argument) != 1L) {
  stop("Run this launcher with Rscript.", call. = FALSE)
}

prototype_dir <- dirname(normalizePath(
  sub("^--file=", "", script_argument),
  mustWork = TRUE
))
package_root <- normalizePath(
  file.path(prototype_dir, "..", ".."),
  mustWork = TRUE
)

pkgload::load_all(
  package_root,
  export_all = FALSE,
  helpers = FALSE,
  quiet = TRUE
)

prototype_env <- new.env(parent = asNamespace("rill"))
sys.source(
  file.path(prototype_dir, "prototype.R"),
  envir = prototype_env
)
prototype_env$run_agent_interaction_prototype(prototype_dir)
