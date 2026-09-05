local_article_preparation_worker <- function(.env = parent.frame()) {
  testthat::local_mocked_bindings(
    launch_article_preparation = function(
      entry,
      config,
      directory,
      package_path
    ) {
      list(is_alive = \() FALSE, wait = \(timeout) NULL, get_result = \() {
        extract_preparation(entry, config)
      })
    },
    .env = .env
  )
}

flush_article_preparation <- function(controller) {
  for (index in seq_len(100L)) {
    if (
      is.null(controller$state$job) &&
        !length(controller$state$queue) &&
        !controller$state$draining
    ) {
      return(invisible(NULL))
    }
    controller$poll()
  }
  stop("Article preparation did not finish")
}

expire_article_preparation <- function(store) {
  for (id in names(store$memory$article_preparations)) {
    store$memory$article_preparations[[id]]$next_attempt_at <- Sys.time() - 1
  }
}
