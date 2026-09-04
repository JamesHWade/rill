local_feed_refresh_worker <- function(
  delay = 0,
  gate = NULL,
  .env = parent.frame()
) {
  testthat::local_mocked_bindings(
    launch_feed_refresh = function(
      request,
      directory,
      package_path,
      development
    ) {
      callr::r_bg(
        function(request, directory, package_path, development, delay, gate) {
          if (development) {
            pkgload::load_all(
              package_path,
              export_all = FALSE,
              helpers = FALSE,
              quiet = TRUE
            )
          } else {
            loadNamespace("rill", lib.loc = dirname(package_path))
          }
          refresh <- function(store, feed) {
            deadline <- Sys.time() + 30
            while (!is.null(gate) && !file.exists(gate)) {
              if (Sys.time() > deadline) {
                stop("Test worker checkpoint timed out")
              }
              Sys.sleep(0.01)
            }
            Sys.sleep(delay)
            parsed <- get("parse_feed_document", asNamespace("rill"))(
              '<rss version="2.0"><channel><title>Updated source</title><link>https://example.com</link><item><guid>background-entry</guid><title>A new entry</title><link>https://example.com/new</link></item></channel></rss>',
              feed$feed_url
            )
            parsed$feed$feed_id <- feed$feed_id
            parsed$entries$feed_id <- feed$feed_id
            parsed$entries$entry_id <- get("rill_id", asNamespace("rill"))(
              "entry",
              feed$feed_id,
              "background-entry"
            )
            get("store_upsert_feed", asNamespace("rill"))(store, parsed$feed)
            added <- get("store_upsert_entries", asNamespace("rill"))(
              store,
              parsed$entries
            )
            list(added = added, not_modified = FALSE)
          }
          get("run_background_feed_refresh", asNamespace("rill"))(
            request,
            directory,
            refresh
          )
        },
        args = list(request, directory, package_path, development, delay, gate),
        supervise = TRUE,
        user_profile = FALSE,
        stdout = file.path(directory, "stdout"),
        stderr = file.path(directory, "stderr")
      )
    },
    .env = .env
  )
}

wait_for_feed_refresh <- function(job, timeout = 30) {
  deadline <- Sys.time() + timeout
  repeat {
    result <- poll_feed_refresh(job)
    if (!identical(result$status, "running")) {
      return(result)
    }
    if (Sys.time() > deadline) {
      stop("Background refresh did not finish")
    }
    later::run_now(0.05)
  }
}
