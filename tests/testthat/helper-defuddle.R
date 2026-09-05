preparation_test_store <- function() {
  store <- rill_store(list(demo_mode = TRUE))
  store$memory$documents <- list()
  store$memory$document_heads <- character()
  store$memory$entries$published_at <- format(
    as.POSIXct("2026-08-19 12:00:00", tz = "UTC") -
      c(60, rep(60 * 60 * 24 * 40, 5)),
    tz = "UTC",
    usetz = TRUE
  )
  store
}
