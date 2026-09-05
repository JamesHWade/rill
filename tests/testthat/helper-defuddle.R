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

preparation_test_agent_run <- function(store, document, status = "completed") {
  run <- store_start_agent_run(
    store,
    reader_id = "reader",
    kind = "question",
    request_key = "fallback-question",
    pinned_inputs = list(document_id = document$document_id)
  )
  run <- store_claim_agent_run(
    store,
    reader_id = "reader",
    run_id = run$run_id,
    worker_id = "test",
    lease_expires_at = Sys.time() + 60
  )
  if (identical(status, "running")) {
    return(run)
  }
  store_finish_agent_run(
    store,
    reader_id = "reader",
    run_id = run$run_id,
    worker_id = "test",
    status = status
  )
}
