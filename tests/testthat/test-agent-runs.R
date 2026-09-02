testthat::test_that("starting the same Agent Run request is idempotent", {
  store <- rill_store(list(demo_mode = TRUE))
  requested_at <- as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  pinned_inputs <- list(
    document_ids = c("document-1", "document-2"),
    policy_version = "v1"
  )

  first <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = "orientation-library-42",
    pinned_inputs = pinned_inputs,
    requested_at = requested_at
  )
  replay <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = "orientation-library-42",
    pinned_inputs = pinned_inputs,
    requested_at = requested_at + 60
  )

  testthat::expect_identical(replay, first)
  testthat::expect_identical(first$status, "pending")
  testthat::expect_identical(first$pinned_inputs, pinned_inputs)
  testthat::expect_identical(
    store_get_agent_run(store, "reader-1", first$run_id),
    first
  )
  testthat::expect_error(
    store_start_agent_run(
      store,
      reader_id = "reader-1",
      kind = "orientation",
      request_key = "orientation-library-42",
      pinned_inputs = list(document_ids = "different-document")
    ),
    class = "rill_agent_run_replay_conflict"
  )
})

testthat::test_that("a Reader has at most one active Agent Run", {
  store <- rill_store(list(demo_mode = TRUE))
  store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = "orientation-library-42",
    pinned_inputs = list(document_ids = "document-1")
  )

  testthat::expect_error(
    store_start_agent_run(
      store,
      reader_id = "reader-1",
      kind = "conversation",
      request_key = "conversation-message-17",
      pinned_inputs = list(document_ids = "document-2")
    ),
    class = "rill_agent_run_conflict"
  )
})

testthat::test_that("claiming an Agent Run records worker ownership", {
  store <- rill_store(list(demo_mode = TRUE))
  run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = "orientation-library-42",
    pinned_inputs = list(document_ids = "document-1")
  )
  started_at <- as.POSIXct("2026-09-02 12:01:00", tz = "UTC")
  lease_expires_at <- as.POSIXct("2026-09-02 12:02:00", tz = "UTC")

  claimed <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    worker_id = "worker-1",
    started_at = started_at,
    lease_expires_at = lease_expires_at
  )

  testthat::expect_identical(claimed$status, "running")
  testthat::expect_identical(claimed$worker_id, "worker-1")
  testthat::expect_identical(claimed$started_at, started_at)
  testthat::expect_identical(claimed$lease_expires_at, lease_expires_at)
  testthat::expect_identical(
    store_get_agent_run(store, "reader-1", run$run_id),
    claimed
  )
})

testthat::test_that("the lease owner records reconnectable partial output", {
  store <- rill_store(list(demo_mode = TRUE))
  run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "conversation",
    request_key = "conversation-message-17",
    pinned_inputs = list(message_id = "message-17")
  )
  run <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    worker_id = "worker-1",
    lease_expires_at = as.POSIXct("2026-09-02 12:02:00", tz = "UTC")
  )
  updated_at <- as.POSIXct("2026-09-02 12:01:30", tz = "UTC")
  lease_expires_at <- as.POSIXct("2026-09-02 12:02:30", tz = "UTC")

  updated <- store_record_agent_run_partial(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    worker_id = "worker-1",
    partial_response = "A useful beginning",
    updated_at = updated_at,
    lease_expires_at = lease_expires_at
  )

  testthat::expect_identical(updated$partial_response, "A useful beginning")
  testthat::expect_identical(updated$updated_at, updated_at)
  testthat::expect_identical(updated$lease_expires_at, lease_expires_at)
  testthat::expect_identical(
    store_get_agent_run(store, "reader-1", run$run_id),
    updated
  )
})

testthat::test_that("cancellation remains pending until a worker confirms it", {
  store <- rill_store(list(demo_mode = TRUE))
  run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "conversation",
    request_key = "conversation-message-17",
    pinned_inputs = list(message_id = "message-17")
  )
  run <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    worker_id = "worker-1",
    lease_expires_at = as.POSIXct("2026-09-02 12:02:00", tz = "UTC")
  )
  requested_at <- as.POSIXct("2026-09-02 12:01:30", tz = "UTC")

  cancelling <- store_request_agent_run_cancel(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    requested_at = requested_at
  )

  testthat::expect_identical(cancelling$status, "cancelling")
  testthat::expect_identical(cancelling$cancel_requested_at, requested_at)
  testthat::expect_identical(
    store_get_agent_run(store, "reader-1", run$run_id),
    cancelling
  )
})

testthat::test_that("an unclaimed Agent Run cancels immediately", {
  store <- rill_store(list(demo_mode = TRUE))
  run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "conversation",
    request_key = "conversation-message-17",
    pinned_inputs = list(message_id = "message-17")
  )
  requested_at <- as.POSIXct("2026-09-02 12:01:30", tz = "UTC")

  cancelled <- store_request_agent_run_cancel(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    requested_at = requested_at
  )

  testthat::expect_identical(cancelled$status, "cancelled")
  testthat::expect_identical(
    cancelled$terminal_reason,
    "cancelled_before_start"
  )
  testthat::expect_identical(cancelled$terminal_at, requested_at)
  next_run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "conversation",
    request_key = "conversation-message-18",
    pinned_inputs = list(message_id = "message-18")
  )
  testthat::expect_identical(next_run$status, "pending")
})

testthat::test_that("terminal Agent Runs clear partial state and release the Reader", {
  store <- rill_store(list(demo_mode = TRUE))
  run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "conversation",
    request_key = "conversation-message-17",
    pinned_inputs = list(message_id = "message-17")
  )
  run <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    worker_id = "worker-1",
    lease_expires_at = as.POSIXct("2026-09-02 12:02:00", tz = "UTC")
  )
  run <- store_record_agent_run_partial(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    worker_id = "worker-1",
    partial_response = "A useful beginning",
    lease_expires_at = as.POSIXct("2026-09-02 12:02:30", tz = "UTC")
  )
  finished_at <- as.POSIXct("2026-09-02 12:02:00", tz = "UTC")
  usage <- list(requests = 1L, output_tokens = 120L)

  completed <- store_finish_agent_run(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    worker_id = "worker-1",
    status = "completed",
    usage = usage,
    terminal_reason = "complete",
    deputy_run_id = "deputy-run-17",
    finished_at = finished_at
  )

  testthat::expect_identical(completed$status, "completed")
  testthat::expect_identical(completed$terminal_at, finished_at)
  testthat::expect_null(completed$partial_response)
  testthat::expect_null(completed$lease_expires_at)
  testthat::expect_identical(completed$usage, usage)
  testthat::expect_identical(completed$terminal_reason, "complete")
  testthat::expect_identical(completed$deputy_run_id, "deputy-run-17")

  next_run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = "orientation-library-43",
    pinned_inputs = list(document_ids = "document-2")
  )
  testthat::expect_identical(next_run$status, "pending")
})

testthat::test_that("expired Agent Run leases become interrupted", {
  store <- rill_store(list(demo_mode = TRUE))
  run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "conversation",
    request_key = "conversation-message-17",
    pinned_inputs = list(message_id = "message-17")
  )
  run <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    worker_id = "worker-1",
    lease_expires_at = as.POSIXct("2026-09-02 12:02:00", tz = "UTC")
  )
  run <- store_record_agent_run_partial(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    worker_id = "worker-1",
    partial_response = "This output will not survive",
    lease_expires_at = as.POSIXct("2026-09-02 12:02:00", tz = "UTC")
  )
  recovered_at <- as.POSIXct("2026-09-02 12:03:00", tz = "UTC")

  interrupted <- store_interrupt_expired_agent_runs(store, recovered_at)

  testthat::expect_length(interrupted, 1L)
  testthat::expect_identical(interrupted[[1]]$status, "interrupted")
  testthat::expect_identical(interrupted[[1]]$terminal_at, recovered_at)
  testthat::expect_identical(
    interrupted[[1]]$terminal_reason,
    "lease_expired"
  )
  testthat::expect_null(interrupted[[1]]$partial_response)
  testthat::expect_null(interrupted[[1]]$lease_expires_at)
})

testthat::test_that("Retry creates a linked Run with the same pinned inputs", {
  store <- rill_store(list(demo_mode = TRUE))
  pinned_inputs <- list(
    message_id = "message-17",
    document_ids = c("document-1", "document-2")
  )
  original <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "conversation",
    request_key = "conversation-message-17",
    pinned_inputs = pinned_inputs
  )
  original <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = original$run_id,
    worker_id = "worker-1",
    lease_expires_at = as.POSIXct("2026-09-02 12:02:00", tz = "UTC")
  )
  original <- store_finish_agent_run(
    store,
    reader_id = "reader-1",
    run_id = original$run_id,
    worker_id = "worker-1",
    status = "failed"
  )

  retry <- store_retry_agent_run(
    store,
    reader_id = "reader-1",
    run_id = original$run_id,
    request_key = "retry-click-1"
  )
  replay <- store_retry_agent_run(
    store,
    reader_id = "reader-1",
    run_id = original$run_id,
    request_key = "retry-click-1"
  )

  testthat::expect_false(identical(retry$run_id, original$run_id))
  testthat::expect_identical(retry$retry_of_run_id, original$run_id)
  testthat::expect_identical(retry$pinned_inputs, pinned_inputs)
  testthat::expect_identical(retry$kind, original$kind)
  testthat::expect_identical(retry$status, "pending")
  testthat::expect_identical(replay, retry)
})
