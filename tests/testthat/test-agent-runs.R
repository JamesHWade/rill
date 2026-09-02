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
    pinned_inputs = pinned_inputs[rev(names(pinned_inputs))],
    requested_at = requested_at + 20
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
      pinned_inputs = list(document_ids = "different-document"),
      requested_at = requested_at + 1
    ),
    class = "rill_agent_run_replay_conflict"
  )
})

testthat::test_that("an expired replay does not retain its dead worker", {
  store <- rill_store(list(demo_mode = TRUE))
  requested_at <- as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = "same-orientation",
    pinned_inputs = list(boundary_hash = "boundary-1"),
    requested_at = requested_at,
    worker_id = "dead-worker"
  )

  replay <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = "same-orientation",
    pinned_inputs = list(boundary_hash = "boundary-1"),
    requested_at = requested_at + 31,
    worker_id = "new-worker"
  )

  testthat::expect_identical(replay$run_id, run$run_id)
  testthat::expect_identical(replay$status, "interrupted")
  testthat::expect_identical(replay$terminal_reason, "lease_expired")
})

testthat::test_that("an unclaimed Agent Run expires and releases the Reader", {
  store <- rill_store(list(demo_mode = TRUE))
  requested_at <- as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = "orientation-library-42",
    pinned_inputs = list(document_ids = "document-1"),
    requested_at = requested_at
  )

  testthat::expect_equal(run$lease_expires_at, requested_at + 30)
  testthat::expect_length(
    store_interrupt_expired_agent_runs(store, requested_at + 29),
    0L
  )

  interrupted <- store_interrupt_expired_agent_runs(
    store,
    requested_at + 30
  )

  testthat::expect_length(interrupted, 1L)
  testthat::expect_identical(interrupted[[1L]]$status, "interrupted")
  replacement <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "ask-rill-message-17",
    pinned_inputs = list(message_id = "message-17"),
    requested_at = requested_at + 30
  )
  testthat::expect_identical(replacement$status, "pending")
})

testthat::test_that("starting work recovers an expired pending Run", {
  store <- rill_store(list(demo_mode = TRUE))
  requested_at <- as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  orphan <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = "orphaned-orientation",
    pinned_inputs = list(document_ids = "document-1"),
    requested_at = requested_at
  )

  replacement <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "question-after-orphan",
    pinned_inputs = list(message_id = "message-1"),
    requested_at = requested_at + 31
  )

  testthat::expect_identical(replacement$status, "pending")
  testthat::expect_identical(
    store_get_agent_run(store, "reader-1", orphan$run_id)$status,
    "interrupted"
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
      kind = "question",
      request_key = "ask-rill-message-17",
      pinned_inputs = list(document_ids = "document-2")
    ),
    class = "rill_agent_run_conflict"
  )
})

testthat::test_that("the latest question remains available after interruption", {
  store <- rill_store(list(demo_mode = TRUE))
  requested_at <- as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  question <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "question-before-restart",
    pinned_inputs = list(document_id = "document-1"),
    requested_at = requested_at,
    worker_id = "old-process"
  )
  question <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = question$run_id,
    worker_id = "old-process",
    started_at = requested_at,
    lease_expires_at = requested_at + 300
  )
  store_interrupt_agent_runs(
    store,
    recovery = "process_restart",
    recovered_at = requested_at + 1
  )
  orientation <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = "orientation-after-restart",
    pinned_inputs = list(boundary_hash = "boundary-2"),
    requested_at = requested_at + 2
  )

  latest <- store_get_latest_question_agent_run(store, "reader-1")

  testthat::expect_identical(latest$run_id, question$run_id)
  testthat::expect_identical(latest$status, "interrupted")
  testthat::expect_identical(
    store_get_active_agent_run(store, "reader-1")$run_id,
    orientation$run_id
  )
})

testthat::test_that("an unconfirmed interrupt releases an owned Agent Run", {
  store <- rill_store(list(demo_mode = TRUE))
  requested_at <- as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "question-with-lost-controller",
    pinned_inputs = list(document_id = "document-1"),
    requested_at = requested_at,
    worker_id = "worker-1"
  )
  run <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    worker_id = "worker-1",
    started_at = requested_at,
    lease_expires_at = requested_at + 300
  )
  interrupted_at <- requested_at + 2

  interrupted <- store_interrupt_agent_run(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    worker_id = "worker-1",
    terminal_reason = "interrupt_unconfirmed:reader_cancelled",
    interrupted_at = interrupted_at
  )

  testthat::expect_identical(interrupted$status, "interrupted")
  testthat::expect_identical(interrupted$terminal_at, interrupted_at)
  testthat::expect_identical(
    interrupted$terminal_reason,
    "interrupt_unconfirmed:reader_cancelled"
  )
  testthat::expect_null(interrupted$lease_expires_at)
  testthat::expect_null(
    store_interrupt_agent_run(
      store,
      reader_id = "reader-1",
      run_id = run$run_id,
      worker_id = "worker-1",
      terminal_reason = "interrupt_unconfirmed:reader_cancelled",
      interrupted_at = interrupted_at + 1
    )
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

testthat::test_that("a failed pending Agent Run releases the Reader", {
  store <- rill_store(list(demo_mode = TRUE))
  run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "ask-rill-message-17",
    pinned_inputs = list(message_id = "message-17"),
    worker_id = "worker-1"
  )
  failed_at <- as.POSIXct("2026-09-02 12:01:00", tz = "UTC")

  failed <- store_fail_unstarted_agent_run(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    worker_id = "worker-1",
    phase = "start",
    terminal_reason = "claim_error:test_database_error",
    failed_at = failed_at
  )

  testthat::expect_identical(failed$status, "failed")
  testthat::expect_identical(
    failed$terminal_reason,
    "claim_error:test_database_error"
  )
  testthat::expect_identical(failed$terminal_at, failed_at)
  testthat::expect_null(
    store_fail_unstarted_agent_run(
      store,
      reader_id = "reader-1",
      run_id = run$run_id,
      worker_id = "worker-1",
      phase = "start",
      terminal_reason = "claim_failed",
      failed_at = failed_at + 1
    )
  )
  next_run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "ask-rill-message-18",
    pinned_inputs = list(message_id = "message-18")
  )
  testthat::expect_identical(next_run$status, "pending")

  claimed <- store_start_agent_run(
    store,
    reader_id = "reader-2",
    kind = "question",
    request_key = "ask-rill-message-19",
    pinned_inputs = list(message_id = "message-19"),
    worker_id = "worker-2"
  )
  claimed <- store_claim_agent_run(
    store,
    reader_id = "reader-2",
    run_id = claimed$run_id,
    worker_id = "worker-2",
    lease_expires_at = failed_at + 300
  )
  testthat::expect_null(
    store_fail_unstarted_agent_run(
      store,
      reader_id = "reader-2",
      run_id = claimed$run_id,
      worker_id = "different-worker",
      phase = "claim",
      terminal_reason = "claim_failed",
      failed_at = failed_at + 1
    )
  )
  testthat::expect_null(
    store_fail_unstarted_agent_run(
      store,
      reader_id = "reader-2",
      run_id = claimed$run_id,
      worker_id = "worker-2",
      phase = "start",
      terminal_reason = "start_error:test_database_error",
      failed_at = failed_at + 1
    )
  )
  failed_claim <- store_fail_unstarted_agent_run(
    store,
    reader_id = "reader-2",
    run_id = claimed$run_id,
    worker_id = "worker-2",
    phase = "claim",
    terminal_reason = "claim_error:test_database_error",
    failed_at = failed_at + 1
  )
  testthat::expect_identical(failed_claim$status, "failed")
  testthat::expect_null(failed_claim$partial_response)
  testthat::expect_null(failed_claim$lease_expires_at)
})

testthat::test_that("the lease owner records reconnectable partial output", {
  store <- rill_store(list(demo_mode = TRUE))
  run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "ask-rill-message-17",
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
    kind = "question",
    request_key = "ask-rill-message-17",
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
    kind = "question",
    request_key = "ask-rill-message-17",
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
  testthat::expect_null(cancelled$lease_expires_at)
  next_run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "ask-rill-message-18",
    pinned_inputs = list(message_id = "message-18")
  )
  testthat::expect_identical(next_run$status, "pending")
})

testthat::test_that("a Reader question cooperatively supersedes Orientation", {
  requested_at <- as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  prioritized_at <- requested_at + 3

  for (status in c("pending", "running", "cancelling")) {
    store <- rill_store(list(demo_mode = TRUE))
    orientation <- store_start_agent_run(
      store,
      reader_id = "reader-1",
      kind = "orientation",
      request_key = "orientation-library-42",
      pinned_inputs = list(document_ids = "document-1"),
      requested_at = requested_at,
      worker_id = "other-session"
    )
    if (!identical(status, "pending")) {
      orientation <- store_claim_agent_run(
        store,
        reader_id = "reader-1",
        run_id = orientation$run_id,
        worker_id = "other-session",
        started_at = requested_at + 1,
        lease_expires_at = requested_at + 120
      )
    }
    if (identical(status, "cancelling")) {
      orientation <- store_request_agent_run_cancel(
        store,
        reader_id = "reader-1",
        run_id = orientation$run_id,
        requested_at = requested_at + 2
      )
    }

    interrupted <- store_prioritize_reader_question(
      store,
      reader_id = "reader-1",
      requested_at = prioritized_at
    )

    expected_status <- if (identical(status, "pending")) {
      "cancelled"
    } else {
      "cancelling"
    }
    testthat::expect_identical(interrupted$status, expected_status)
    testthat::expect_identical(interrupted$worker_id, "other-session")
    if (identical(status, "pending")) {
      testthat::expect_identical(
        interrupted$terminal_reason,
        "reader_question"
      )
      testthat::expect_identical(interrupted$terminal_at, prioritized_at)
      testthat::expect_null(interrupted$lease_expires_at)
    } else {
      testthat::expect_null(interrupted$terminal_reason)
      testthat::expect_null(interrupted$terminal_at)
      testthat::expect_identical(
        interrupted$lease_expires_at,
        orientation$lease_expires_at
      )
      testthat::expect_error(
        store_start_agent_run(
          store,
          reader_id = "reader-1",
          kind = "question",
          request_key = "question-before-stop",
          pinned_inputs = list(message_id = "message-before-stop"),
          requested_at = prioritized_at
        ),
        class = "rill_agent_run_conflict"
      )
      interrupted <- store_finish_agent_run(
        store,
        reader_id = "reader-1",
        run_id = orientation$run_id,
        worker_id = "other-session",
        status = "cancelled",
        terminal_reason = "reader_question",
        finished_at = prioritized_at + 1
      )
      testthat::expect_identical(interrupted$status, "cancelled")
    }
    question <- store_start_agent_run(
      store,
      reader_id = "reader-1",
      kind = "question",
      request_key = "ask-rill-message-17",
      pinned_inputs = list(message_id = "message-17"),
      requested_at = prioritized_at
    )
    testthat::expect_identical(question$status, "pending")
  }

  question_store <- rill_store(list(demo_mode = TRUE))
  question <- store_start_agent_run(
    question_store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "ask-rill-message-17",
    pinned_inputs = list(message_id = "message-17"),
    requested_at = requested_at
  )
  testthat::expect_null(
    store_prioritize_reader_question(
      question_store,
      reader_id = "reader-1",
      requested_at = prioritized_at
    )
  )
  testthat::expect_identical(
    store_get_agent_run(question_store, "reader-1", question$run_id)$status,
    "pending"
  )
})

testthat::test_that("question replay does not preempt unrelated Orientation", {
  store <- rill_store(list(demo_mode = TRUE))
  requested_at <- as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  inputs <- list(document_id = "document-1", question = "What changed?")
  question <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "question-replay",
    pinned_inputs = inputs,
    requested_at = requested_at,
    worker_id = "question-worker"
  )
  question <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = question$run_id,
    worker_id = "question-worker",
    started_at = requested_at + 1,
    lease_expires_at = requested_at + 120
  )
  question <- store_finish_agent_run(
    store,
    reader_id = "reader-1",
    run_id = question$run_id,
    worker_id = "question-worker",
    status = "completed",
    finished_at = requested_at + 2
  )
  orientation <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = "current-orientation",
    pinned_inputs = list(document_ids = "document-2"),
    requested_at = requested_at + 3,
    worker_id = "orientation-worker"
  )

  replay <- store_start_prioritized_reader_question(
    store,
    reader_id = "reader-1",
    request_key = "question-replay",
    pinned_inputs = inputs,
    requested_at = requested_at + 4,
    worker_id = "new-question-worker"
  )

  testthat::expect_identical(replay$run$run_id, question$run_id)
  testthat::expect_null(replay$preempted)
  testthat::expect_identical(
    store_get_agent_run(store, "reader-1", orientation$run_id)$status,
    "pending"
  )

  invalid_retry <- store_start_prioritized_reader_question(
    store,
    reader_id = "reader-1",
    request_key = "invalid-orientation-retry",
    retry_of = orientation,
    requested_at = requested_at + 5,
    worker_id = "new-question-worker"
  )
  testthat::expect_null(invalid_retry$run)
  testthat::expect_null(invalid_retry$preempted)
  testthat::expect_identical(
    store_get_agent_run(store, "reader-1", orientation$run_id)$status,
    "pending"
  )
})

testthat::test_that("prioritized questions wait for Orientation stop confirmation", {
  store <- rill_store(list(demo_mode = TRUE))
  requested_at <- as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  orientation <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "orientation",
    request_key = "orientation-current-library",
    pinned_inputs = list(document_ids = "document-1"),
    requested_at = requested_at,
    worker_id = "orientation-worker"
  )
  orientation <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = orientation$run_id,
    worker_id = "orientation-worker",
    started_at = requested_at + 1,
    lease_expires_at = requested_at + 120
  )
  inputs <- list(document_id = "document-1", question = "What changed?")

  waiting <- store_start_prioritized_reader_question(
    store,
    reader_id = "reader-1",
    request_key = "question-after-orientation",
    pinned_inputs = inputs,
    requested_at = requested_at + 2,
    worker_id = "question-worker"
  )

  testthat::expect_null(waiting$run)
  testthat::expect_identical(waiting$preempted$status, "cancelling")
  testthat::expect_null(waiting$preempted$terminal_at)
  testthat::expect_identical(
    waiting$preempted$lease_expires_at,
    orientation$lease_expires_at
  )

  stopped <- store_finish_agent_run(
    store,
    reader_id = "reader-1",
    run_id = orientation$run_id,
    worker_id = "orientation-worker",
    status = "cancelled",
    terminal_reason = "reader_question",
    finished_at = requested_at + 3
  )
  testthat::expect_identical(stopped$status, "cancelled")

  started <- store_start_prioritized_reader_question(
    store,
    reader_id = "reader-1",
    request_key = "question-after-orientation",
    pinned_inputs = inputs,
    requested_at = requested_at + 2,
    worker_id = "question-worker"
  )
  testthat::expect_identical(started$run$status, "pending")
  testthat::expect_null(started$preempted)
})

testthat::test_that("terminal Agent Runs clear partial state and release the Reader", {
  store <- rill_store(list(demo_mode = TRUE))
  run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "ask-rill-message-17",
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

testthat::test_that("ordinary lease recovery only clears unstarted work", {
  store <- rill_store(list(demo_mode = TRUE))
  requested_at <- as.POSIXct("2026-09-02 12:00:00", tz = "UTC")
  run <- store_start_agent_run(
    store,
    reader_id = "reader-1",
    kind = "question",
    request_key = "ask-rill-message-17",
    pinned_inputs = list(message_id = "message-17"),
    requested_at = requested_at
  )
  run <- store_claim_agent_run(
    store,
    reader_id = "reader-1",
    run_id = run$run_id,
    worker_id = "worker-1",
    started_at = requested_at + 60,
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
  active <- store_start_agent_run(
    store,
    reader_id = "reader-2",
    kind = "question",
    request_key = "ask-rill-message-18",
    pinned_inputs = list(message_id = "message-18"),
    requested_at = requested_at
  )
  active <- store_claim_agent_run(
    store,
    reader_id = "reader-2",
    run_id = active$run_id,
    worker_id = "worker-2",
    started_at = requested_at,
    lease_expires_at = requested_at + 300
  )
  pending <- store_start_agent_run(
    store,
    reader_id = "reader-3",
    kind = "orientation",
    request_key = "orientation-never-started",
    pinned_inputs = list(document_ids = "document-1"),
    requested_at = requested_at
  )
  recovered_at <- as.POSIXct("2026-09-02 12:03:00", tz = "UTC")

  interrupted <- store_interrupt_expired_agent_runs(store, recovered_at)

  testthat::expect_length(interrupted, 1L)
  testthat::expect_identical(interrupted[[1]]$run_id, pending$run_id)
  testthat::expect_identical(interrupted[[1]]$status, "interrupted")
  testthat::expect_identical(interrupted[[1]]$terminal_at, recovered_at)
  testthat::expect_identical(
    interrupted[[1]]$terminal_reason,
    "lease_expired"
  )
  testthat::expect_null(interrupted[[1]]$partial_response)
  testthat::expect_null(interrupted[[1]]$lease_expires_at)
  testthat::expect_length(
    store_interrupt_expired_agent_runs(store, recovered_at),
    0L
  )
  testthat::expect_error(
    store_start_agent_run(
      store,
      reader_id = "reader-2",
      kind = "question",
      request_key = "ask-rill-message-19",
      pinned_inputs = list(message_id = "message-19"),
      requested_at = recovered_at
    ),
    class = "rill_agent_run_conflict"
  )
  preserved <- store_get_agent_run(store, "reader-1", run$run_id)
  testthat::expect_identical(preserved$status, "running")
  testthat::expect_identical(
    preserved$partial_response,
    "This output will not survive"
  )
  testthat::expect_error(
    store_start_agent_run(
      store,
      reader_id = "reader-1",
      kind = "question",
      request_key = "ask-rill-message-20",
      pinned_inputs = list(message_id = "message-20"),
      requested_at = recovered_at
    ),
    class = "rill_agent_run_conflict"
  )
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
    kind = "question",
    request_key = "ask-rill-message-17",
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

  testthat::expect_length(unique(c(retry$run_id, original$run_id)), 2L)
  testthat::expect_identical(retry$retry_of_run_id, original$run_id)
  testthat::expect_identical(retry$pinned_inputs, pinned_inputs)
  testthat::expect_identical(retry$kind, original$kind)
  testthat::expect_identical(retry$status, "pending")
  testthat::expect_identical(replay, retry)
})
