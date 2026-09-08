feedback_test_run <- function(store, reader_id, key, requested_at) {
  run <- store_start_agent_run(
    store,
    reader_id,
    "question",
    key,
    pinned_inputs = list(
      question = paste("Question", key),
      model = "test-model",
      document_content_hash = "content-hash",
      document_record_hash = "record-hash"
    ),
    requested_at = requested_at,
    worker_id = "test-worker"
  )
  store_claim_agent_run(
    store,
    reader_id,
    run$run_id,
    "test-worker",
    lease_expires_at = requested_at + 120,
    started_at = requested_at
  )
  store_record_agent_run_response(
    store,
    reader_id,
    run$run_id,
    "test-worker",
    paste("Answer", key),
    updated_at = requested_at + 1
  )
  store_finish_agent_run(
    store,
    reader_id,
    run$run_id,
    "test-worker",
    "completed",
    finished_at = requested_at + 2
  )
}
