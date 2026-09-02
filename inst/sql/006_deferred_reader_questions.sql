CREATE TABLE deferred_reader_questions (
  reader_id text PRIMARY KEY,
  request_key text NOT NULL,
  pinned_inputs jsonb NOT NULL,
  retry_of_run_id text REFERENCES agent_runs(run_id),
  requested_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL DEFAULT now(),
  CONSTRAINT deferred_reader_questions_reader_request_key
    UNIQUE (reader_id, request_key)
);
