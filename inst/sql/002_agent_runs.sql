CREATE TABLE agent_runs (
  run_id text PRIMARY KEY,
  reader_id text NOT NULL,
  kind text NOT NULL,
  request_key text NOT NULL,
  retry_of_run_id text REFERENCES agent_runs(run_id),
  status text NOT NULL,
  pinned_inputs jsonb NOT NULL,
  requested_at timestamptz NOT NULL,
  started_at timestamptz,
  updated_at timestamptz NOT NULL,
  terminal_at timestamptz,
  worker_id text,
  lease_expires_at timestamptz,
  partial_response text,
  cancel_requested_at timestamptz,
  usage jsonb,
  terminal_reason text,
  deputy_run_id text,
  CONSTRAINT agent_runs_reader_request_key UNIQUE (reader_id, request_key),
  CONSTRAINT agent_runs_kind_check
    CHECK (kind IN ('orientation', 'conversation')),
  CONSTRAINT agent_runs_status_check
    CHECK (
      status IN (
        'pending',
        'running',
        'cancelling',
        'completed',
        'failed',
        'cancelled',
        'interrupted'
      )
    )
);

CREATE UNIQUE INDEX agent_runs_one_active_reader_idx
  ON agent_runs (reader_id)
  WHERE status IN ('pending', 'running', 'cancelling');

CREATE INDEX agent_runs_reader_requested_idx
  ON agent_runs (reader_id, requested_at DESC);
