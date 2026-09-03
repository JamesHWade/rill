ALTER TABLE agent_runs
  ADD CONSTRAINT agent_runs_reader_run_key UNIQUE (reader_id, run_id);

CREATE TABLE orientations (
  reader_id text PRIMARY KEY,
  orientation_id text NOT NULL UNIQUE,
  revision_id text NOT NULL,
  boundary_hash text NOT NULL,
  evaluation_run_id text NOT NULL UNIQUE,
  evaluated_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL DEFAULT now(),
  payload jsonb NOT NULL,
  FOREIGN KEY (reader_id, evaluation_run_id)
    REFERENCES agent_runs(reader_id, run_id)
);
