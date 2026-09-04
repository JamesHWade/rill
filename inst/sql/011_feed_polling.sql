CREATE TABLE feed_poll_runs (
  run_id text PRIMARY KEY,
  started_at timestamptz NOT NULL,
  completed_at timestamptz,
  status text NOT NULL CHECK (
    status IN ('running', 'succeeded', 'partial', 'failed')
  ),
  due_count integer NOT NULL CHECK (due_count >= 0),
  succeeded_count integer NOT NULL DEFAULT 0 CHECK (succeeded_count >= 0),
  failed_count integer NOT NULL DEFAULT 0 CHECK (failed_count >= 0),
  failure_threshold integer NOT NULL CHECK (failure_threshold >= 1),
  error_class text,
  error_message text,
  CHECK (succeeded_count + failed_count <= due_count),
  CHECK (
    (status = 'running' AND completed_at IS NULL)
    OR
    (status <> 'running' AND completed_at IS NOT NULL)
  )
);

CREATE TABLE feed_poll_outcomes (
  run_id text NOT NULL REFERENCES feed_poll_runs(run_id) ON DELETE CASCADE,
  feed_id text NOT NULL REFERENCES feeds(feed_id) ON DELETE CASCADE,
  started_at timestamptz NOT NULL,
  completed_at timestamptz NOT NULL,
  status text NOT NULL CHECK (
    status IN ('updated', 'not_modified', 'failed')
  ),
  added_count integer NOT NULL DEFAULT 0 CHECK (added_count >= 0),
  error_class text,
  error_message text,
  PRIMARY KEY (run_id, feed_id),
  CHECK (
    (status = 'failed' AND error_class IS NOT NULL)
    OR
    (status <> 'failed' AND error_class IS NULL AND error_message IS NULL)
  )
);

CREATE INDEX feed_poll_runs_started_idx
  ON feed_poll_runs (started_at DESC);

CREATE INDEX feed_poll_outcomes_feed_idx
  ON feed_poll_outcomes (feed_id, completed_at DESC);
