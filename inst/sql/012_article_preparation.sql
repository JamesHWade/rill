CREATE TABLE article_preparations (
  entry_id text PRIMARY KEY REFERENCES entries(entry_id) ON DELETE CASCADE,
  token text NOT NULL,
  attempts integer NOT NULL CHECK (attempts > 0),
  status text NOT NULL CHECK (status IN ('running', 'failed', 'succeeded')),
  next_attempt_at timestamptz NOT NULL,
  failure jsonb
);
