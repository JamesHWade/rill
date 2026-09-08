CREATE TABLE reader_feedback (
  reader_id text NOT NULL REFERENCES readers(reader_id) ON DELETE CASCADE,
  target_id text NOT NULL,
  record jsonb NOT NULL,
  PRIMARY KEY (reader_id, target_id)
);
