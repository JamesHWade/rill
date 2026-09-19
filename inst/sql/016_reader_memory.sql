-- Content and decision history belong to Graft; this is Rill's private catalog.
CREATE TABLE reader_memory_index (
  reader_id text NOT NULL REFERENCES readers(reader_id),
  memory_id text NOT NULL,
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (reader_id, memory_id)
);
