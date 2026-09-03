ALTER TABLE documents
  ADD COLUMN reader_id text;

UPDATE documents
SET reader_id = current_setting('rill.legacy_reader_id')
WHERE acquisition_method = 'browser_capture'
  AND provenance ->> 'captured_by' = current_setting('rill.legacy_reader_id');

ALTER TABLE documents
  ADD CONSTRAINT documents_reader_fk
  FOREIGN KEY (reader_id) REFERENCES readers(reader_id);

ALTER TABLE documents
  ADD CONSTRAINT documents_scope_check CHECK (
    (acquisition_method = 'browser_capture' AND reader_id IS NOT NULL)
    OR
    (acquisition_method <> 'browser_capture' AND reader_id IS NULL)
  );

ALTER TABLE documents
  ADD COLUMN ownership_key text GENERATED ALWAYS AS (
    CASE
      WHEN reader_id IS NULL THEN 'public'
      ELSE 'reader:' || reader_id
    END
  ) STORED;

ALTER TABLE documents
  ADD CONSTRAINT documents_selection_key
  UNIQUE (entry_id, document_id, ownership_key);

ALTER TABLE entry_document_heads RENAME TO public_document_heads;

CREATE TABLE reader_document_selections (
  reader_id text NOT NULL REFERENCES readers(reader_id),
  feed_id text NOT NULL,
  entry_id text NOT NULL,
  document_id text NOT NULL,
  ownership_key text NOT NULL,
  selected_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (reader_id, entry_id),
  FOREIGN KEY (reader_id, feed_id)
    REFERENCES subscriptions(reader_id, feed_id),
  FOREIGN KEY (feed_id, entry_id)
    REFERENCES entries(feed_id, entry_id),
  FOREIGN KEY (entry_id, document_id, ownership_key)
    REFERENCES documents(entry_id, document_id, ownership_key),
  CHECK (
    ownership_key = 'public'
    OR ownership_key = 'reader:' || reader_id
  )
);

-- ADR 0007 gives every existing global selection only to the legacy Reader.
INSERT INTO reader_document_selections (
  reader_id,
  feed_id,
  entry_id,
  document_id,
  ownership_key,
  selected_at
)
SELECT
  current_setting('rill.legacy_reader_id'),
  e.feed_id,
  h.entry_id,
  h.document_id,
  d.ownership_key,
  h.selected_at
FROM public_document_heads h
JOIN documents d ON d.document_id = h.document_id
JOIN entries e ON e.entry_id = h.entry_id;

DELETE FROM public_document_heads h
USING documents d
WHERE d.document_id = h.document_id
  AND d.reader_id IS NOT NULL;

ALTER TABLE public_document_heads
  ADD COLUMN ownership_key text NOT NULL DEFAULT 'public'
  CHECK (ownership_key = 'public');

ALTER TABLE public_document_heads
  ADD CONSTRAINT public_document_heads_document_fk
  FOREIGN KEY (entry_id, document_id, ownership_key)
  REFERENCES documents(entry_id, document_id, ownership_key);

CREATE TABLE reader_capture_credentials (
  reader_id text PRIMARY KEY REFERENCES readers(reader_id) ON DELETE CASCADE,
  token_hash text NOT NULL UNIQUE,
  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX documents_reader_entry_idx
  ON documents (reader_id, entry_id, received_at DESC);
