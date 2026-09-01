CREATE TABLE IF NOT EXISTS feeds (
  feed_id text PRIMARY KEY,
  feed_url text NOT NULL UNIQUE,
  site_url text,
  title text NOT NULL,
  folder text NOT NULL DEFAULT 'Unsorted',
  source_kind text NOT NULL DEFAULT 'subscription',
  etag text,
  last_modified text,
  poll_status text NOT NULL DEFAULT 'new',
  last_polled_at timestamptz,
  created_at timestamptz NOT NULL DEFAULT now()
);

ALTER TABLE feeds
  ADD COLUMN IF NOT EXISTS source_kind text NOT NULL DEFAULT 'subscription';

CREATE TABLE IF NOT EXISTS entries (
  entry_id text PRIMARY KEY,
  feed_id text NOT NULL REFERENCES feeds(feed_id) ON DELETE CASCADE,
  external_id text NOT NULL,
  url text NOT NULL,
  canonical_url text,
  title text NOT NULL,
  author text,
  summary text,
  feed_content text,
  published_at timestamptz,
  inserted_at timestamptz NOT NULL DEFAULT now(),
  content_hash text,
  UNIQUE (feed_id, external_id)
);

CREATE TABLE IF NOT EXISTS article_documents (
  entry_id text PRIMARY KEY REFERENCES entries(entry_id) ON DELETE CASCADE,
  source_url text NOT NULL,
  engine text NOT NULL,
  engine_version text,
  title text,
  author text,
  site text,
  published_at timestamptz,
  markdown text NOT NULL,
  word_count integer,
  content_hash text,
  fetched_at timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS documents (
  document_id text PRIMARY KEY,
  entry_id text NOT NULL REFERENCES entries(entry_id) ON DELETE CASCADE,
  source_url text NOT NULL,
  canonical_url text,
  acquisition_method text NOT NULL,
  producer text NOT NULL,
  producer_version text,
  producer_record_id text,
  captured_at timestamptz NOT NULL,
  received_at timestamptz NOT NULL DEFAULT now(),
  title text,
  author text,
  site text,
  published_at timestamptz,
  markdown text NOT NULL,
  word_count integer NOT NULL,
  content_hash text NOT NULL,
  record_hash text NOT NULL,
  provenance jsonb NOT NULL DEFAULT '{}'::jsonb,
  schema_version integer NOT NULL DEFAULT 1
);

CREATE TABLE IF NOT EXISTS entry_document_heads (
  entry_id text PRIMARY KEY REFERENCES entries(entry_id) ON DELETE CASCADE,
  document_id text NOT NULL UNIQUE REFERENCES documents(document_id) ON DELETE CASCADE,
  selected_at timestamptz NOT NULL DEFAULT now()
);

INSERT INTO documents (
  document_id,
  entry_id,
  source_url,
  acquisition_method,
  producer,
  producer_version,
  captured_at,
  received_at,
  title,
  author,
  site,
  published_at,
  markdown,
  word_count,
  content_hash,
  record_hash,
  provenance
)
SELECT
  'legacy:' || entry_id,
  entry_id,
  source_url,
  CASE
    WHEN engine = 'feed-fallback' THEN 'feed_fallback'
    WHEN engine = 'sample' THEN 'sample'
    ELSE 'web_extraction'
  END,
  engine,
  engine_version,
  fetched_at,
  fetched_at,
  title,
  author,
  site,
  published_at,
  markdown,
  COALESCE(word_count, 0),
  COALESCE(content_hash, md5(markdown)),
  COALESCE(content_hash, md5(markdown)),
  jsonb_build_object('migrated_from', 'article_documents')
FROM article_documents
ON CONFLICT (document_id) DO NOTHING;

INSERT INTO entry_document_heads (entry_id, document_id, selected_at)
SELECT entry_id, 'legacy:' || entry_id, fetched_at
FROM article_documents
ON CONFLICT (entry_id) DO NOTHING;

CREATE TABLE IF NOT EXISTS entry_state (
  actor_id text NOT NULL,
  entry_id text NOT NULL REFERENCES entries(entry_id) ON DELETE CASCADE,
  read_at timestamptz,
  read_reason text,
  starred boolean NOT NULL DEFAULT false,
  saved boolean NOT NULL DEFAULT false,
  hidden boolean NOT NULL DEFAULT false,
  last_opened_at timestamptz,
  PRIMARY KEY (actor_id, entry_id)
);

CREATE TABLE IF NOT EXISTS subscription_preferences (
  reader_id text NOT NULL,
  feed_id text NOT NULL REFERENCES feeds(feed_id) ON DELETE CASCADE,
  display_title text,
  PRIMARY KEY (reader_id, feed_id)
);

ALTER TABLE entry_state
  ADD COLUMN IF NOT EXISTS read_reason text;

UPDATE entry_state
SET read_reason = 'opened'
WHERE read_at IS NOT NULL
  AND last_opened_at IS NOT NULL
  AND read_reason IS NULL;

CREATE TABLE IF NOT EXISTS events (
  event_id text PRIMARY KEY,
  actor_id text NOT NULL,
  entry_id text REFERENCES entries(entry_id) ON DELETE CASCADE,
  session_id text NOT NULL,
  event_type text NOT NULL,
  happened_at timestamptz NOT NULL,
  surface text,
  position integer,
  payload jsonb NOT NULL DEFAULT '{}'::jsonb,
  schema_version integer NOT NULL DEFAULT 1,
  received_at timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS entries_feed_published_idx
  ON entries (feed_id, published_at DESC);

CREATE INDEX IF NOT EXISTS entry_state_actor_idx
  ON entry_state (actor_id, read_at, starred, saved);

CREATE INDEX IF NOT EXISTS events_actor_time_idx
  ON events (actor_id, happened_at DESC);

CREATE INDEX IF NOT EXISTS events_entry_time_idx
  ON events (entry_id, happened_at DESC);

CREATE INDEX IF NOT EXISTS documents_entry_received_idx
  ON documents (entry_id, received_at DESC);

CREATE INDEX IF NOT EXISTS documents_content_hash_idx
  ON documents (content_hash);
