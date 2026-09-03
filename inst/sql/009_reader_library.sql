INSERT INTO readers (reader_id, status, created_at, updated_at)
VALUES (current_setting('rill.legacy_reader_id'), 'active', now(), now())
ON CONFLICT (reader_id) DO NOTHING;

CREATE TABLE subscriptions (
  reader_id text NOT NULL REFERENCES readers(reader_id),
  feed_id text NOT NULL REFERENCES feeds(feed_id),
  folder text NOT NULL DEFAULT 'Unsorted',
  display_title text,
  status text NOT NULL DEFAULT 'active' CHECK (
    status IN ('active', 'inactive')
  ),
  subscribed_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now(),
  deactivated_at timestamptz,
  PRIMARY KEY (reader_id, feed_id)
);

INSERT INTO subscriptions (
  reader_id,
  feed_id,
  folder,
  display_title,
  subscribed_at,
  updated_at
)
SELECT
  current_setting('rill.legacy_reader_id'),
  f.feed_id,
  f.folder,
  p.display_title,
  f.created_at,
  now()
FROM feeds f
LEFT JOIN subscription_preferences p
  ON p.reader_id = current_setting('rill.legacy_reader_id')
  AND p.feed_id = f.feed_id
WHERE f.source_kind = 'subscription'
  OR EXISTS (
    SELECT 1
    FROM entries e
    JOIN entry_state s ON s.entry_id = e.entry_id
    WHERE e.feed_id = f.feed_id
      AND s.actor_id = current_setting('rill.legacy_reader_id')
  )
  OR EXISTS (
    SELECT 1
    FROM entries e
    JOIN events v ON v.entry_id = e.entry_id
    WHERE e.feed_id = f.feed_id
      AND v.actor_id = current_setting('rill.legacy_reader_id')
  );

ALTER TABLE subscription_preferences
  ADD CONSTRAINT subscription_preferences_reader_fk
  FOREIGN KEY (reader_id) REFERENCES readers(reader_id);

ALTER TABLE entries
  ADD CONSTRAINT entries_feed_entry_key UNIQUE (feed_id, entry_id);

ALTER TABLE entry_state RENAME COLUMN actor_id TO reader_id;

ALTER TABLE entry_state ADD COLUMN feed_id text;

UPDATE entry_state s
SET feed_id = e.feed_id
FROM entries e
WHERE e.entry_id = s.entry_id;

ALTER TABLE entry_state ALTER COLUMN feed_id SET NOT NULL;

ALTER TABLE entry_state
  ADD CONSTRAINT entry_state_reader_fk
  FOREIGN KEY (reader_id) REFERENCES readers(reader_id);

ALTER TABLE entry_state DROP CONSTRAINT IF EXISTS entry_state_entry_id_fkey;

ALTER TABLE entry_state
  ADD CONSTRAINT entry_state_entry_fk
  FOREIGN KEY (feed_id, entry_id) REFERENCES entries(feed_id, entry_id);

ALTER TABLE entry_state
  ADD CONSTRAINT entry_state_subscription_fk
  FOREIGN KEY (reader_id, feed_id)
  REFERENCES subscriptions(reader_id, feed_id);

DROP INDEX IF EXISTS entry_state_actor_idx;

CREATE INDEX entry_state_reader_idx
  ON entry_state (reader_id, read_at, starred, saved);

ALTER TABLE events RENAME COLUMN actor_id TO reader_id;

ALTER TABLE events ADD COLUMN feed_id text;

UPDATE events v
SET feed_id = e.feed_id
FROM entries e
WHERE e.entry_id = v.entry_id;

ALTER TABLE events
  ADD CONSTRAINT events_entry_consistency_check
  CHECK ((entry_id IS NULL) = (feed_id IS NULL));

ALTER TABLE events
  ADD CONSTRAINT events_reader_fk
  FOREIGN KEY (reader_id) REFERENCES readers(reader_id);

ALTER TABLE events DROP CONSTRAINT IF EXISTS events_entry_id_fkey;

ALTER TABLE events
  ADD CONSTRAINT events_subscription_fk
  FOREIGN KEY (reader_id, feed_id)
  REFERENCES subscriptions(reader_id, feed_id);

DROP INDEX IF EXISTS events_actor_time_idx;

CREATE INDEX events_reader_time_idx
  ON events (reader_id, happened_at DESC);

ALTER TABLE agent_runs
  ADD CONSTRAINT agent_runs_reader_fk
  FOREIGN KEY (reader_id) REFERENCES readers(reader_id);

ALTER TABLE orientations
  ADD CONSTRAINT orientations_reader_fk
  FOREIGN KEY (reader_id) REFERENCES readers(reader_id);

ALTER TABLE orientation_destination_settings
  ADD CONSTRAINT orientation_destination_settings_reader_fk
  FOREIGN KEY (reader_id) REFERENCES readers(reader_id);

ALTER TABLE deferred_reader_questions
  ADD CONSTRAINT deferred_reader_questions_reader_fk
  FOREIGN KEY (reader_id) REFERENCES readers(reader_id);
