CREATE TABLE feed_groups (
  reader_id text NOT NULL REFERENCES readers(reader_id) ON DELETE CASCADE,
  group_id text NOT NULL,
  name text NOT NULL CHECK (length(btrim(name)) > 0),
  created_at timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (reader_id, group_id),
  UNIQUE (reader_id, name)
);

CREATE TABLE subscription_groups (
  reader_id text NOT NULL,
  feed_id text NOT NULL,
  group_id text NOT NULL,
  PRIMARY KEY (reader_id, feed_id, group_id),
  FOREIGN KEY (reader_id, feed_id)
    REFERENCES subscriptions(reader_id, feed_id) ON DELETE CASCADE,
  FOREIGN KEY (reader_id, group_id)
    REFERENCES feed_groups(reader_id, group_id) ON DELETE CASCADE
);
CREATE INDEX subscription_groups_group_idx
  ON subscription_groups (reader_id, group_id, feed_id);

INSERT INTO feed_groups (reader_id, group_id, name)
SELECT DISTINCT s.reader_id, md5(s.reader_id || ':' || s.folder), s.folder
FROM subscriptions s JOIN feeds f USING (feed_id)
WHERE lower(s.folder) <> 'unsorted' AND f.source_kind = 'subscription';
INSERT INTO subscription_groups (reader_id, feed_id, group_id)
SELECT s.reader_id, s.feed_id, g.group_id
FROM subscriptions s JOIN feed_groups g
  ON g.reader_id = s.reader_id AND g.name = s.folder
JOIN feeds f ON f.feed_id = s.feed_id
WHERE f.source_kind = 'subscription';

-- Keep folder writes from a preceding application instance additive during rollout.
-- Current clients use memberships and project one name into this compatibility field.
CREATE FUNCTION subscription_folder_group() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE selected_group text;
BEGIN
  IF lower(NEW.folder) <> 'unsorted' AND EXISTS (
    SELECT 1 FROM feeds WHERE feed_id = NEW.feed_id AND source_kind = 'subscription'
  ) THEN
    INSERT INTO feed_groups (reader_id, group_id, name)
    VALUES (NEW.reader_id, md5(random()::text || clock_timestamp()::text || NEW.reader_id), NEW.folder)
    ON CONFLICT (reader_id, name) DO NOTHING;
    SELECT group_id INTO selected_group FROM feed_groups
    WHERE reader_id = NEW.reader_id AND name = NEW.folder;
    INSERT INTO subscription_groups (reader_id, feed_id, group_id)
    VALUES (NEW.reader_id, NEW.feed_id, selected_group)
    ON CONFLICT DO NOTHING;
  END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER subscription_folder_group_trigger
AFTER INSERT OR UPDATE OF folder ON subscriptions
FOR EACH ROW EXECUTE FUNCTION subscription_folder_group();
