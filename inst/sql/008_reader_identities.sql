CREATE TABLE readers (
  reader_id text PRIMARY KEY,
  status text NOT NULL DEFAULT 'active' CHECK (
    status IN ('active', 'disabled')
  ),
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  disabled_at timestamptz
);

CREATE TABLE reader_external_identities (
  issuer text NOT NULL,
  subject text NOT NULL,
  reader_id text NOT NULL REFERENCES readers(reader_id) ON DELETE CASCADE,
  email text,
  display_name text,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  last_seen_at timestamptz,
  revoked_at timestamptz,
  PRIMARY KEY (issuer, subject)
);

CREATE INDEX reader_external_identities_reader_idx
  ON reader_external_identities (reader_id);

CREATE TABLE reader_admission_requests (
  issuer text NOT NULL,
  subject text NOT NULL,
  status text NOT NULL DEFAULT 'pending' CHECK (
    status IN ('pending', 'approved', 'rejected')
  ),
  email text,
  display_name text,
  first_seen_at timestamptz NOT NULL,
  last_seen_at timestamptz NOT NULL,
  attempt_count integer NOT NULL DEFAULT 1 CHECK (attempt_count > 0),
  decided_at timestamptz,
  PRIMARY KEY (issuer, subject)
);

CREATE TABLE reader_identity_events (
  event_sequence bigint GENERATED ALWAYS AS IDENTITY UNIQUE NOT NULL,
  event_id text PRIMARY KEY,
  reader_id text,
  issuer text,
  subject text,
  action text NOT NULL,
  responsible_id text NOT NULL,
  reason text NOT NULL,
  happened_at timestamptz NOT NULL
);

CREATE INDEX reader_identity_events_reader_time_idx
  ON reader_identity_events (reader_id, happened_at DESC);
