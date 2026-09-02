CREATE TABLE orientation_destination_settings (
  reader_id text PRIMARY KEY,
  enabled boolean NOT NULL DEFAULT false,
  destination_id text NOT NULL,
  destination_name text NOT NULL,
  destination_kind text NOT NULL CHECK (
    destination_kind IN ('external', 'installation')
  ),
  policy_url text,
  confirmed_at timestamptz,
  updated_at timestamptz NOT NULL DEFAULT now(),
  CHECK (
    destination_kind <> 'external' OR NOT enabled OR confirmed_at IS NOT NULL
  )
);
