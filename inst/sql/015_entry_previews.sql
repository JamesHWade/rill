ALTER TABLE entries
  ADD COLUMN IF NOT EXISTS preview_image_url text,
  ADD COLUMN IF NOT EXISTS preview_image_alt text;
