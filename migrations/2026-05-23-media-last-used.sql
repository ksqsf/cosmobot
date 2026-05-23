-- Add last-used tracking for the media cache GC.
--
-- Usage:
--   sqlite3 path/to/cosmobot.sqlite3 < migrations/2026-05-23-media-last-used.sql
--
-- Make a backup before running this against a live database.

BEGIN IMMEDIATE;

ALTER TABLE media_files
  ADD COLUMN last_used_at_unix INTEGER NOT NULL DEFAULT 0;

UPDATE media_files
SET last_used_at_unix = created_at_unix
WHERE last_used_at_unix = 0;

COMMIT;
