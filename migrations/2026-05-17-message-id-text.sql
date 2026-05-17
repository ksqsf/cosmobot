-- Convert persisted MessageId columns from INTEGER storage to TEXT storage.
--
-- Usage:
--   sqlite3 path/to/cosmobot.sqlite3 < migrations/2026-05-17-message-id-text.sql
--
-- Make a backup before running this against a live database.

PRAGMA foreign_keys = OFF;

BEGIN IMMEDIATE;

DROP TABLE IF EXISTS conversation_nodes_scoped_new;

CREATE TABLE conversation_nodes_scoped_new
  ( id INTEGER PRIMARY KEY
  , platform_key TEXT NOT NULL
  , chat_id INTEGER
  , message_id TEXT NOT NULL
  , conversation_id INTEGER
  , parent_chat_id INTEGER
  , parent_message_id TEXT
  , messages_json TEXT NOT NULL
  );

INSERT INTO conversation_nodes_scoped_new
  ( id
  , platform_key
  , chat_id
  , message_id
  , conversation_id
  , parent_chat_id
  , parent_message_id
  , messages_json
  )
SELECT
  id
, platform_key
, chat_id
, CAST(message_id AS TEXT)
, conversation_id
, parent_chat_id
, CAST(parent_message_id AS TEXT)
, messages_json
FROM conversation_nodes_scoped;

DROP TABLE conversation_nodes_scoped;
ALTER TABLE conversation_nodes_scoped_new RENAME TO conversation_nodes_scoped;

CREATE INDEX IF NOT EXISTS idx_conversation_nodes_scoped_platform_key
  ON conversation_nodes_scoped (platform_key);
CREATE INDEX IF NOT EXISTS idx_conversation_nodes_scoped_chat_id
  ON conversation_nodes_scoped (chat_id);
CREATE INDEX IF NOT EXISTS idx_conversation_nodes_scoped_message_id
  ON conversation_nodes_scoped (message_id);
CREATE INDEX IF NOT EXISTS idx_conversation_nodes_scoped_conversation_id
  ON conversation_nodes_scoped (conversation_id);
CREATE INDEX IF NOT EXISTS idx_conversation_nodes_scoped_parent_message_id
  ON conversation_nodes_scoped (parent_message_id);

DROP TABLE IF EXISTS chat_log_entries_new;

CREATE TABLE chat_log_entries_new
  ( id INTEGER PRIMARY KEY
  , platform_key TEXT NOT NULL
  , kind_key TEXT NOT NULL
  , chat_id INTEGER
  , sender_id TEXT
  , sender_username TEXT
  , message_id TEXT
  , reply_to_message_id TEXT
  , is_bot BOOLEAN NOT NULL
  , mentions TEXT NOT NULL
  , mention_usernames TEXT NOT NULL
  , image_urls TEXT NOT NULL
  , body_text TEXT NOT NULL
  );

INSERT INTO chat_log_entries_new
  ( id
  , platform_key
  , kind_key
  , chat_id
  , sender_id
  , sender_username
  , message_id
  , reply_to_message_id
  , is_bot
  , mentions
  , mention_usernames
  , image_urls
  , body_text
  )
SELECT
  id
, platform_key
, kind_key
, chat_id
, sender_id
, sender_username
, CAST(message_id AS TEXT)
, CAST(reply_to_message_id AS TEXT)
, is_bot
, mentions
, mention_usernames
, image_urls
, body_text
FROM chat_log_entries;

DROP TABLE chat_log_entries;
ALTER TABLE chat_log_entries_new RENAME TO chat_log_entries;

CREATE INDEX IF NOT EXISTS idx_chat_log_entries_platform_key
  ON chat_log_entries (platform_key);
CREATE INDEX IF NOT EXISTS idx_chat_log_entries_kind_key
  ON chat_log_entries (kind_key);
CREATE INDEX IF NOT EXISTS idx_chat_log_entries_chat_id
  ON chat_log_entries (chat_id);
CREATE INDEX IF NOT EXISTS idx_chat_log_entries_sender_id
  ON chat_log_entries (sender_id);

DROP TABLE IF EXISTS agent_trace_new;

CREATE TABLE agent_trace_new
  ( id INTEGER PRIMARY KEY
  , run_id TEXT NOT NULL
  , occurred_at DATETIME NOT NULL
  , linked_message_id TEXT
  , parent_message_id TEXT
  , event_json TEXT NOT NULL
  );

INSERT INTO agent_trace_new
  ( id
  , run_id
  , occurred_at
  , linked_message_id
  , parent_message_id
  , event_json
  )
SELECT
  id
, run_id
, occurred_at
, CAST(linked_message_id AS TEXT)
, CAST(parent_message_id AS TEXT)
, event_json
FROM agent_trace;

DROP TABLE agent_trace;
ALTER TABLE agent_trace_new RENAME TO agent_trace;

CREATE INDEX IF NOT EXISTS idx_agent_trace_run_id
  ON agent_trace (run_id);
CREATE INDEX IF NOT EXISTS idx_agent_trace_linked_message_id
  ON agent_trace (linked_message_id);
CREATE INDEX IF NOT EXISTS idx_agent_trace_parent_message_id
  ON agent_trace (parent_message_id);

-- event_json may still contain historic numeric MessageId values. The current
-- decoder accepts both JSON strings and JSON numbers, so the payload is left
-- unchanged here.

COMMIT;

PRAGMA foreign_keys = ON;
