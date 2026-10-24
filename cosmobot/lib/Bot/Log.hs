module Bot.Log
  ( module Bot.Log.Common
  , journalScribe
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as TextBuilder
import Bot.Log.Common
import Language.Haskell.TH (Loc (..))
import Relude
import qualified Systemd.Journal as Journal


journalScribe :: PermitFunc -> Scribe
journalScribe permit =
  Scribe writeJournalItem (pure ()) permit

writeJournalItem :: LogItem a => Item a -> IO ()
writeJournalItem item =
  Journal.sendJournalFields $
    Journal.message (LazyText.toStrict (TextBuilder.toLazyText (unLogStr item._itemMessage)))
      <> Journal.priority (journalPriority item._itemSeverity)
      <> maybe mempty locationFields item._itemLoc
      <> contextFields item._itemPayload

locationFields :: Loc -> Journal.JournalFields
locationFields loc =
  Journal.syslogIdentifier (Text.pack loc.loc_module)
    <> Journal.codeFile loc.loc_filename
    <> Journal.codeLine (fst loc.loc_start)
    <> journalField "HASKELL_MODULE" (TextEncoding.encodeUtf8 (Text.pack loc.loc_module))

contextFields :: LogItem a => a -> Journal.JournalFields
contextFields payload =
  foldMap contextField (AesonKeyMap.toList (payloadObject V2 payload))
  where
    contextField (_, Aeson.Null) = mempty
    contextField (key, value) =
      journalField (journalContextKey (AesonKey.toText key)) (journalValue value)

journalField :: Text -> ByteString.ByteString -> Journal.JournalFields
journalField name value =
  HashMap.singleton (Journal.mkJournalField name) value

journalContextKey :: Text -> Text
journalContextKey key =
  let valid = Text.map normalize (Text.toUpper key)
  in case Text.uncons valid of
    Just (initial, _)
      | isAsciiLetter initial
      , not (reservedJournalField valid) -> valid
    _ -> "KATIP_" <> valid
  where
    normalize char
      | isAsciiLetter char || (char >= '0' && char <= '9') = char
      | otherwise = '_'
    isAsciiLetter char = char >= 'A' && char <= 'Z'

reservedJournalField :: Text -> Bool
reservedJournalField field =
  field `elem`
    [ "MESSAGE"
    , "MESSAGE_ID"
    , "PRIORITY"
    , "CODE_FILE"
    , "CODE_LINE"
    , "CODE_FUNC"
    , "ERRNO"
    , "INVOCATION_ID"
    , "USER_INVOCATION_ID"
    , "SYSLOG_FACILITY"
    , "SYSLOG_IDENTIFIER"
    , "SYSLOG_PID"
    , "DOCUMENTATION"
    , "TID"
    , "UNIT"
    , "USER_UNIT"
    ]
    || "OBJECT_" `Text.isPrefixOf` field

journalValue :: Aeson.Value -> ByteString.ByteString
journalValue = \case
  Aeson.String value -> TextEncoding.encodeUtf8 value
  value -> LazyByteString.toStrict (Aeson.encode value)

journalPriority :: Severity -> Journal.Priority
journalPriority = \case
  DebugS -> Journal.Debug
  InfoS -> Journal.Info
  NoticeS -> Journal.Notice
  WarningS -> Journal.Warning
  ErrorS -> Journal.Error
  CriticalS -> Journal.Critical
  AlertS -> Journal.Alert
  EmergencyS -> Journal.Emergency
