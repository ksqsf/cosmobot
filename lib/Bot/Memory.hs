{-|
Module      : Bot.Memory
Description : Per-sender and per-chat structural memory files
Stability   : experimental
-}

module Bot.Memory
  ( MemoryConfig (..)
  , MemoryScope (..)
  , memoryLimitChars
  , senderMemoryScope
  , chatMemoryScope
  , loadMemory
  , replaceMemory
  , clearMemory
  , memoryPath
  )
where

import Bot.Core.Message
import Bot.Prelude
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory
import System.FilePath

-- | Filesystem-backed memory settings.
newtype MemoryConfig = MemoryConfig
  { dir :: FilePath
  }
  deriving (Show)

data MemoryScope
  = SenderMemory !ChatPlatform !Text
  | ChatMemory !ChatPlatform !Integer
  deriving (Eq, Show)

memoryLimitChars :: Int
memoryLimitChars = 1000

senderMemoryScope :: IncomingMessage -> Either Text MemoryScope
senderMemoryScope message =
  case message.senderId of
    Nothing ->
      Left "No sender id is available for this message."
    Just senderId ->
      Right (SenderMemory message.platform senderId)

chatMemoryScope :: IncomingMessage -> Either Text MemoryScope
chatMemoryScope message =
  case message.chatId of
    Nothing ->
      Left "No chat id is available for this message."
    Just chatId ->
      Right (ChatMemory message.platform chatId)

loadMemory :: IOE :> es => MemoryConfig -> MemoryScope -> Eff es (Maybe Text)
loadMemory cfg scope = liftIO do
  exists <- doesFileExist path
  if exists
    then nonEmptyMemory <$> TextIO.readFile path
    else pure Nothing
  where
    path = memoryPath cfg scope

replaceMemory :: IOE :> es => MemoryConfig -> MemoryScope -> Text -> Eff es ()
replaceMemory cfg scope memory = liftIO do
  createDirectoryIfMissing True (takeDirectory path)
  TextIO.writeFile path (Text.strip memory)
  where
    path = memoryPath cfg scope

clearMemory :: IOE :> es => MemoryConfig -> MemoryScope -> Eff es ()
clearMemory cfg scope = liftIO do
  exists <- doesFileExist path
  when exists (removeFile path)
  where
    path = memoryPath cfg scope

memoryPath :: MemoryConfig -> MemoryScope -> FilePath
memoryPath cfg scope =
  cfg.dir </> platformPathPart platform </> scopeKind </> Text.unpack scopeId <.> "md"
  where
    (platform, scopeKind, scopeId) =
      case scope of
        SenderMemory scopePlatform senderId ->
          (scopePlatform, "sender", senderId)
        ChatMemory scopePlatform chatId ->
          (scopePlatform, "chat", Text.pack (show chatId))

platformPathPart :: ChatPlatform -> FilePath
platformPathPart = toString . chatPlatformKey

nonEmptyMemory :: Text -> Maybe Text
nonEmptyMemory text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped
