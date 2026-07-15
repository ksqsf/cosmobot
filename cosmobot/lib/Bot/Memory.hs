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
  , initializeMemoryRepo
  , commitMemoryUpdate
  , memoryPath
  )
where

import Bot.Core.Message
import Bot.Prelude
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Effectful.Process (Process, proc, readCreateProcessWithExitCode)
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO.Error (userError)

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

initializeMemoryRepo :: (IOE :> es, Process :> es) => MemoryConfig -> Eff es ()
initializeMemoryRepo cfg = do
  liftIO $ createDirectoryIfMissing True cfg.dir
  runGit cfg ["init", "--quiet"]
  hasHead <- gitSucceeds cfg ["rev-parse", "--verify", "HEAD"]
  commitMemory cfg (if hasHead then "Update memory" else "Initialize memory") (not hasHead)

commitMemoryUpdate :: (IOE :> es, Process :> es) => MemoryConfig -> Eff es ()
commitMemoryUpdate cfg =
  commitMemory cfg "Update memory" False

commitMemory :: (IOE :> es, Process :> es) => MemoryConfig -> String -> Bool -> Eff es ()
commitMemory cfg message allowEmpty = do
  runGit cfg ["add", "--all"]
  changed <- not <$> gitSucceeds cfg ["diff", "--cached", "--quiet"]
  when (allowEmpty || changed) $
    runGit cfg
      [ "-c", "user.name=Cosmobot"
      , "-c", "user.email=cosmobot@localhost"
      , "commit", "--quiet", "--allow-empty", "-m", message
      ]

runGit :: (IOE :> es, Process :> es) => MemoryConfig -> [String] -> Eff es ()
runGit cfg args = do
  (exitCode, _, stderrText) <- git cfg args
  unless (exitCode == ExitSuccess) $
    throwIO $ userError $ "git " <> List.intercalate " " args <> " failed: " <> stderrText

gitSucceeds :: Process :> es => MemoryConfig -> [String] -> Eff es Bool
gitSucceeds cfg args = do
  (exitCode, _, _) <- git cfg args
  pure (exitCode == ExitSuccess)

git :: Process :> es => MemoryConfig -> [String] -> Eff es (ExitCode, String, String)
git cfg args =
  readCreateProcessWithExitCode (proc "git" (["-C", cfg.dir] <> args)) ""

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
