{-|
Module      : Bot.Memory
Description : Per-sender and per-chat structural memory files
Stability   : experimental
-}

module Bot.Memory
  ( MemoryConfig (..)
  , MemoryScope (..)
  , MemoryCommitMessage
  , memoryCommitMessage
  , MemoryEntry (..)
  , MemoryRevision (..)
  , MemoryHistoryEntry (..)
  , memoryLimitChars
  , senderMemoryScope
  , chatMemoryScope
  , loadMemory
  , listMemories
  , memoryHistory
  , loadMemoryRevision
  , revertMemory
  , replaceMemory
  , clearMemory
  , initializeMemoryRepo
  , memoryPath
  )
where

import Bot.Core.Message
import Bot.Prelude
import Data.Char (isControl)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.List as List
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.FileSystem.IO.ByteString as FileSystemByteString
import qualified Effectful.FileSystem.IO.File as FileSystemFile
import Effectful.Process (Process, proc, readCreateProcessWithExitCode)
import System.Exit (ExitCode (..))
import System.FilePath

data MemoryException = MemoryGitFailed ![Text] !Text
  deriving (Eq, Show)

instance Exception MemoryException where
  displayException (MemoryGitFailed args stderrText) =
    Text.unpack [i|git #{Text.unwords args} failed: #{stderrText}|]

-- | Filesystem-backed memory settings.
newtype MemoryConfig = MemoryConfig
  { dir :: FilePath
  }
  deriving (Show)

data MemoryScope
  = SenderMemory !ChatPlatform !Text
  | ChatMemory !ChatPlatform !Integer
  deriving (Eq, Show)

newtype MemoryCommitMessage = MemoryCommitMessage Text
  deriving (Eq, Show)

memoryCommitMessage :: Text -> Either Text MemoryCommitMessage
memoryCommitMessage raw
  | Text.null message = Left "message must not be empty"
  | Text.length message > 72 = Left "message must not exceed 72 characters"
  | Text.any (`elem` ['\r', '\n']) message = Left "message must be a single line"
  | Text.any isControl message = Left "message must not contain control characters"
  | otherwise = Right (MemoryCommitMessage message)
  where
    message = Text.strip raw

data MemoryEntry = MemoryEntry
  { scope :: !MemoryScope
  , content :: !Text
  }
  deriving (Eq, Show)

newtype MemoryRevision = MemoryRevision
  { value :: Text
  }
  deriving (Eq, Show)

data MemoryHistoryEntry = MemoryHistoryEntry
  { revision :: !MemoryRevision
  , committedAt :: !Text
  , subject :: !Text
  }
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

loadMemory :: FileSystem :> es => MemoryConfig -> MemoryScope -> Eff es (Maybe Text)
loadMemory cfg scope = do
  exists <- FileSystem.doesFileExist path
  if exists
    then nonEmptyMemory . TextEncoding.decodeUtf8 <$> FileSystemByteString.readFile path
    else pure Nothing
  where
    path = memoryPath cfg scope

listMemories :: FileSystem :> es => MemoryConfig -> Eff es [MemoryEntry]
listMemories cfg =
  concat <$> traverse listPlatformMemories platforms
  where
    platforms = [PlatformQQ, PlatformTelegram, PlatformMatrix, PlatformDiscord, PlatformRPC, PlatformACP]
    listPlatformMemories platform =
      (<>) <$> listScope platform "sender" senderScope <*> listScope platform "chat" chatScope
    listScope platform kind scopeFromId = do
      let dir = cfg.dir </> platformPathPart platform </> kind
      exists <- FileSystem.doesDirectoryExist dir
      if not exists
        then pure []
        else do
          names <- List.sort <$> FileSystem.listDirectory dir
          fmap catMaybes $ forM names \name -> do
            let path = dir </> name
            isFile <- FileSystem.doesFileExist path
            case scopeFromId platform (Text.pack (dropExtension name)) of
              Nothing -> pure Nothing
              Just scope | isFile && takeExtension name == ".md" ->
                fmap (MemoryEntry scope) <$> loadMemory cfg scope
              Just _ -> pure Nothing
    senderScope platform = Just . SenderMemory platform
    chatScope platform = fmap (ChatMemory platform) . readMaybe . Text.unpack

memoryHistory :: Process :> es => MemoryConfig -> MemoryScope -> Eff es [MemoryHistoryEntry]
memoryHistory cfg scope = do
  let args = ["log", "--format=%H%x09%cI%x09%s", "--", makeRelative cfg.dir (memoryPath cfg scope)]
  (exitCode, stdoutText, stderrText) <- git cfg args
  unless (exitCode == ExitSuccess) $
    throwIO (MemoryGitFailed (map Text.pack args) (Text.pack stderrText))
  pure (mapMaybe parseHistoryLine (Text.lines (Text.pack stdoutText)))

loadMemoryRevision :: Process :> es => MemoryConfig -> MemoryScope -> MemoryRevision -> Eff es (Maybe Text)
loadMemoryRevision cfg scope revision = do
  validRevision <- gitSucceeds cfg ["cat-file", "-e", Text.unpack revision.value <> "^{commit}"]
  unless validRevision $
    throwIO (MemoryGitFailed ["cat-file", "-e", revision.value <> "^{commit}"] "unknown memory revision")
  let object = Text.unpack revision.value <> ":" <> makeRelative cfg.dir (memoryPath cfg scope)
  exists <- gitSucceeds cfg ["cat-file", "-e", object]
  if not exists
    then pure Nothing
    else do
      (exitCode, stdoutText, stderrText) <- git cfg ["show", "--no-ext-diff", object]
      unless (exitCode == ExitSuccess) $
        throwIO (MemoryGitFailed ["show", Text.pack object] (Text.pack stderrText))
      pure (nonEmptyMemory (Text.pack stdoutText))

revertMemory
  :: (FileSystem :> es, IOE :> es, Process :> es)
  => MemoryConfig
  -> MemoryScope
  -> MemoryRevision
  -> Eff es ()
revertMemory cfg scope revision =
  loadMemoryRevision cfg scope revision >>= \case
    Nothing -> clearMemory cfg scope revertMessage
    Just content -> replaceMemory cfg scope revertMessage content
  where
    revertMessage = MemoryCommitMessage ("Revert memory to " <> Text.take 8 revision.value)

parseHistoryLine :: Text -> Maybe MemoryHistoryEntry
parseHistoryLine line =
  case Text.splitOn "\t" line of
    revision : committedAt : subjectParts ->
      Just MemoryHistoryEntry
        { revision = MemoryRevision revision
        , committedAt
        , subject = Text.intercalate "\t" subjectParts
        }
    _ -> Nothing

replaceMemory :: (FileSystem :> es, IOE :> es, Process :> es) => MemoryConfig -> MemoryScope -> MemoryCommitMessage -> Text -> Eff es ()
replaceMemory cfg scope message memory =
  updateMemory cfg scope message $ \path -> do
    FileSystem.createDirectoryIfMissing True (takeDirectory path)
    FileSystemFile.writeBinaryFileAtomic path (TextEncoding.encodeUtf8 (Text.strip memory))

clearMemory :: (FileSystem :> es, IOE :> es, Process :> es) => MemoryConfig -> MemoryScope -> MemoryCommitMessage -> Eff es ()
clearMemory cfg scope message =
  updateMemory cfg scope message removeMemoryFile

initializeMemoryRepo :: (FileSystem :> es, IOE :> es, Process :> es) => MemoryConfig -> Eff es ()
initializeMemoryRepo cfg = do
  FileSystem.createDirectoryIfMissing True cfg.dir
  runGit cfg ["init", "--quiet"]
  hasHead <- gitSucceeds cfg ["rev-parse", "--verify", "HEAD"]
  commitMemory cfg (if hasHead then "Update memory" else "Initialize memory") (not hasHead)

updateMemory
  :: (FileSystem :> es, IOE :> es, Process :> es)
  => MemoryConfig
  -> MemoryScope
  -> MemoryCommitMessage
  -> (FilePath -> Eff es ())
  -> Eff es ()
updateMemory cfg scope message update = mask \restore -> do
  previous <- readMemoryFile path
  restore (update path >> commitMemoryPath cfg path message)
    `onException` rollbackMemoryFile cfg path previous
  where
    path = memoryPath cfg scope

readMemoryFile :: FileSystem :> es => FilePath -> Eff es (Maybe ByteString)
readMemoryFile path = do
  exists <- FileSystem.doesFileExist path
  if exists
    then Just <$> FileSystemByteString.readFile path
    else pure Nothing

rollbackMemoryFile
  :: (FileSystem :> es, IOE :> es, Process :> es)
  => MemoryConfig
  -> FilePath
  -> Maybe ByteString
  -> Eff es ()
rollbackMemoryFile cfg path previous = do
  case previous of
    Nothing -> removeMemoryFile path
    Just contents -> FileSystemFile.writeBinaryFileAtomic path contents
  runGit cfg ["reset", "--quiet", "HEAD", "--", makeRelative cfg.dir path]

removeMemoryFile :: FileSystem :> es => FilePath -> Eff es ()
removeMemoryFile path = do
  exists <- FileSystem.doesFileExist path
  when exists (FileSystem.removeFile path)

commitMemoryPath :: (IOE :> es, Process :> es) => MemoryConfig -> FilePath -> MemoryCommitMessage -> Eff es ()
commitMemoryPath cfg path (MemoryCommitMessage message) = do
  stageMemoryPath cfg path
  changed <- not <$> gitSucceeds cfg ["diff", "--cached", "--quiet", "--", makeRelative cfg.dir path]
  when changed $
    runGit cfg
      [ "-c", "user.name=Cosmobot"
      , "-c", "user.email=cosmobot@localhost"
      , "commit", "--quiet", "-m", Text.unpack message
      ]

stageMemoryPath :: (IOE :> es, Process :> es) => MemoryConfig -> FilePath -> Eff es ()
stageMemoryPath cfg path =
  runGit cfg ["add", "--all", "--", makeRelative cfg.dir path]

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
    throwIO (MemoryGitFailed (map Text.pack args) (Text.pack stderrText))

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
