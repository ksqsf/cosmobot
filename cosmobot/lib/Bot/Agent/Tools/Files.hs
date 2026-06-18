{-|
Module      : Bot.Agent.Tools.Files
Description : Agent filesystem tools
Stability   : experimental
-}

module Bot.Agent.Tools.Files
  ( listDirectoryTool
  , readFileTool
  , acpReadClientFileTool
  , acpWriteClientFileTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Core.Message
import qualified Bot.Effect.ACP as ACP
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
import System.FilePath
import System.IO.Error (userError)

listDirectoryTool :: IOE :> es => Tool es
listDirectoryTool = Tool
  { name = "list_files"
  , description = "List files and directories under a path inside the bot working directory."
  , parameters = objectSchema
      [ fieldText "path" "Directory path relative to the bot working directory. Use \".\" for the working directory."
      ]
      ["path"]
  , noisy = False
  , allowed = superuserOnly
  , start = \_ -> pure \args -> withTextArg "path" (\path -> do
      target <- resolveSafePath path
      isDir <- liftIO (doesDirectoryExist target)
      if not isDir
        then pure (toolText "Not a directory.")
        else do
          entries <- liftIO (listDirectory target)
          pure (toolText (jsonText entries))
      ) args
  }

readFileTool :: IOE :> es => Tool es
readFileTool = Tool
  { name = "read_file"
  , description = "Read a UTF-8 text file inside the bot working directory."
  , parameters = objectSchema
      [ fieldText "path" "File path relative to the bot working directory."
      ]
      ["path"]
  , noisy = False
  , allowed = superuserOnly
  , start = \_ -> pure \args -> withTextArg "path" (\path -> do
      target <- resolveSafePath path
      isFile <- liftIO (doesFileExist target)
      if not isFile
        then pure (toolText "Not a file.")
        else toolText <$> liftIO (Text.readFile target)
      ) args
  }

acpReadClientFileTool :: ACP.ACP :> es => Tool es
acpReadClientFileTool = Tool
  { name = "acp_read_client_file"
  , description = "Read a UTF-8 text file from the connected ACP client workspace. Relative paths are resolved against the ACP session cwd."
  , parameters = objectSchema
      [ fieldText "path" "Absolute file path, or a path relative to the ACP session cwd."
      , fieldInteger "line" "Optional 1-based starting line."
      , fieldInteger "limit" "Optional maximum number of lines to read."
      ]
      ["path"]
  , noisy = False
  , allowed = acpOnly
  , start = \context -> pure \args ->
      withParsedToolArgs parseReadClientFileArgs args \ReadClientFileArgs{path, line, limit} ->
        ACP.readClientFile context.message path line limit >>= \case
          Left err ->
            pure (clientFileFailure err)
          Right content ->
            pure (toolText content)
  }

acpWriteClientFileTool :: ACP.ACP :> es => Tool es
acpWriteClientFileTool = Tool
  { name = "acp_write_client_file"
  , description = "Write a UTF-8 text file in the connected ACP client workspace. Relative paths are resolved against the ACP session cwd."
  , parameters = objectSchema
      [ fieldText "path" "Absolute file path, or a path relative to the ACP session cwd."
      , fieldText "content" "Complete UTF-8 text content to write."
      ]
      ["path", "content"]
  , noisy = False
  , allowed = acpOnly
  , start = \context -> pure \args ->
      withParsedToolArgs parseWriteClientFileArgs args \WriteClientFileArgs{path, content} ->
        ACP.writeClientFile context.message path content >>= \case
          Left err ->
            pure (clientFileFailure err)
          Right () ->
            pure (toolText "File written.")
  }

data ReadClientFileArgs = ReadClientFileArgs
  { path :: !Text
  , line :: !(Maybe Int)
  , limit :: !(Maybe Int)
  }

data WriteClientFileArgs = WriteClientFileArgs
  { path :: !Text
  , content :: !Text
  }

parseReadClientFileArgs :: Aeson.Value -> AesonTypes.Parser ReadClientFileArgs
parseReadClientFileArgs =
  Aeson.withObject "acp_read_client_file arguments" \o ->
    ReadClientFileArgs
      <$> o Aeson..: Key.fromText "path"
      <*> o Aeson..:? Key.fromText "line"
      <*> o Aeson..:? Key.fromText "limit"

parseWriteClientFileArgs :: Aeson.Value -> AesonTypes.Parser WriteClientFileArgs
parseWriteClientFileArgs =
  Aeson.withObject "acp_write_client_file arguments" \o ->
    WriteClientFileArgs
      <$> o Aeson..: Key.fromText "path"
      <*> o Aeson..: Key.fromText "content"

acpOnly :: AgentContext es -> Bool
acpOnly context =
  context.message.platform == PlatformACP

clientFileFailure :: Text -> ToolResult
clientFileFailure err =
  toolFailure (permanentArgumentFailure err err).failure

resolveSafePath :: IOE :> es => Text -> Eff es FilePath
resolveSafePath rawPath = do
  cwd <- liftIO getCurrentDirectory
  target <- liftIO (canonicalizePath (cwd </> Text.unpack rawPath))
  unless (cwd `isEqualOrParentOf` target) do
    throwIO (userError "Path escapes the bot working directory.")
  pure target

isEqualOrParentOf :: FilePath -> FilePath -> Bool
isEqualOrParentOf parent child =
  parent == child || addTrailingPathSeparator parent `isPrefixOf` child
