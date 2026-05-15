{-|
Module      : Bot.Agent.Tools.Files
Description : Agent filesystem tools
Stability   : experimental
-}

module Bot.Agent.Tools.Files
  ( listDirectoryTool
  , readFileTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import Bot.Prelude
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Directory
import System.FilePath
import System.IO.Error (userError)

listDirectoryTool :: IOE :> es => Tool es
listDirectoryTool = Tool
  { name = "list_directory"
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
