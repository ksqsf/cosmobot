{-|
Module      : Bot.System.Typst.CLI
Description : Typst CLI-backed renderer
Stability   : experimental
-}

module Bot.System.Typst.CLI
  ( runTypst
  , withRenderedTypst
  , TypstOutputFormat
  )
where

import Bot.Prelude
import Bot.System.Typst.Types
import qualified Bot.Effect.Typst as Typst
import qualified Bot.Util.Image as Image
import qualified Data.Text.IO as TextIO
import Effectful.FileSystem (FileSystem)
import Effectful.Process
import System.Directory (removeDirectoryRecursive)
import System.Exit
import System.FilePath
import System.IO.Error (userError)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

runTypst
  :: (IOE :> es, KatipE :> es, Fail :> es, FileSystem :> es, Process :> es)
  => Eff (Typst.Typst : es) a
  -> Eff es a
runTypst = interpret $ \localEnv operation ->
  localSeqUnlift localEnv \runLocal ->
    case operation of
      Typst.WithTypst format source action ->
        withRenderedTypst format source (runLocal . action)

withRenderedTypst
  :: (IOE :> es, KatipE :> es, Fail :> es, FileSystem :> es, Process :> es)
  => TypstOutputFormat
  -> Text
  -> (FilePath -> Eff es a)
  -> Eff es a
withRenderedTypst format source action =
  bracket acquire release \dir -> do
    imagePath <- renderTypst format dir source
    action imagePath
  where
    acquire = liftIO do
      tmp <- getCanonicalTemporaryDirectory
      createTempDirectory tmp "cosmobot-typst-"
    release =
      liftIO . removeDirectoryRecursive

renderTypst :: (IOE :> es, KatipE :> es, Fail :> es, FileSystem :> es, Process :> es) => TypstOutputFormat -> FilePath -> Text -> Eff es FilePath
renderTypst format dir source = do
  let extName = typstFormatToExtName format
      typstPath = dir </> "document" <.> "typ"
      outputPath = dir </> "document" <.> toString extName
  liftIO $ TextIO.writeFile typstPath source
  logInfo [i|Rendering Typst document: #{typstPath}|]
  (code, _out, err) <-
    readProcessWithExitCode "typst" ["compile", "--root", dir, typstPath, outputPath] ""
  case code of
    ExitSuccess -> do
      logInfo [i|Rendered Typst document: #{outputPath}|]
      pure outputPath
    ExitFailure _ -> do
      Image.removeFilesIfExists [typstPath, outputPath]
      throwIO (userError ("typst failed: " <> err))
