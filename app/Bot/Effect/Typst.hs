{-|
Module      : Bot.Effect.Typst
Description : Typst document rendering capability
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bot.Effect.Typst
  ( Typst
  , withTypstPng
  , runTypst
  , runTypstWith
  )
where

import Bot.Prelude
import qualified Bot.Util.Image as Image
import qualified Data.Text.IO as TextIO
import System.Directory (removeDirectoryRecursive)
import System.Exit
import System.FilePath
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.IO.Error (userError)
import System.Process

-- | Render Typst source into a PNG available for the duration of a continuation.
data Typst :: Effect where
  WithTypstPng
    :: Text
    -> (FilePath -> m a)
    -> Typst m a

type instance DispatchOf Typst = Dynamic

withTypstPng :: Typst :> es => Text -> (FilePath -> Eff es a) -> Eff es a
withTypstPng source action =
  send (WithTypstPng source action)

runTypst
  :: (IOE :> es, Log :> es)
  => Eff (Typst : es) a
  -> Eff es a
runTypst = interpret $ \localEnv operation ->
  localSeqUnlift localEnv \runLocal ->
    case operation of
      WithTypstPng source action ->
        withRenderedTypstPng source (runLocal . action)

runTypstWith
  :: (forall r. Text -> (FilePath -> Eff es r) -> Eff es r)
  -> Eff (Typst : es) a
  -> Eff es a
runTypstWith render = interpret $ \localEnv operation ->
  localSeqUnlift localEnv \runLocal ->
  case operation of
    WithTypstPng source action ->
      render source (runLocal . action)

withRenderedTypstPng
  :: (IOE :> es, Log :> es)
  => Text
  -> (FilePath -> Eff es a)
  -> Eff es a
withRenderedTypstPng source action =
  bracket acquire release \dir -> do
    imagePath <- renderTypstPng dir source
    action imagePath
  where
    acquire = liftIO do
      tmp <- getCanonicalTemporaryDirectory
      createTempDirectory tmp "cosmobot-typst-"
    release =
      liftIO . removeDirectoryRecursive

renderTypstPng :: (IOE :> es, Log :> es) => FilePath -> Text -> Eff es FilePath
renderTypstPng dir source = do
  let typstPath = dir </> "document" <.> "typ"
      pngPath = dir </> "document" <.> "png"
  liftIO $ TextIO.writeFile typstPath source
  logInfo_ [i|Rendering Typst document: #{typstPath}|]
  (code, _out, err) <- liftIO $
    readProcessWithExitCode "typst" ["compile", "--root", dir, typstPath, pngPath] ""
  case code of
    ExitSuccess -> do
      logInfo_ [i|Rendered Typst PNG: #{pngPath}|]
      pure pngPath
    ExitFailure _ -> do
      liftIO $ Image.removeFilesIfExists [typstPath, pngPath]
      throwIO (userError ("typst failed: " <> err))
