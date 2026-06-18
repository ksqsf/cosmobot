{-|
Module      : Bot.ACP.Client
Description : ACP client capability interpreter
Stability   : experimental
-}

module Bot.ACP.Client
  ( runACP
  )
where

import qualified Bot.ACP.State as State
import Bot.Core.Message
import qualified Bot.Effect.ACP as ACP
import qualified Bot.Effect.Storage as Storage
import Bot.Prelude
import qualified Bot.Session as Session
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import System.FilePath ((</>), isAbsolute)

runACP :: (Concurrent :> es, IOE :> es, Storage.Storage :> es) => State.AcpState -> Eff (ACP.ACP : es) a -> Eff es a
runACP acpState =
  interpret \_ -> \case
    ACP.ReadClientFile message path line limit ->
      clientFileRequest acpState message "fs/read_text_file" (.readTextFile) (readTextFileParams path line limit) parseReadTextFileResult
    ACP.WriteClientFile message path content ->
      clientFileRequest acpState message "fs/write_text_file" (.writeTextFile) (writeTextFileParams path content) parseWriteTextFileResult

clientFileRequest
  :: (Concurrent :> es, IOE :> es, Storage.Storage :> es)
  => State.AcpState
  -> IncomingMessage
  -> Text
  -> (State.AcpClientCapabilities -> Bool)
  -> (Text -> Maybe Text -> Aeson.Value)
  -> (Aeson.Value -> AesonTypes.Parser a)
  -> Eff es (Either Text a)
clientFileRequest acpState message method supported params parseResult
  | message.platform /= PlatformACP =
      pure (Left "ACP client file tools are only available in ACP sessions.")
  | otherwise =
      case listToMaybe message.chatAliases of
        Nothing ->
          pure (Left "ACP session id is unavailable for this message.")
        Just sessionIdText ->
          sessionCwd (Session.SessionId sessionIdText) >>= \cwd ->
            State.requestSessionClient acpState (Session.SessionId sessionIdText) method supported (params sessionIdText cwd) >>= \case
            Left err ->
              pure (Left err)
            Right result ->
              pure (first Text.pack (AesonTypes.parseEither parseResult result))

sessionCwd :: Storage.Storage :> es => Session.SessionId -> Eff es (Maybe Text)
sessionCwd sessionId =
  Session.getSession sessionId <&> (>>= (.label))

readTextFileParams :: Text -> Maybe Int -> Maybe Int -> Text -> Maybe Text -> Aeson.Value
readTextFileParams path line limit sessionId cwd =
  Aeson.object $
    [ "sessionId" Aeson..= sessionId
    , "path" Aeson..= clientPath cwd path
    ]
      <> maybe [] (\value -> ["line" Aeson..= value]) line
      <> maybe [] (\value -> ["limit" Aeson..= value]) limit

writeTextFileParams :: Text -> Text -> Text -> Maybe Text -> Aeson.Value
writeTextFileParams path content sessionId cwd =
  Aeson.object
    [ "sessionId" Aeson..= sessionId
    , "path" Aeson..= clientPath cwd path
    , "content" Aeson..= content
    ]

clientPath :: Maybe Text -> Text -> Text
clientPath cwd rawPath
  | isAbsolute path =
      rawPath
  | Just cwdText <- cwd
  , isAbsolute (Text.unpack cwdText) =
      Text.pack (Text.unpack cwdText </> path)
  | otherwise =
      rawPath
  where
    path = Text.unpack rawPath

parseReadTextFileResult :: Aeson.Value -> AesonTypes.Parser Text
parseReadTextFileResult =
  Aeson.withObject "fs/read_text_file result" (Aeson..: "content")

parseWriteTextFileResult :: Aeson.Value -> AesonTypes.Parser ()
parseWriteTextFileResult _ =
  pure ()
