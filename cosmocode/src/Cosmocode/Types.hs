{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Cosmocode.Types
  ( Options (..)
  , Command (..)
  , ConnectionStatus (..)
  , SessionMessage (..)
  , Activity (..)
  , ServerEvent (..)
  , Model (..)
  , initialModel
  , applyServerEvent
  ) where

import qualified Data.Aeson as Aeson
import Data.List (findIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)

data Command = NewSession | ResumeSession !Text
  deriving (Eq, Show)

data Options = Options
  { host :: !String
  , port :: !Int
  , token :: !Text
  , command :: !Command
  }
  deriving (Eq, Show)

data ConnectionStatus = Connected | Disconnected !Text | RpcFailed !Text
  deriving (Eq, Show)

data SessionMessage = SessionMessage
  { sessionId :: !Text
  , messageId :: !Text
  , sender :: !Text
  , text :: !Text
  }
  deriving (Eq, Show, Generic)

instance Aeson.FromJSON SessionMessage where
  parseJSON = Aeson.withObject "session message" \o ->
    SessionMessage <$> o Aeson..: "sessionId" <*> o Aeson..: "messageId"
      <*> o Aeson..: "sender" <*> o Aeson..: "text"

data ServerEvent
  = MessageReceived !SessionMessage
  | MessageUpdated !Text !Text !Text
  | MessageDone !Text !Text
  | ActivityChanged !Text !Activity
  | RequestFailed !Text
  | ConnectionClosed !Text
  deriving (Eq, Show)

data Activity
  = ReasoningStarted !Text !Int
  | ReasoningFinished !Text !Int !Text
  | ToolCallStarted !Text !Int !Text !Text
  | ToolCallFinished !Text !Int !Text !Text !Text
  deriving (Eq, Show)

data Model = Model
  { server :: !Text
  , sessionId :: !Text
  , status :: !ConnectionStatus
  , messages :: ![SessionMessage]
  , completedMessages :: ![Text]
  , reasoning :: !(Maybe (Text, Int))
  , activeTools :: !(Map Text Text)
  }
  deriving (Eq, Show)

initialModel :: Text -> Text -> [SessionMessage] -> Model
initialModel server sessionId messages = Model
  { server
  , sessionId
  , status = Connected
  , messages
  , completedMessages = map (.messageId) messages
  , reasoning = Nothing
  , activeTools = Map.empty
  }

applyServerEvent :: ServerEvent -> Model -> Model
applyServerEvent event model = case event of
  MessageReceived message
    | message.sessionId == model.sessionId -> model{messages = upsertMessage message model.messages}
    | otherwise -> model
  MessageUpdated sessionId messageId body
    | sessionId == model.sessionId -> model{messages = updateMessage messageId body model.messages}
    | otherwise -> model
  MessageDone sessionId messageId
    | sessionId == model.sessionId -> model{completedMessages = insertOnce messageId model.completedMessages}
    | otherwise -> model
  ActivityChanged sessionId activity
    | sessionId == model.sessionId -> applyActivity activity model
    | otherwise -> model
  RequestFailed err -> model{status = RpcFailed err}
  ConnectionClosed reason -> model{status = Disconnected reason}

applyActivity :: Activity -> Model -> Model
applyActivity activity model = case activity of
  ReasoningStarted runId turn -> model{reasoning = Just (runId, turn)}
  ReasoningFinished{} -> model{reasoning = Nothing}
  ToolCallStarted _ _ toolCallId toolName -> model{activeTools = Map.insert toolCallId toolName model.activeTools}
  ToolCallFinished _ _ toolCallId _ _ -> model{activeTools = Map.delete toolCallId model.activeTools}

insertOnce :: Eq a => a -> [a] -> [a]
insertOnce value values = values <> [value | value `notElem` values]

upsertMessage :: SessionMessage -> [SessionMessage] -> [SessionMessage]
upsertMessage message messages = case findIndex ((== message.messageId) . (.messageId)) messages of
  Nothing -> messages <> [message]
  Just index -> take index messages <> [message] <> drop (index + 1) messages

updateMessage :: Text -> Text -> [SessionMessage] -> [SessionMessage]
updateMessage target body = map \message ->
  if message.messageId == target then message{text = body} else message
