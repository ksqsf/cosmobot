{-|
Module      : Bot.Chat.Driver.Types
Description : Shared chat driver adapter types
Stability   : experimental
-}

module Bot.Chat.Driver.Types
  ( ChatPlatformDriver (..)
  )
where

import Bot.Core.Message
import qualified Bot.Chat.Types as Chat
import Bot.Prelude
import qualified Data.Aeson as Aeson

data ChatPlatformDriver es = ChatPlatformDriver
  { platform :: !ChatPlatform
  , replyTo :: IncomingMessage -> Text -> Eff es (Maybe MessageId)
  , replyAudio :: IncomingMessage -> Text -> Maybe Text -> Eff es (Either Text (Maybe MessageId))
  , uploadFile :: IncomingMessage -> FilePath -> Eff es (Either Text (Maybe MessageId))
  , editMessage :: IncomingMessage -> MessageId -> Text -> Eff es Bool
  , deleteMessage :: IncomingMessage -> MessageId -> Eff es Bool
  , replyStreamStyle :: IncomingMessage -> Eff es Chat.ReplyStreamStyle
  , getMessageContent :: IncomingMessage -> MessageId -> Eff es (Maybe ReferencedMessage)
  , getSenderMemberInfo :: IncomingMessage -> Eff es (Maybe Aeson.Value)
  , getMemberInfo :: IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
  , getUserAvatar :: IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
  , listGroupMembers :: IncomingMessage -> Eff es (Maybe Aeson.Value)
  , normalizeMediaRef :: Text -> Eff es Text
  , mentionUser :: IncomingMessage -> Text -> Text -> Eff es (Maybe MessageId)
  , setMemberTitle :: IncomingMessage -> Text -> Text -> Eff es Bool
  }
