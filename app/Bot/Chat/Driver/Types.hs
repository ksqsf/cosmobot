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
import qualified Bot.Effect.Chat as Chat
import Bot.Prelude
import qualified Data.Aeson as Aeson

data ChatPlatformDriver es = ChatPlatformDriver
  { platform :: !ChatPlatform
  , replyTo :: IncomingMessage -> Text -> Eff es (Maybe Integer)
  , editMessage :: IncomingMessage -> Integer -> Text -> Eff es Bool
  , replyStreamStyle :: IncomingMessage -> Eff es Chat.ReplyStreamStyle
  , getMessageContent :: IncomingMessage -> Integer -> Eff es (Maybe ReferencedMessage)
  , getSenderMemberInfo :: IncomingMessage -> Eff es (Maybe Aeson.Value)
  , getMemberInfo :: IncomingMessage -> Integer -> Eff es (Maybe Aeson.Value)
  , getUserAvatar :: IncomingMessage -> Integer -> Eff es (Maybe Aeson.Value)
  , listGroupMembers :: IncomingMessage -> Eff es (Maybe Aeson.Value)
  , mentionUser :: IncomingMessage -> Integer -> Text -> Eff es (Maybe Integer)
  }
