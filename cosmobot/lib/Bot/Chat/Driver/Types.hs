{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Bot.Chat.Driver.Types
Description : Shared chat driver adapter types
Stability   : experimental
-}

module Bot.Chat.Driver.Types
  ( ChatDriver (..)
  )
where

import Bot.Core.Message
import qualified Bot.Chat.Types as Chat
import Bot.Prelude
import qualified Data.Aeson as Aeson

class ChatDriver driver where
  type ChatDriverEffects driver (es :: [Effect]) :: Constraint
  type ChatDriverEffects driver es = ()

  driverPlatform :: driver -> ChatPlatform

  sendReplyMessage :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Eff es (Either Text MessageId)
  sendReplyMessage driver _ _ =
    pure (Left [i|#{driverPlatform driver} does not support replies.|])

  replyAudio :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Maybe Text -> Eff es (Either Text MessageId)
  replyAudio driver _ _ _ =
    pure (Left [i|#{driverPlatform driver} does not support audio replies.|])

  uploadFile :: ChatDriverEffects driver es => driver -> IncomingMessage -> FilePath -> Eff es (Either Text MessageId)
  uploadFile driver _ _ =
    pure (Left [i|#{driverPlatform driver} does not support file uploads.|])

  editMessage :: ChatDriverEffects driver es => driver -> IncomingMessage -> MessageId -> Text -> Eff es Bool
  editMessage _ _ _ _ =
    pure False

  completeMessageEdit :: ChatDriverEffects driver es => driver -> IncomingMessage -> MessageId -> Eff es Bool
  completeMessageEdit _ _ _ =
    pure True

  deleteMessage :: ChatDriverEffects driver es => driver -> IncomingMessage -> MessageId -> Eff es Bool
  deleteMessage _ _ _ =
    pure False

  messageOutPolicy :: ChatDriverEffects driver es => driver -> IncomingMessage -> Eff es Chat.MessageOutPolicy
  messageOutPolicy _ _ =
    pure (Chat.ChunkedMessage 4000)

  getMessageContent :: ChatDriverEffects driver es => driver -> IncomingMessage -> MessageId -> Eff es (Maybe ReferencedMessage)
  getMessageContent _ _ _ =
    pure Nothing

  getSenderMemberInfo :: ChatDriverEffects driver es => driver -> IncomingMessage -> Eff es (Maybe Aeson.Value)
  getSenderMemberInfo _ _ =
    pure Nothing

  getMemberInfo :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
  getMemberInfo _ _ _ =
    pure Nothing

  getUserAvatar :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Eff es (Maybe Aeson.Value)
  getUserAvatar _ _ _ =
    pure Nothing

  listGroupMembers :: ChatDriverEffects driver es => driver -> IncomingMessage -> Eff es (Maybe Aeson.Value)
  listGroupMembers _ _ =
    pure Nothing

  normalizeMediaRef :: ChatDriverEffects driver es => driver -> Text -> Eff es Text
  normalizeMediaRef _ =
    pure

  mentionUser :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Text -> Eff es (Either Text MessageId)
  mentionUser driver message _ body =
    sendReplyMessage driver message body

  setMemberTitle :: ChatDriverEffects driver es => driver -> IncomingMessage -> Text -> Text -> Eff es Bool
  setMemberTitle _ _ _ _ =
    pure False

  setTyping :: ChatDriverEffects driver es => driver -> IncomingMessage -> Int -> Eff es ()
  setTyping _ _ _ =
    pure ()
