{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Bot.Chat.Driver.QQ.Types
Description : QQ driver configuration and OneBot wire types
Stability   : experimental
-}

module Bot.Chat.Driver.QQ.Types where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Encoding as TextEncoding

data Config = Config
  { host  :: !String
  , port  :: !Int
  , path  :: !String
  , token :: !(Maybe Text)
  , botQQ :: !(Maybe Integer)
  , allowedGroups :: ![Integer]
  , allowedUsers :: ![Integer]
  , superusers :: ![Integer]
  }
  deriving (Show)

data Event = Event
  { time        :: !(Maybe Integer)
  , selfId      :: !(Maybe Integer)
  , postType    :: !Text
  , messageType :: !(Maybe Text)
  , subType     :: !(Maybe Text)
  , messageId   :: !(Maybe Integer)
  , userId      :: !(Maybe Integer)
  , groupId     :: !(Maybe Integer)
  , message     :: !(Maybe Aeson.Value)
  , rawMessage  :: !(Maybe Text)
  , sender      :: !(Maybe Aeson.Value)
  , rawEvent    :: !Aeson.Value
  }
  deriving (Show)

instance Aeson.FromJSON Event where
  parseJSON rawEvent = Aeson.withObject "OneBotEvent" parse rawEvent
    where
      parse o = do
        time <- o Aeson..:? "time"
        selfId <- o Aeson..:? "self_id"
        postType <- o Aeson..: "post_type"
        messageType <- o Aeson..:? "message_type"
        subType <- o Aeson..:? "sub_type"
        messageId <- o Aeson..:? "message_id"
        userId <- o Aeson..:? "user_id"
        groupId <- o Aeson..:? "group_id"
        message <- o Aeson..:? "message"
        rawMessage <- o Aeson..:? "raw_message"
        sender <- o Aeson..:? "sender"
        pure Event{..}

data ActionResponse = ActionResponse
  { status  :: !(Maybe Text)
  , retcode :: !(Maybe Integer)
  , data_   :: !(Maybe Aeson.Value)
  , message :: !(Maybe Text)
  , echo    :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance Aeson.FromJSON ActionResponse where
  parseJSON = Aeson.withObject "ActionResponse" $ \o -> do
    status <- o Aeson..:? "status"
    retcode <- o Aeson..:? "retcode"
    data_ <- o Aeson..:? "data"
    message <- o Aeson..:? "message"
    echo <- parseEcho o
    pure ActionResponse{..}
    where
      parseEcho o =
        (o Aeson..:? "echo" :: Aeson.Parser (Maybe Text)) >>= \case
          Just value -> pure (Just value)
          Nothing -> do
            raw <- o Aeson..:? "echo"
            pure (TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode <$> (raw :: Maybe Aeson.Value))

responseMessageId :: ActionResponse -> Maybe Integer
responseMessageId response =
  response.data_ >>= \case
    Aeson.Object obj -> case KeyMap.lookup "message_id" obj of
      Just value -> Aeson.parseMaybe Aeson.parseJSON value
      Nothing    -> Nothing
    _ -> Nothing
