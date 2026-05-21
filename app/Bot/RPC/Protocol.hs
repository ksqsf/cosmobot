{-|
Module      : Bot.RPC.Protocol
Description : JSON-RPC envelope types used by the local websocket service
Stability   : experimental
-}

module Bot.RPC.Protocol
  ( RpcRequest (..)
  , RpcResponse (..)
  , RpcError (..)
  , RpcNotification (..)
  , successResponse
  , errorResponse
  , notification
  )
where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap

data RpcRequest = RpcRequest
  { id :: !Text
  , method :: !Text
  , params :: !Aeson.Value
  }
  deriving (Eq, Show)

instance Aeson.FromJSON RpcRequest where
  parseJSON = Aeson.withObject "RpcRequest" \o ->
    RpcRequest
      <$> o Aeson..: "id"
      <*> o Aeson..: "method"
      <*> pure (fromMaybe (Aeson.Object KeyMap.empty) (KeyMap.lookup "params" o))

data RpcResponse = RpcResponse
  { id :: !Text
  , ok :: !Bool
  , result :: !(Maybe Aeson.Value)
  , error :: !(Maybe RpcError)
  }
  deriving (Eq, Show)

instance Aeson.ToJSON RpcResponse where
  toJSON response =
    Aeson.object $
      [ "id" Aeson..= response.id
      , "ok" Aeson..= response.ok
      ]
      <> maybe [] (\value -> ["result" Aeson..= value]) response.result
      <> maybe [] (\value -> ["error" Aeson..= value]) response.error

data RpcError = RpcError
  { code :: !Text
  , message :: !Text
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data RpcNotification = RpcNotification
  { method :: !Text
  , params :: !Aeson.Value
  }
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

successResponse :: Aeson.ToJSON a => Text -> a -> RpcResponse
successResponse requestId value =
  RpcResponse
    { id = requestId
    , ok = True
    , result = Just (Aeson.toJSON value)
    , error = Nothing
    }

errorResponse :: Text -> Text -> Text -> RpcResponse
errorResponse requestId code message =
  RpcResponse
    { id = requestId
    , ok = False
    , result = Nothing
    , error = Just RpcError{code, message}
    }

notification :: Aeson.ToJSON a => Text -> a -> RpcNotification
notification method value =
  RpcNotification
    { method
    , params = Aeson.toJSON value
    }
