{-|
Module      : Bot.RPC.Protocol
Description : JSON-RPC envelope helpers used by the local websocket service
Stability   : experimental
-}

module Bot.RPC.Protocol
  ( RpcRequest
  , RpcResponse
  , RpcError
  , RpcNotification
  , RequestId
  , rpcRequest
  , requestId
  , requestMethod
  , requestParams
  , successResponse
  , errorResponse
  , parseErrorResponse
  , invalidRequestResponse
  , rpcError
  , notification
  )
where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified JSONRPC

type RpcRequest = JSONRPC.JSONRPCRequest
type RpcResponse = JSONRPC.JSONRPCMessage
type RpcError = JSONRPC.JSONRPCErrorInfo
type RpcNotification = JSONRPC.JSONRPCNotification
type RequestId = JSONRPC.RequestId

rpcRequest :: Text -> Aeson.Value -> Text -> RpcRequest
rpcRequest method params requestId_ =
  JSONRPC.JSONRPCRequest JSONRPC.rPC_VERSION (textRequestId requestId_) method params

requestId :: RpcRequest -> RequestId
requestId request =
  request.id

requestMethod :: RpcRequest -> Text
requestMethod request =
  request.method

requestParams :: RpcRequest -> Aeson.Value
requestParams request =
  request.params

successResponse :: Aeson.ToJSON a => RequestId -> a -> RpcResponse
successResponse responseId value =
  JSONRPC.ResponseMessage $
    JSONRPC.JSONRPCResponse JSONRPC.rPC_VERSION responseId (Aeson.toJSON value)

errorResponse :: RequestId -> Text -> Text -> RpcResponse
errorResponse responseId code message =
  JSONRPC.ErrorMessage $
    JSONRPC.JSONRPCError JSONRPC.rPC_VERSION responseId (rpcError code message)

parseErrorResponse :: Text -> RpcResponse
parseErrorResponse message =
  JSONRPC.ErrorMessage $
    JSONRPC.JSONRPCError JSONRPC.rPC_VERSION nullRequestId $
      JSONRPC.JSONRPCErrorInfo JSONRPC.pARSE_ERROR "Parse error" (Just (Aeson.String message))

invalidRequestResponse :: Text -> RpcResponse
invalidRequestResponse message =
  JSONRPC.ErrorMessage $
    JSONRPC.JSONRPCError JSONRPC.rPC_VERSION nullRequestId $
      JSONRPC.JSONRPCErrorInfo JSONRPC.iNVALID_REQUEST "Invalid request" (Just (Aeson.String message))

rpcError :: Text -> Text -> RpcError
rpcError code message =
  JSONRPC.JSONRPCErrorInfo
    { JSONRPC.code = errorCodeNumber code
    , JSONRPC.message = message
    , JSONRPC.errorData =
        Just $
          Aeson.object
            [ "code" Aeson..= code
            ]
    }

notification :: Aeson.ToJSON a => Text -> a -> RpcNotification
notification method value =
  JSONRPC.JSONRPCNotification JSONRPC.rPC_VERSION method (Aeson.toJSON value)

textRequestId :: Text -> RequestId
textRequestId =
  JSONRPC.RequestId . Aeson.String

nullRequestId :: RequestId
nullRequestId =
  JSONRPC.RequestId Aeson.Null

errorCodeNumber :: Text -> Int
errorCodeNumber = \case
  "invalid_json" -> JSONRPC.pARSE_ERROR
  "invalid_request" -> JSONRPC.iNVALID_REQUEST
  "method_not_found" -> JSONRPC.mETHOD_NOT_FOUND
  "invalid_params" -> JSONRPC.iNVALID_PARAMS
  _ -> JSONRPC.iNTERNAL_ERROR
