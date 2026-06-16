{-|
Module      : Bot.ACP.Types
Description : Minimal Agent Client Protocol JSON-RPC types
Stability   : experimental
-}

module Bot.ACP.Types
  ( AcpRequest
  , AcpResponse
  , AcpNotification
  , RequestId
  , requestId
  , requestMethod
  , requestParams
  , successResponse
  , errorResponse
  , parseErrorResponse
  , invalidRequestResponse
  )
where

import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified JSONRPC

type AcpRequest = JSONRPC.JSONRPCRequest
type AcpResponse = JSONRPC.JSONRPCMessage
type AcpNotification = JSONRPC.JSONRPCNotification
type RequestId = JSONRPC.RequestId

requestId :: AcpRequest -> RequestId
requestId request =
  request.id

requestMethod :: AcpRequest -> Text
requestMethod request =
  request.method

requestParams :: AcpRequest -> Aeson.Value
requestParams request =
  request.params

successResponse :: Aeson.ToJSON a => RequestId -> a -> AcpResponse
successResponse responseId value =
  JSONRPC.ResponseMessage $
    JSONRPC.JSONRPCResponse JSONRPC.rPC_VERSION responseId (Aeson.toJSON value)

errorResponse :: RequestId -> Text -> Text -> AcpResponse
errorResponse responseId code message =
  JSONRPC.ErrorMessage $
    JSONRPC.JSONRPCError JSONRPC.rPC_VERSION responseId (acpError code message)

parseErrorResponse :: Text -> AcpResponse
parseErrorResponse message =
  JSONRPC.ErrorMessage $
    JSONRPC.JSONRPCError JSONRPC.rPC_VERSION nullRequestId $
      JSONRPC.JSONRPCErrorInfo JSONRPC.pARSE_ERROR "Parse error" (Just (Aeson.String message))

invalidRequestResponse :: Text -> AcpResponse
invalidRequestResponse message =
  JSONRPC.ErrorMessage $
    JSONRPC.JSONRPCError JSONRPC.rPC_VERSION nullRequestId $
      JSONRPC.JSONRPCErrorInfo JSONRPC.iNVALID_REQUEST "Invalid request" (Just (Aeson.String message))

acpError :: Text -> Text -> JSONRPC.JSONRPCErrorInfo
acpError code message =
  JSONRPC.JSONRPCErrorInfo
    { JSONRPC.code = errorCodeNumber code
    , JSONRPC.message = message
    , JSONRPC.errorData =
        Just $
          Aeson.object
            [ "code" Aeson..= code
            ]
    }

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
