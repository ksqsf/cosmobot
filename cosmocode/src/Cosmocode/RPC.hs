{-# LANGUAGE DataKinds #-}
module Cosmocode.RPC
  ( Rpc
  , openSession
  , getSession
  , receiveServerEvent
  , sendChat
  ) where

import Cosmocode.Types
import Cosmocode.RPC.Internal
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic

openSession :: Rpc :> es => Eff es (Either Text Text)
openSession = send OpenSession

getSession :: Rpc :> es => Text -> Eff es (Either Text (Maybe [SessionMessage]))
getSession = send . GetSession

receiveServerEvent :: Rpc :> es => Eff es (Either Text (Maybe ServerEvent))
receiveServerEvent = send ReceiveServerEvent

sendChat :: Rpc :> es => Text -> Text -> Eff es (Either Text ())
sendChat sessionId body = send (SendChat sessionId body)
