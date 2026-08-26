{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Cosmocode.RPC.Internal
  ( Rpc (..)
  ) where

import Cosmocode.Types
import Data.Text (Text)
import Effectful

data Rpc :: Effect where
  OpenSession :: Rpc m (Either Text Text)
  GetSession :: Text -> Rpc m (Either Text (Maybe [SessionMessage]))
  ReceiveServerEvent :: Rpc m (Either Text (Maybe ServerEvent))
  SendChat :: Text -> Text -> Rpc m ()

type instance DispatchOf Rpc = Dynamic
