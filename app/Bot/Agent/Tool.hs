{-|
Module      : Bot.Agent.Tool
Description : Agent tool and context types
Stability   : experimental
-}

module Bot.Agent.Tool
  ( Tool (..)
  , AgentContext (..)
  , ToolResult (..)
  , toolText
  , toolMessage
  )
where

import Bot.Conversation
import qualified Bot.Memory as Memory
import Bot.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson

-- | Tool definition exposed to the LLM function-calling API.
data Tool es = Tool
  { name        :: !Text
  , description :: !Text
  , parameters  :: !Aeson.Value
  , allowed     :: AgentContext es -> Bool
  , run         :: AgentContext es -> Aeson.Value -> Eff es ToolResult
  }

-- | Per-message capabilities and permissions made available to tools.
data AgentContext es = AgentContext
  { message :: IncomingMessage
  , superuser :: !Bool
  , askCommand :: !Text
  , memoryConfig :: !(Maybe Memory.MemoryConfig)
  , remember :: Maybe Integer -> Conversation -> Eff es ()
  , recordBotMessage :: Maybe Integer -> Text -> Eff es ()
  }

-- | Text returned to the LLM plus any bot message ids produced by a tool.
data ToolResult = ToolResult
  { content    :: !Text
  , messageIds :: ![Maybe Integer]
  }

toolText :: Text -> ToolResult
toolText content =
  ToolResult content []

toolMessage :: Maybe Integer -> Text -> ToolResult
toolMessage messageId content =
  ToolResult content [messageId]
