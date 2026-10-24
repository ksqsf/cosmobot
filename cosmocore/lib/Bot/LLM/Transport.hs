module Bot.LLM.Transport (askChatStreaming, askChatWithToolsStreaming) where

import Bot.Prelude
import qualified Bot.Effect.HTTP as HTTP
import Bot.LLM.Provider
import Bot.LLM.Types
import qualified Bot.LLM.Anthropic as Anthropic
import qualified Bot.LLM.Gemini as Gemini
import qualified Bot.LLM.Responses as Responses
import qualified Bot.LLM.OpenAI as Transport
import Effectful.Timeout
import qualified Effectful.Resource as Resource

askChatStreaming
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Timeout :> es, Fail :> es, Resource.Resource :> es)
  => Config -> [ChatMessage] -> Stream (Of Text) (Eff es) Text
askChatStreaming cfg messages = case cfg.chatProvider of
  Just provider | provider.api == "anthropic" -> chatAnswerContent <$> Anthropic.askStreaming provider [] messages
  Just provider | provider.api == "gemini" -> chatAnswerContent <$> Gemini.askStreaming provider [] messages
  Just provider | provider.api == "responses" -> chatAnswerContent <$> Responses.askStreaming provider [] messages
  _ -> Transport.askOpenAIStreaming cfg messages

askChatWithToolsStreaming
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Timeout :> es, Fail :> es, Resource.Resource :> es)
  => Config -> [FunctionTool] -> [ChatMessage] -> Stream (Of Text) (Eff es) ChatAnswer
askChatWithToolsStreaming cfg tools messages = case cfg.chatProvider of
  Just provider | provider.api == "anthropic" -> Anthropic.askStreaming provider tools messages
  Just provider | provider.api == "gemini" -> Gemini.askStreaming provider tools messages
  Just provider | provider.api == "responses" -> Responses.askStreaming provider tools messages
  _ -> Transport.askOpenAIWithToolsStreaming cfg tools messages
