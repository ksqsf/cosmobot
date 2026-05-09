{-
Module      : Bot.Agent
Description : Agent loop and extensible tool framework
Stability   : experimental
-}

module Bot.Agent where

import Bot.Conversation
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.LLM as LLM
import Bot.Message
import Bot.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import System.Directory
import System.FilePath
import System.IO.Error (userError)

data Tool es = Tool
  { name        :: !Text
  , description :: !Text
  , parameters  :: !Aeson.Value
  , allowed     :: AgentContext -> Bool
  , run         :: AgentContext -> Aeson.Value -> Eff es Text
  }

data AgentContext = AgentContext
  { message :: IncomingMessage
  , superuser :: !Bool
  }

runAgent
  :: (LLM.LLM :> es, Log :> es)
  => Int
  -> AgentContext
  -> [Tool es]
  -> Conversation
  -> Eff es (Text, Conversation)
runAgent maxTurns context tools conversation =
  loop (max 1 maxTurns) conversation
  where
    exposedTools = filter (`toolAllowed` context) tools

    loop turnsLeft current = do
      answer <- LLM.askWithTools (map toolSchema exposedTools) current.messages
      let answered = appendMessage (LLM.assistantAnswer answer) current
      case answer.toolCalls of
        [] ->
          pure (answer.content, answered)
        calls
          | turnsLeft <= 1 -> do
              logInfo "Agent tool turn limit reached" calls
              pure (toolLimitMessage answer.content, answered)
          | otherwise -> do
              results <- traverse execute calls
              loop (turnsLeft - 1) (appendMessages results answered)

    execute call = do
      let callName = call.name
      result <- runTool context tools call `catch` \(err :: SomeException) ->
        pure [i|Tool #{callName} failed: #{show err :: String}|]
      pure (LLM.toolResult call result)

toolLimitMessage :: Text -> Text
toolLimitMessage content
  | Text.null (Text.strip content) = "Agent stopped: maximum tool turns reached."
  | otherwise = content

toolSchema :: Tool es -> LLM.FunctionTool
toolSchema Tool{name, description, parameters} =
  LLM.FunctionTool
    { name = name
    , description = description
    , parameters = parameters
    }

runTool :: AgentContext -> [Tool es] -> LLM.ToolCall -> Eff es Text
runTool context tools call =
  case find ((== call.name) . (.name)) tools of
    Nothing ->
      pure [i|Unknown tool: #{callName}|]
    Just tool
      | not (toolAllowed tool context) ->
          pure [i|Permission denied for tool: #{callName}|]
      | otherwise ->
      case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 call.arguments) of
        Left err ->
          pure [i|Invalid JSON arguments for #{callName}: #{err}|]
        Right args ->
          tool.run context args
  where
    callName = call.name

toolAllowed :: Tool es -> AgentContext -> Bool
toolAllowed tool context =
  tool.allowed context

everyone :: AgentContext -> Bool
everyone _ =
  True

superuserOnly :: AgentContext -> Bool
superuserOnly =
  (.superuser)

appendMessage :: LLM.ChatMessage -> Conversation -> Conversation
appendMessage message (Conversation messages) =
  Conversation (messages <> [message])

appendMessages :: [LLM.ChatMessage] -> Conversation -> Conversation
appendMessages newMessages (Conversation messages) =
  Conversation (messages <> newMessages)

defaultTools :: (Chat.Chat :> es, IOE :> es) => [Tool es]
defaultTools =
  [ listDirectoryTool
  , readFileTool
  , sendReplyTool
  , senderMemberInfoTool
  ]

listDirectoryTool :: IOE :> es => Tool es
listDirectoryTool = Tool
  { name = "list_directory"
  , description = "List files and directories under a path inside the bot working directory."
  , parameters = objectSchema
      [ requiredText "path" "Directory path relative to the bot working directory. Use \".\" for the working directory."
      ]
      ["path"]
  , allowed = superuserOnly
  , run = \_ -> withTextArg "path" \path -> do
      target <- resolveSafePath path
      isDir <- liftIO (doesDirectoryExist target)
      if not isDir
        then pure "Not a directory."
        else do
          entries <- liftIO (listDirectory target)
          pure (jsonText entries)
  }

readFileTool :: IOE :> es => Tool es
readFileTool = Tool
  { name = "read_file"
  , description = "Read a UTF-8 text file inside the bot working directory."
  , parameters = objectSchema
      [ requiredText "path" "File path relative to the bot working directory."
      ]
      ["path"]
  , allowed = superuserOnly
  , run = \_ -> withTextArg "path" \path -> do
      target <- resolveSafePath path
      isFile <- liftIO (doesFileExist target)
      if not isFile
        then pure "Not a file."
        else liftIO (Text.readFile target)
  }

sendReplyTool :: Chat.Chat :> es => Tool es
sendReplyTool = Tool
  { name = "send_reply_to_current_chat"
  , description = "Send a reply message to the same chat as the current user message. Use only when the user asks you to send an additional message before the final answer."
  , parameters = objectSchema
      [ requiredText "text" "Message text to send."
      ]
      ["text"]
  , allowed = everyone
  , run = \context -> withTextArg "text" \text -> do
      sent <- Chat.replyTo context.message text
      let sentText = show sent :: String
      pure [i|Sent message id: #{sentText}|]
  }

senderMemberInfoTool :: Chat.Chat :> es => Tool es
senderMemberInfoTool = Tool
  { name = "get_current_sender_member_info"
  , description = "Get platform-provided member information for the sender of the current message in the current group chat."
  , parameters = objectSchema [] []
  , allowed = everyone
  , run = \context _ -> do
      info <- Chat.getSenderMemberInfo context.message
      pure (maybe "No member information is available for this message." jsonText info)
  }

withTextArg :: Text -> (Text -> Eff es Text) -> Aeson.Value -> Eff es Text
withTextArg key action =
  either (pure . Text.pack) action . AesonTypes.parseEither parser
  where
    parser = Aeson.withObject "tool arguments" (Aeson..: Key.fromText key)

resolveSafePath :: IOE :> es => Text -> Eff es FilePath
resolveSafePath rawPath = do
  cwd <- liftIO getCurrentDirectory
  target <- liftIO (canonicalizePath (cwd </> Text.unpack rawPath))
  unless (cwd `isEqualOrParentOf` target) do
    throwIO (userError "Path escapes the bot working directory.")
  pure target

isEqualOrParentOf :: FilePath -> FilePath -> Bool
isEqualOrParentOf parent child =
  parent == child || addTrailingPathSeparator parent `isPrefixOf` child

requiredText :: Text -> Text -> (Text, Aeson.Value)
requiredText name description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "string"
      , "description" Aeson..= description
      ]
  )

objectSchema :: [(Text, Aeson.Value)] -> [Text] -> Aeson.Value
objectSchema fields required =
  Aeson.object
    [ "type" Aeson..= Aeson.String "object"
    , "properties" Aeson..= Aeson.object
        [ Key.fromText name Aeson..= schema
        | (name, schema) <- fields
        ]
    , "required" Aeson..= required
    , "additionalProperties" Aeson..= False
    ]

jsonText :: Aeson.ToJSON a => a -> Text
jsonText =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode
