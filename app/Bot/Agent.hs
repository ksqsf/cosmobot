{-|
Module      : Bot.Agent
Description : Agent loop and extensible tool framework
Stability   : experimental
-}

module Bot.Agent
  ( Tool (..)
  , AgentContext (..)
  , ToolConfig (..)
  , WebSearchApi (..)
  , defaultToolConfig
  , ToolResult (..)
  , runAgent
  , runAgentStreaming
  , defaultTools
  )
where

import Bot.Conversation
import Bot.Agent.Tool
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.ChatLog as ChatLog
import qualified Bot.Effect.LLM as LLM
import qualified Bot.Effect.Scheduler as Scheduler
import qualified Bot.Memory as Memory
import Bot.Message
import Bot.Prelude
import qualified Bot.ReplyBody as ReplyBody
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (digitToInt, isHexDigit)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding
import qualified Data.Text.IO as Text
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Req
import System.Directory
import System.Exit (ExitCode)
import System.FilePath
import System.IO (hClose)
import System.IO.Error (userError)
import System.Posix.Signals (signalProcess, signalProcessGroup, sigKILL)
import System.Process (ProcessHandle, createProcess, shell, std_out, std_err, create_group, StdStream(..), getPid, waitForProcess)
import System.Timeout (timeout)
import qualified Streaming.Prelude as S
import qualified Text.URI as URI

-- | Run an LLM/tool loop until the model answers or the tool turn limit is hit.
runAgent
  :: (LLM.LLM :> es, Log :> es, IOE :> es)
  => Int
  -> AgentContext es
  -> [Tool es]
  -> Conversation
  -> Eff es (Text, Conversation)
runAgent maxTurns context tools conversation =
  S.mapM_ (\_ -> pure ()) (runAgentStreaming maxTurns context tools conversation)

-- | Run an LLM/tool loop, streaming assistant text chunks from the final model turn.
runAgentStreaming
  :: (LLM.LLM :> es, Log :> es, IOE :> es)
  => Int
  -> AgentContext es
  -> [Tool es]
  -> Conversation
  -> Stream (Of Text) (Eff es) (Text, Conversation)
runAgentStreaming maxTurns context tools conversation =
  loop (max 1 maxTurns) 0 (closeInterruptedToolCalls conversation)
  where
    exposedTools = filter (`toolAllowed` context) tools

    loop turnsLeft webFetchUses current = do
      answer <- LLM.askWithToolsStreaming (map toolSchema exposedTools) current.messages
      let answered = appendMessage (LLM.assistantAnswer answer) current
      case answer.toolCalls of
        [] ->
          pure (answer.content, answered)
        calls
          | turnsLeft <= 1 -> do
              lift $ logInfo "Agent tool turn limit reached" calls
              let paused = appendMessages (map pausedToolResult calls) answered
              pure (toolLimitMessage answer.content calls, paused)
          | otherwise -> do
              (results, nextWebFetchUses) <- lift $ executeCalls webFetchUses calls
              let next = appendMessages (map fst results) answered
              lift $ traverse_ (\messageId -> context.remember messageId next) (concatMap snd results)
              loop (turnsLeft - 1) nextWebFetchUses next

    executeCalls webFetchUses [] =
      pure ([], webFetchUses)
    executeCalls webFetchUses (call : calls) = do
      (result, nextWebFetchUses) <- execute webFetchUses call
      (rest, finalWebFetchUses) <- executeCalls nextWebFetchUses calls
      pure (result : rest, finalWebFetchUses)

    execute webFetchUses call = do
      let callName = call.name
          webFetchCall = callName == "web_fetch"
          webFetchLimit = context.toolConfig.webFetchMaxUses
      result <-
        if webFetchCall && maybe False (webFetchUses >=) webFetchLimit
          then pure (toolText [i|web_fetch use limit reached for this agent run: #{webFetchUses}.|])
          else runTool context tools call `catch` \(err :: SomeException) ->
            pure (toolText [i|Tool #{callName} failed: #{show err :: String}|])
      let nextWebFetchUses =
            if webFetchCall && maybe True (webFetchUses <) webFetchLimit
              then webFetchUses + 1
              else webFetchUses
      pure ((LLM.toolResult call result.content, result.messageIds), nextWebFetchUses)

toolLimitMessage :: Text -> [LLM.ToolCall] -> Text
toolLimitMessage content calls
  | Text.null stripped =
      [i|已暂停：本次 agent 工具调用轮数已用完，尚未执行下一步工具调用：#{toolCallList calls}

如果需要继续，请直接回复下一条消息。|]
  | otherwise =
      [i|#{stripped}

已暂停：本次 agent 工具调用轮数已用完，尚未执行下一步工具调用：#{toolCallList calls}

如果需要继续，请直接回复下一条消息。|]
  where
    stripped = Text.strip content

toolCallList :: [LLM.ToolCall] -> Text
toolCallList calls =
  Text.intercalate ", " (map (.name) calls)

pausedToolResult :: LLM.ToolCall -> LLM.ChatMessage
pausedToolResult call =
  LLM.toolResult call "Agent paused because the maximum tool turn limit was reached before this tool call could run. The user may continue the conversation to resume the work."

closeInterruptedToolCalls :: Conversation -> Conversation
closeInterruptedToolCalls (Conversation messages) =
  Conversation (go messages)
  where
    go [] = []
    go (message : rest)
      | message.role == "assistant" && not (null message.toolCalls) =
          let (toolResults, remaining) = span isToolResult rest
              existingIds = mapMaybe (.toolCallId) toolResults
              missingCalls = filter ((`notElem` existingIds) . (.id)) message.toolCalls
          in message : toolResults <> map pausedToolResult missingCalls <> go remaining
      | otherwise =
          message : go rest

    isToolResult message =
      message.role == "tool"

toolSchema :: Tool es -> LLM.FunctionTool
toolSchema Tool{name, description, parameters} =
  LLM.FunctionTool
    { name = name
    , description = description
    , parameters = parameters
    }

runTool :: AgentContext es -> [Tool es] -> LLM.ToolCall -> Eff es ToolResult
runTool context tools call =
  case find ((== call.name) . (.name)) tools of
    Nothing ->
      pure (toolText [i|Unknown tool: #{callName}|])
    Just tool
      | not (toolAllowed tool context) ->
          pure (toolText [i|Permission denied for tool: #{callName}|])
      | otherwise ->
      case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 call.arguments) of
        Left err ->
          pure (toolText [i|Invalid JSON arguments for #{callName}: #{err}|])
        Right args ->
          tool.run context args
  where
    callName = call.name

toolAllowed :: Tool es -> AgentContext es -> Bool
toolAllowed tool context =
  tool.allowed context

everyone :: AgentContext es -> Bool
everyone _ =
  True

webSearchEnabled :: AgentContext es -> Bool
webSearchEnabled context =
  context.toolConfig.webSearchEnable

webFetchEnabled :: AgentContext es -> Bool
webFetchEnabled context =
  context.toolConfig.webFetch

datetimeEnabled :: AgentContext es -> Bool
datetimeEnabled context =
  context.toolConfig.datetime

superuserOnly :: AgentContext es -> Bool
superuserOnly =
  (.superuser)

appendMessage :: LLM.ChatMessage -> Conversation -> Conversation
appendMessage message (Conversation messages) =
  Conversation (messages <> [message])

appendMessages :: [LLM.ChatMessage] -> Conversation -> Conversation
appendMessages newMessages (Conversation messages) =
  Conversation (messages <> newMessages)

-- | Built-in tools exposed to the model after per-message permission checks.
defaultTools :: (Chat.Chat :> es, ChatLog.ChatLog :> es, LLM.LLM :> es, Scheduler.Scheduler :> es, IOE :> es) => [Tool es]
defaultTools =
  [ listDirectoryTool
  , readFileTool
  , queryChatLogTool
  , webSearchTool
  , webFetchTool
  , datetimeTool
  , generateImageTool
  , sendReplyTool
  , mentionUserTool
  , senderMemberInfoTool
  , memberInfoTool
  , listGroupMembersTool
  , currentMentionsTool
  , scheduleAgentActionTool
  , deleteScheduledAgentActionTool
  , listCurrentUserSchedulesTool
  , manageMemoryTool
  , manageChatMemoryTool
  , runBashTool
  ]

manageMemoryTool :: IOE :> es => Tool es
manageMemoryTool = Tool
  { name = "manage_current_sender_memory"
  , description = "View, replace, or clear the persistent MEMORY.md for the current message sender. Use this when the sender asks to view or clear memory, or when the sender gives durable preferences such as a preferred name, style, language, stable personal facts, or recurring instructions. Keep memory concise: non-superusers must stay within 1000 characters; if an update is rejected, summarize it shorter and try again."
  , parameters = objectSchema
      [ fieldText "action" "One of: view, replace, clear."
      , fieldText "memory" "Complete replacement MEMORY.md content. Required only when action is replace."
      ]
      ["action"]
  , allowed = everyone
  , run = \context args ->
      case context.memoryConfig of
        Nothing ->
          pure (toolText "Memory is not configured.")
        Just cfg ->
          case AesonTypes.parseEither memoryArgs args of
            Left err ->
              pure (toolText (Text.pack err))
            Right (action, memory) ->
              runMemoryAction senderMemoryScope cfg context action memory
  }

manageChatMemoryTool :: IOE :> es => Tool es
manageChatMemoryTool = Tool
  { name = "manage_current_chat_memory"
  , description = "View, replace, or clear the persistent MEMORY.md for the current chat/conversation. Use this when the user asks to view or clear chat memory, or when durable preferences, facts, norms, recurring instructions, or context apply to this chat as a whole rather than only to the current sender. Keep memory concise: non-superusers must stay within 1000 characters; if an update is rejected, summarize it shorter and try again."
  , parameters = objectSchema
      [ fieldText "action" "One of: view, replace, clear."
      , fieldText "memory" "Complete replacement MEMORY.md content. Required only when action is replace."
      ]
      ["action"]
  , allowed = everyone
  , run = \context args ->
      case context.memoryConfig of
        Nothing ->
          pure (toolText "Memory is not configured.")
        Just cfg ->
          case AesonTypes.parseEither memoryArgs args of
            Left err ->
              pure (toolText (Text.pack err))
            Right (action, memory) ->
              runMemoryAction chatMemoryScope cfg context action memory
  }

listDirectoryTool :: IOE :> es => Tool es
listDirectoryTool = Tool
  { name = "list_directory"
  , description = "List files and directories under a path inside the bot working directory."
  , parameters = objectSchema
      [ fieldText "path" "Directory path relative to the bot working directory. Use \".\" for the working directory."
      ]
      ["path"]
  , allowed = superuserOnly
  , run = \_ -> withTextArg "path" \path -> do
      target <- resolveSafePath path
      isDir <- liftIO (doesDirectoryExist target)
      if not isDir
        then pure (toolText "Not a directory.")
        else do
          entries <- liftIO (listDirectory target)
          pure (toolText (jsonText entries))
  }

readFileTool :: IOE :> es => Tool es
readFileTool = Tool
  { name = "read_file"
  , description = "Read a UTF-8 text file inside the bot working directory."
  , parameters = objectSchema
      [ fieldText "path" "File path relative to the bot working directory."
      ]
      ["path"]
  , allowed = superuserOnly
  , run = \_ -> withTextArg "path" \path -> do
      target <- resolveSafePath path
      isFile <- liftIO (doesFileExist target)
      if not isFile
        then pure (toolText "Not a file.")
        else toolText <$> liftIO (Text.readFile target)
  }

queryChatLogTool :: ChatLog.ChatLog :> es => Tool es
queryChatLogTool = Tool
  { name = "query_current_chat_log"
  , description = "Return recent messages recorded in the current chat. Results are in chronological order and include sender ids, message ids, mentions, image urls, and text."
  , parameters = objectSchema
      [ fieldInteger "limit" "Maximum number of recent messages to return."
      , fieldBoolean "include_bot_messages" "Whether to include bot messages. Defaults to false."
      ]
      ["limit"]
  , allowed = everyone
  , run = \context args ->
      case AesonTypes.parseEither queryChatLogArgs args of
        Left err ->
          pure (toolText (Text.pack err))
        Right (limit, includeBotMessages) -> do
          entries <- ChatLog.queryChat context.message (fromInteger (max 0 limit)) includeBotMessages
          pure (toolText (jsonText entries))
  }

webSearchTool :: IOE :> es => Tool es
webSearchTool = Tool
  { name = "web_search"
  , description = "Search the web for current information. Returns a JSON object with the query and a results array containing title, url, and snippet."
  , parameters = objectSchema
      [ fieldText "query" "Search query."
      , fieldInteger "max_results" "Maximum number of results to return. Defaults to 5 and is capped at 20."
      ]
      ["query"]
  , allowed = webSearchEnabled
  , run = \context args ->
      case AesonTypes.parseEither (webSearchArgs context.toolConfig.webSearchMaxResults) args of
        Left err ->
          pure (toolText (Text.pack err))
        Right (query, maxResults) -> do
          let searchConfig = context.toolConfig
          results <- liftIO (webSearch searchConfig query maxResults)
          pure (toolText (jsonText (Aeson.object
            [ "query" Aeson..= query
            , "source" Aeson..= webSearchSource searchConfig.webSearchApi
            , "results" Aeson..= results
            ])))
  }

webFetchTool :: IOE :> es => Tool es
webFetchTool = Tool
  { name = "web_fetch"
  , description = "Fetch a web page by URL and return extracted readable text. Supports http and https URLs."
  , parameters = objectSchema
      [ fieldText "url" "HTTP or HTTPS URL to fetch."
      , fieldInteger "max_content_tokens" "Approximate maximum content tokens to return. Defaults to the configured tool.web_fetch.max_content_tokens or 50000."
      ]
      ["url"]
  , allowed = webFetchEnabled
  , run = \context args ->
      case AesonTypes.parseEither (webFetchArgs context.toolConfig.webFetchMaxContentTokens) args of
        Left err ->
          pure (toolText (Text.pack err))
        Right (url, maxContentTokens) -> do
          page <- liftIO (fetchWebPage url maxContentTokens)
          pure (toolText (jsonText page))
  }

datetimeTool :: IOE :> es => Tool es
datetimeTool = Tool
  { name = "datetime"
  , description = "Return the current date and time in UTC and in the bot host's local timezone."
  , parameters = objectSchema [] []
  , allowed = datetimeEnabled
  , run = \_ _ -> do
      now <- liftIO getCurrentTime
      zone <- liftIO getCurrentTimeZone
      let localTime = utcToLocalTime zone now
      pure (toolText (jsonText (Aeson.object
        [ "utc" Aeson..= Text.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
        , "local" Aeson..= Text.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" localTime)
        , "timezone_name" Aeson..= Text.pack (timeZoneName zone)
        , "timezone_offset_minutes" Aeson..= timeZoneMinutes zone
        , "unix_time" Aeson..= (realToFrac (utcTimeToPOSIXSeconds now) :: Double)
        ])))
  }

webSearch :: ToolConfig -> Text -> Int -> IO [Aeson.Value]
webSearch cfg query maxResults =
  case cfg.webSearchApi of
    WebSearchTavily ->
      case cfg.tavilyApiKey of
        Nothing -> Exception.throwIO (userError "Tavily search is not configured: set tool.web_search.tavily_api_key.")
        Just key -> tavilySearch key query maxResults
    WebSearchBrave ->
      case cfg.braveApiKey of
        Nothing -> Exception.throwIO (userError "Brave search is not configured: set tool.web_search.brave_api_key.")
        Just key -> braveSearch key query maxResults
    WebSearchDDG ->
      duckDuckGoSearch query maxResults

webSearchSource :: WebSearchApi -> Text
webSearchSource = \case
  WebSearchTavily -> "tavily"
  WebSearchBrave -> "brave"
  WebSearchDDG -> "duckduckgo_html"

tavilySearch :: Text -> Text -> Int -> IO [Aeson.Value]
tavilySearch apiKey query maxResults = do
  response <- runReq defaultHttpConfig $
    req POST
      (https "api.tavily.com" /: "search")
      (ReqBodyJson (Aeson.object
        [ "query" Aeson..= query
        , "max_results" Aeson..= maxResults
        , "search_depth" Aeson..= Aeson.String "basic"
        , "include_answer" Aeson..= False
        , "include_raw_content" Aeson..= False
        ]))
      jsonResponse
      ( header "Authorization" (ByteString.pack [i|Bearer #{apiKey}|])
          <> webRequestOptions
      )
  either (Exception.throwIO . userError) pure $
    AesonTypes.parseEither parseTavilyResults (responseBody response)

braveSearch :: Text -> Text -> Int -> IO [Aeson.Value]
braveSearch apiKey query maxResults = do
  response <- runReq defaultHttpConfig $
    req GET
      (https "api.search.brave.com" /: "res" /: "v1" /: "web" /: "search")
      NoReqBody
      jsonResponse
      ( "q" =: query
          <> "count" =: maxResults
          <> header "X-Subscription-Token" (TextEncoding.encodeUtf8 apiKey)
          <> webRequestOptions
      )
  either (Exception.throwIO . userError) pure $
    AesonTypes.parseEither parseBraveResults (responseBody response)

parseTavilyResults :: Aeson.Value -> AesonTypes.Parser [Aeson.Value]
parseTavilyResults =
  Aeson.withObject "Tavily search response" $ \o -> do
    results <- o Aeson..: Key.fromText "results"
    traverse parseResult results
  where
    parseResult =
      Aeson.withObject "Tavily result" $ \o -> do
        title <- o Aeson..: Key.fromText "title"
        url <- o Aeson..: Key.fromText "url"
        snippet <- fromMaybe "" <$> o Aeson..:? Key.fromText "content"
        pure (searchResult title url snippet)

parseBraveResults :: Aeson.Value -> AesonTypes.Parser [Aeson.Value]
parseBraveResults =
  Aeson.withObject "Brave search response" $ \o -> do
    web <- o Aeson..:? Key.fromText "web"
    case web of
      Nothing ->
        pure []
      Just webObject ->
        Aeson.withObject "Brave web results" parseWeb webObject
  where
    parseWeb o = do
      results <- fromMaybe [] <$> o Aeson..:? Key.fromText "results"
      traverse parseResult results

    parseResult =
      Aeson.withObject "Brave result" $ \o -> do
        title <- o Aeson..: Key.fromText "title"
        url <- o Aeson..: Key.fromText "url"
        snippet <- fromMaybe "" <$> o Aeson..:? Key.fromText "description"
        pure (searchResult title url snippet)

searchResult :: Text -> Text -> Text -> Aeson.Value
searchResult title url snippet =
  Aeson.object
    [ "title" Aeson..= title
    , "url" Aeson..= url
    , "snippet" Aeson..= snippet
    ]

duckDuckGoSearch :: Text -> Int -> IO [Aeson.Value]
duckDuckGoSearch query maxResults = do
  response <- runReq defaultHttpConfig $
    req GET
      (https "html.duckduckgo.com" /: "html")
      NoReqBody
      bsResponse
      ("q" =: query <> webRequestOptions)
  let html = decodeResponseBody (responseBody response)
      anchors = mapMaybe parseSearchAnchor (Text.lines html)
      snippets = mapMaybe parseSearchSnippet (Text.lines html)
  pure (take maxResults (zipWithSearchSnippets anchors snippets))

parseSearchAnchor :: Text -> Maybe (Text, Text)
parseSearchAnchor line = do
  guard ("result__a" `Text.isInfixOf` line)
  href <- extractHtmlAttr "href" line
  let title = htmlToPlainText line
      url = normalizeDuckDuckGoUrl href
  guard (not (Text.null title) && not (Text.null url))
  pure (title, url)

parseSearchSnippet :: Text -> Maybe Text
parseSearchSnippet line = do
  guard ("result__snippet" `Text.isInfixOf` line)
  let snippet = htmlToPlainText line
  guard (not (Text.null snippet))
  pure snippet

zipWithSearchSnippets :: [(Text, Text)] -> [Text] -> [Aeson.Value]
zipWithSearchSnippets results snippets =
  [ Aeson.object
      [ "title" Aeson..= title
      , "url" Aeson..= url
      , "snippet" Aeson..= snippet
      ]
  | ((title, url), snippet) <- zip results (snippets <> repeat "")
  ]

fetchWebPage :: Text -> Int -> IO Aeson.Value
fetchWebPage rawUrl maxContentTokens = do
  uri <- URI.mkURI rawUrl
  case useURI uri of
    Nothing ->
      Exception.throwIO (userError [i|Unsupported URL: #{rawUrl}. Use an http or https URL.|])
    Just (Left (url, options)) ->
      fetch url options
    Just (Right (url, options)) ->
      fetch url options
  where
    fetch :: Url scheme -> Option scheme -> IO Aeson.Value
    fetch url options = do
      response <- runReq defaultHttpConfig $
        req GET url NoReqBody bsResponse (options <> webRequestOptions)
      let contentType = TextEncoding.decodeUtf8With TextEncoding.lenientDecode <$> responseHeader response "Content-Type"
          body = htmlToPlainText (decodeResponseBody (responseBody response))
          text = takeApproxTokens maxContentTokens body
      pure (Aeson.object
        [ "url" Aeson..= rawUrl
        , "status" Aeson..= responseStatusCode response
        , "content_type" Aeson..= contentType
        , "content" Aeson..= text
        , "truncated" Aeson..= (Text.length text < Text.length body)
        ])

takeApproxTokens :: Int -> Text -> Text
takeApproxTokens maxTokens =
  Text.take (maxTokens * 4)

webRequestOptions :: Option scheme
webRequestOptions =
  header "User-Agent" "cosmobot/0.1 (+https://github.com/ksqsf/cosmobot)"
    <> responseTimeout (15 * 1_000_000)

decodeResponseBody :: ByteString.ByteString -> Text
decodeResponseBody =
  TextEncoding.decodeUtf8With TextEncoding.lenientDecode

normalizeDuckDuckGoUrl :: Text -> Text
normalizeDuckDuckGoUrl rawHref =
  let href = htmlDecode rawHref
      absolute =
        if "//" `Text.isPrefixOf` href
          then "https:" <> href
          else href
  in maybe absolute percentDecodeText (queryTextParam "uddg" absolute)

queryTextParam :: Text -> Text -> Maybe Text
queryTextParam key text =
  case Text.breakOn (key <> "=") text of
    (_, rest)
      | Text.null rest -> Nothing
      | otherwise ->
          Just $
            Text.takeWhile (/= '&') $
              Text.drop (Text.length key + 1) rest

extractHtmlAttr :: Text -> Text -> Maybe Text
extractHtmlAttr name html =
  extractWith "\"" <|> extractWith "'"
  where
    extractWith quote =
      let marker = name <> "=" <> quote
          (_, rest) = Text.breakOn marker html
      in if Text.null rest
        then Nothing
        else Just $
          Text.takeWhile (/= Text.head quote) $
            Text.drop (Text.length marker) rest

htmlToPlainText :: Text -> Text
htmlToPlainText =
  Text.unwords . Text.words . htmlDecode . stripHtmlTags

stripHtmlTags :: Text -> Text
stripHtmlTags =
  Text.pack . reverse . fst . Text.foldl' step ([], False)
  where
    step (acc, inTag) char =
      case (char, inTag) of
        ('<', _) -> (' ' : acc, True)
        ('>', _) -> (' ' : acc, False)
        (_, True) -> (acc, True)
        _ -> (char : acc, False)

htmlDecode :: Text -> Text
htmlDecode =
  Text.replace "&#39;" "'"
    . Text.replace "&quot;" "\""
    . Text.replace "&apos;" "'"
    . Text.replace "&gt;" ">"
    . Text.replace "&lt;" "<"
    . Text.replace "&amp;" "&"

percentDecodeText :: Text -> Text
percentDecodeText =
  Text.pack . go . Text.unpack
  where
    go ('%' : a : b : rest)
      | isHexDigit a && isHexDigit b =
          chr (digitToInt a * 16 + digitToInt b) : go rest
    go ('+' : rest) =
      ' ' : go rest
    go (char : rest) =
      char : go rest
    go [] =
      []

generateImageTool :: (Chat.Chat :> es, LLM.LLM :> es) => Tool es
generateImageTool = Tool
  { name = "generate_image"
  , description = "Generate an actual image from a prompt and send it to the current chat. Use this when the user *literally* asks to *draw*, *create*, or *generate* an image, including scheduled future image requests. After using this tool, keep the final answer brief and do not repeat the image URL. Never use this when the user is merely asking for, finding, or searching for an image; instead, use the web search tool."
  , parameters = objectSchema
      [ fieldText "prompt" "Image generation prompt. Include the user's visual requirements, style, subject, text, and constraints."
      ]
      ["prompt"]
  , allowed = everyone
  , run = \context -> withTextArg "prompt" \prompt -> do
      generated <- LLM.askImageWithHistory [LLM.userWithImages prompt context.message.imageUrls]
      case Chat.replyImageUrls generated of
        [] ->
          pure (toolText generated)
        _ -> do
          sent <- Chat.replyTo context.message generated
          context.recordBotMessage sent generated
          let sentText = show sent :: String
          pure (toolMessage sent [i|Generated and sent image message id: #{sentText}|])
  }

sendReplyTool :: Chat.Chat :> es => Tool es
sendReplyTool = Tool
  { name = "send_reply_to_current_chat"
  , description = "Send a reply message to the same chat as the current user message. Supports text and image URLs. Use image_urls when the user asks you to send an image found or generated elsewhere. Use only when the user asks you to send an additional message before the final answer."
  , parameters = objectSchema
      [ fieldText "text" "Message text to send. May be omitted when image_urls is non-empty."
      , fieldTextArray "image_urls" "Image URLs to send as images in the same reply. The platform must be able to fetch these URLs."
      ]
      []
  , allowed = everyone
  , run = \context args ->
      case AesonTypes.parseEither sendReplyArgs args of
        Left err ->
          pure (toolText (Text.pack err))
        Right body -> do
          sent <- Chat.replyTo context.message body
          context.recordBotMessage sent body
          let sentText = show sent :: String
          pure (toolMessage sent [i|Sent message id: #{sentText}|])
  }

mentionUserTool :: Chat.Chat :> es => Tool es
mentionUserTool = Tool
  { name = "mention_user"
  , description = "Send a reply in the current chat that mentions the given user id. On QQ this sends an actual at segment."
  , parameters = objectSchema
      [ fieldInteger "user_id" "Platform user id to mention."
      , fieldText "text" "Message text to send after the mention."
      ]
      ["user_id", "text"]
  , allowed = everyone
  , run = \context args ->
      case AesonTypes.parseEither mentionUserArgs args of
        Left err ->
          pure (toolText (Text.pack err))
        Right (userId, text) -> do
          sent <- Chat.mentionUser context.message userId text
          context.recordBotMessage sent text
          let sentText = show sent :: String
          pure (toolMessage sent [i|Sent mention message id: #{sentText}|])
  }

senderMemberInfoTool :: Chat.Chat :> es => Tool es
senderMemberInfoTool = Tool
  { name = "get_current_sender_member_info"
  , description = "Get platform-provided member information for the sender of the current message in the current group chat."
  , parameters = objectSchema [] []
  , allowed = everyone
  , run = \context _ -> do
      info <- Chat.getSenderMemberInfo context.message
      pure (toolText (maybe "No member information is available for this message." jsonText info))
  }

memberInfoTool :: Chat.Chat :> es => Tool es
memberInfoTool = Tool
  { name = "get_group_member_info"
  , description = "Get platform-provided member information for any user id in the current group chat."
  , parameters = objectSchema
      [ fieldInteger "user_id" "Platform user id to query in the current group."
      ]
      ["user_id"]
  , allowed = everyone
  , run = \context -> withIntegerArg "user_id" \userId -> do
      info <- Chat.getMemberInfo context.message userId
      pure (toolText (maybe "No member information is available for this user in the current chat." jsonText info))
  }

listGroupMembersTool :: Chat.Chat :> es => Tool es
listGroupMembersTool = Tool
  { name = "list_group_members"
  , description = "List members in the current group chat, including platform user ids and nicknames when available. QQ groups are supported. Telegram Bot API does not expose full member lists, so Telegram may return unavailable."
  , parameters = objectSchema [] []
  , allowed = everyone
  , run = \context _ -> do
      members <- Chat.listGroupMembers context.message
      pure (toolText (maybe "Group member listing is not available for this platform or chat." jsonText members))
  }

currentMentionsTool :: Tool es
currentMentionsTool = Tool
  { name = "get_current_message_mentions"
  , description = "Return platform user ids mentioned in the current message, in message order. On QQ these are QQ numbers from at segments."
  , parameters = objectSchema [] []
  , allowed = everyone
  , run = \context _ ->
      pure (toolText (jsonText context.message.mentions))
  }

scheduleAgentActionTool :: Scheduler.Scheduler :> es => Tool es
scheduleAgentActionTool = Tool
  { name = "schedule_agent_action"
  , description = "Schedule a future agent action in the current chat. The future action is processed through the same incoming message pipeline and replies to the current user message."
  , parameters = objectSchema
      [ fieldInteger "delay_seconds" "Delay before running the future agent action, in seconds."
      , fieldText "prompt" "Prompt for the future agent action."
      ]
      ["delay_seconds", "prompt"]
  , allowed = everyone
  , run = \context args ->
      case AesonTypes.parseEither scheduledActionArgs args of
        Left err ->
          pure (toolText (Text.pack err))
        Right (delaySeconds, prompt) -> do
          scheduled <- Scheduler.scheduleMessage delaySeconds (scheduledAgentMessage context delaySeconds prompt)
          if scheduled
            then pure (toolText [i|Scheduled agent action in #{delaySeconds} seconds.|])
            else pure (toolText "Could not schedule agent action: scheduler is at capacity.")
  }

deleteScheduledAgentActionTool :: Scheduler.Scheduler :> es => Tool es
deleteScheduledAgentActionTool = Tool
  { name = "delete_scheduled_agent_action"
  , description = "Delete a schedule using schedule ID. Only current user's schedules may be deleted."
  , parameters = objectSchema
    [ fieldInteger "schedule_id" "The schedule ID to be deleted."
    ]
    ["schedule_id"]
  , allowed = everyone
  , run = \context -> withIntegerArg "schedule_id" $ \scheduleId -> do
      ok <- Scheduler.deleteScheduledMessage context.message scheduleId
      if ok
        then pure (toolText [i|Schedule #{scheduleId} has been removed.|])
        else pure (toolText [i|Schedule #{scheduleId} is not available to the user.|])
  }

listCurrentUserSchedulesTool :: Scheduler.Scheduler :> es => Tool es
listCurrentUserSchedulesTool = Tool
  { name = "list_current_user_schedules"
  , description = "List pending scheduled agent actions created by the current user in the current chat. Returns schedule ids, remaining seconds, and scheduled prompts."
  , parameters = objectSchema [] []
  , allowed = everyone
  , run = \context _ -> do
      schedules <- Scheduler.listScheduledMessages context.message
      pure (toolText (jsonText (map scheduleSummary schedules)))
  }

runBashTool :: IOE :> es => Tool es
runBashTool = Tool
  { name = "run_bash"
  , description = "Run a bash script and obtain outputs; do not run malicious code."
  , parameters = objectSchema
      [ fieldText "script" "The bash script to be executed in the cwd"
      , fieldInteger "timeout_seconds" "Maximum seconds to wait before killing the process. Defaults to 30."
      ]
      ["script"]
  , allowed = superuserOnly
  , run = \_ args ->
      case AesonTypes.parseEither runBashArgs args of
        Left err ->
          pure (toolText (Text.pack err))
        Right (script, timeoutSeconds) -> do
          result <- liftIO $ runBashSafe timeoutSeconds (Text.unpack script)
          pure (toolText result)
  }

runBashSafe :: Int -> String -> IO Text
runBashSafe timeoutSeconds script = do
  let effectiveTimeout = max 1 timeoutSeconds
  (_, Just hOut, Just hErr, ph) <- createProcess
    (shell script)
      { std_out = CreatePipe
      , std_err = CreatePipe
      , create_group = True
      }
  stdoutVar <- readHandleAsync hOut
  stderrVar <- readHandleAsync hErr
  exitVar <- waitForProcessAsync ph
  outcome <- timeout (effectiveTimeout * 1_000_000) (MVar.takeMVar exitVar)
  case outcome of
    Nothing -> do
      killProcessTree ph
      _ <- timeout processExitGraceMicroseconds (MVar.takeMVar exitVar)
      stdoutText <- readerText "stdout" stdoutVar
      stderrText <- readerText "stderr" stderrVar
      ignoreIO (hClose hOut)
      ignoreIO (hClose hErr)
      pure $ Text.strip $ Text.unlines $ filter (not . Text.null)
        [ "Script timed out after " <> Text.pack (show effectiveTimeout) <> " seconds and was killed."
        , if Text.null stdoutText then "" else "stdout:\n" <> stdoutText
        , if Text.null stderrText then "" else "stderr:\n" <> stderrText
        ]
    Just (Left err) ->
      Exception.throwIO err
    Just (Right exitCode) -> do
      stdoutText <- readerText "stdout" stdoutVar
      stderrText <- readerText "stderr" stderrVar
      ignoreIO (hClose hOut)
      ignoreIO (hClose hErr)
      pure (formatBashResult exitCode stdoutText stderrText)

waitForProcessAsync :: ProcessHandle -> IO (MVar.MVar (Either SomeException ExitCode))
waitForProcessAsync ph = do
  result <- MVar.newEmptyMVar
  void $ forkIO do
    output <- Exception.try (waitForProcess ph)
    void (MVar.tryPutMVar result output)
  pure result

readHandleAsync :: Handle -> IO (MVar.MVar (Either SomeException Text))
readHandleAsync processOutputHandle = do
  result <- MVar.newEmptyMVar
  void $ forkIO do
    output <- Exception.try do
      text <- Text.hGetContents processOutputHandle
      Exception.evaluate (Text.length text) $> text
    void (MVar.tryPutMVar result output)
  pure result

readerText :: Text -> MVar.MVar (Either SomeException Text) -> IO Text
readerText label result = do
  outcome <- timeout processExitGraceMicroseconds (MVar.takeMVar result)
  pure case outcome of
    Nothing ->
      [i|Could not read #{label}: reader timed out.|]
    Just (Left err) ->
      [i|Could not read #{label}: #{show err :: String}|]
    Just (Right text) ->
      text

killProcessTree :: ProcessHandle -> IO ()
killProcessTree ph = do
  mPid <- getPid ph
  traverse_ killPid mPid
  where
    killPid pid =
      ignoreIO $
        signalProcessGroup sigKILL (fromIntegral pid)
          `Exception.catch` \(_ :: SomeException) ->
            signalProcess sigKILL pid

ignoreIO :: IO () -> IO ()
ignoreIO action =
  action `Exception.catch` \(_ :: SomeException) -> pure ()

formatBashResult :: Show exitCode => exitCode -> Text -> Text -> Text
formatBashResult exitCode stdoutText stderrText =
  Text.strip $ Text.unlines $ filter (not . Text.null)
    [ if Text.null stdoutText then "" else "stdout:\n" <> stdoutText
    , if Text.null stderrText then "" else "stderr:\n" <> stderrText
    , "exit code: " <> Text.pack (show exitCode)
    ]

processExitGraceMicroseconds :: Int
processExitGraceMicroseconds =
  5 * 1_000_000

data ScheduleSummary = ScheduleSummary
  { scheduleId :: !Integer
  , remainingSeconds :: !Int
  , prompt :: !Text
  }
  deriving (Show, Generic, Aeson.ToJSON)

scheduleSummary :: Scheduler.ScheduledMessage -> ScheduleSummary
scheduleSummary schedule =
  ScheduleSummary
    { scheduleId = schedule.scheduleId
    , remainingSeconds = schedule.remainingSeconds
    , prompt = scheduledPrompt schedule.message
    }

scheduledPrompt :: IncomingMessage -> Text
scheduledPrompt message =
  fromMaybe message.text (AesonTypes.parseMaybe parsePrompt message.raw)
  where
    parsePrompt =
      Aeson.withObject "scheduled action" (Aeson..: Key.fromText "prompt")

scheduledActionArgs :: Aeson.Value -> AesonTypes.Parser (Int, Text)
scheduledActionArgs =
  Aeson.withObject "scheduled action arguments" $ \o -> do
    delaySeconds <- o Aeson..: Key.fromText "delay_seconds"
    prompt <- o Aeson..: Key.fromText "prompt"
    pure (delaySeconds, prompt)

data MemoryAction
  = MemoryView
  | MemoryReplace
  | MemoryClear

data MemoryScope es = MemoryScope
  { missingMessage :: !Text
  , updatedMessage :: !Text
  , clearedMessage :: !Text
  , loadMemory :: Memory.MemoryConfig -> IncomingMessage -> Eff es (Maybe Text)
  , replaceMemory :: Memory.MemoryConfig -> IncomingMessage -> Text -> Eff es (Either Text ())
  , clearMemory :: Memory.MemoryConfig -> IncomingMessage -> Eff es (Either Text ())
  }

senderMemoryScope :: IOE :> es => MemoryScope es
senderMemoryScope = MemoryScope
  { missingMessage = "No memory is stored for the current sender."
  , updatedMessage = "Memory updated."
  , clearedMessage = "Memory cleared."
  , loadMemory = Memory.loadSenderMemory
  , replaceMemory = Memory.replaceSenderMemory
  , clearMemory = Memory.clearSenderMemory
  }

chatMemoryScope :: IOE :> es => MemoryScope es
chatMemoryScope = MemoryScope
  { missingMessage = "No memory is stored for the current chat."
  , updatedMessage = "Chat memory updated."
  , clearedMessage = "Chat memory cleared."
  , loadMemory = Memory.loadChatMemory
  , replaceMemory = Memory.replaceChatMemory
  , clearMemory = Memory.clearChatMemory
  }

memoryArgs :: Aeson.Value -> AesonTypes.Parser (MemoryAction, Maybe Text)
memoryArgs =
  Aeson.withObject "memory arguments" $ \o -> do
    actionText <- Text.toLower . Text.strip <$> o Aeson..: Key.fromText "action"
    memory <- fmap Text.strip <$> o Aeson..:? Key.fromText "memory"
    action <- case actionText of
      "view" ->
        pure MemoryView
      "replace" ->
        pure MemoryReplace
      "clear" ->
        pure MemoryClear
      _ ->
        fail "action must be one of: view, replace, clear"
    when (actionText == "replace" && maybe True Text.null memory) do
      fail "memory is required when action is replace"
    pure (action, memory)

runMemoryAction :: IOE :> es => MemoryScope es -> Memory.MemoryConfig -> AgentContext es -> MemoryAction -> Maybe Text -> Eff es ToolResult
runMemoryAction scope cfg context action memory =
  case action of
    MemoryView -> do
      current <- scope.loadMemory cfg context.message
      pure (toolText (fromMaybe scope.missingMessage current))
    MemoryReplace ->
      case memory of
        Nothing ->
          pure (toolText "memory is required when action is replace")
        Just content
          | not context.superuser && Text.length content > Memory.memoryLimitChars ->
              pure (toolText [i|Memory update rejected: memory is #{Text.length content} characters, over the #{Memory.memoryLimitChars} character limit. Please summarize it more concisely and try again.|])
          | otherwise -> do
              result <- scope.replaceMemory cfg context.message content
              pure (toolText (either identity (const scope.updatedMessage) result))
    MemoryClear -> do
      result <- scope.clearMemory cfg context.message
      pure (toolText (either identity (const scope.clearedMessage) result))

scheduledAgentMessage :: AgentContext es -> Int -> Text -> IncomingMessage
scheduledAgentMessage context delaySeconds prompt =
  let original = context.message
      commandText = context.askCommand <> " " <> prompt
  in original
      { messageId = original.messageId
      , replyToMessageId = Nothing
      , mentions = original.mentions
      , mentionUsernames = original.mentionUsernames
      , imageUrls = []
      , text = commandText
      , raw = Aeson.object
          [ "type" Aeson..= Aeson.String "scheduled_agent_action"
          , "delay_seconds" Aeson..= delaySeconds
          , "prompt" Aeson..= prompt
          , "original_message" Aeson..= original.raw
          ]
      }

withTextArg :: Text -> (Text -> Eff es ToolResult) -> Aeson.Value -> Eff es ToolResult
withTextArg key action =
  either (pure . toolText . Text.pack) action . AesonTypes.parseEither parser
  where
    parser = Aeson.withObject "tool arguments" (Aeson..: Key.fromText key)

withIntegerArg :: Text -> (Integer -> Eff es ToolResult) -> Aeson.Value -> Eff es ToolResult
withIntegerArg key action =
  either (pure . toolText . Text.pack) action . AesonTypes.parseEither parser
  where
    parser = Aeson.withObject "tool arguments" (Aeson..: Key.fromText key)

queryChatLogArgs :: Aeson.Value -> AesonTypes.Parser (Integer, Bool)
queryChatLogArgs =
  Aeson.withObject "query chat log arguments" $ \o -> do
    limit <- o Aeson..: Key.fromText "limit"
    includeBotMessages <- fromMaybe False <$> o Aeson..:? Key.fromText "include_bot_messages"
    pure (limit, includeBotMessages)

webSearchArgs :: Maybe Int -> Aeson.Value -> AesonTypes.Parser (Text, Int)
webSearchArgs configuredDefault =
  Aeson.withObject "web search arguments" $ \o -> do
    query <- Text.strip <$> o Aeson..: Key.fromText "query"
    maxResults <- fromMaybe (fromMaybe 5 configuredDefault) <$> o Aeson..:? Key.fromText "max_results"
    when (Text.null query) do
      fail "query must not be empty."
    when (maxResults <= 0) do
      fail "max_results must be positive."
    pure (query, min 20 maxResults)

webFetchArgs :: Maybe Int -> Aeson.Value -> AesonTypes.Parser (Text, Int)
webFetchArgs configuredDefault =
  Aeson.withObject "web fetch arguments" $ \o -> do
    url <- Text.strip <$> o Aeson..: Key.fromText "url"
    maxContentTokens <- fromMaybe (fromMaybe 50000 configuredDefault) <$> o Aeson..:? Key.fromText "max_content_tokens"
    when (Text.null url) do
      fail "url must not be empty."
    when (maxContentTokens <= 0) do
      fail "max_content_tokens must be positive."
    pure (url, min 200000 maxContentTokens)

runBashArgs :: Aeson.Value -> AesonTypes.Parser (Text, Int)
runBashArgs =
  Aeson.withObject "run bash arguments" $ \o -> do
    script <- o Aeson..: Key.fromText "script"
    timeoutSeconds <- fromMaybe 30 <$> o Aeson..:? Key.fromText "timeout_seconds"
    when (timeoutSeconds <= 0) do
      fail "timeout_seconds must be positive."
    pure (script, timeoutSeconds)

mentionUserArgs :: Aeson.Value -> AesonTypes.Parser (Integer, Text)
mentionUserArgs =
  Aeson.withObject "mention user arguments" $ \o -> do
    userId <- o Aeson..: Key.fromText "user_id"
    text <- o Aeson..: Key.fromText "text"
    pure (userId, text)

sendReplyArgs :: Aeson.Value -> AesonTypes.Parser Text
sendReplyArgs =
  Aeson.withObject "send reply arguments" $ \o -> do
    text <- Text.strip . fromMaybe "" <$> o Aeson..:? Key.fromText "text"
    imageUrls <- map Text.strip . fromMaybe [] <$> o Aeson..:? Key.fromText "image_urls"
    let body = replyBodyWithImages text (filter (not . Text.null) imageUrls)
    when (Text.null body) do
      fail "Either text or image_urls must be provided."
    pure body

replyBodyWithImages :: Text -> [Text] -> Text
replyBodyWithImages text imageUrls =
  Text.strip $ Text.unlines $
    [ text | not (Text.null text) ]
      <> map ReplyBody.imageDirective imageUrls

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

fieldText :: Text -> Text -> (Text, Aeson.Value)
fieldText name description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "string"
      , "description" Aeson..= description
      ]
  )

fieldTextArray :: Text -> Text -> (Text, Aeson.Value)
fieldTextArray name description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "array"
      , "items" Aeson..= Aeson.object
          [ "type" Aeson..= Aeson.String "string"
          ]
      , "description" Aeson..= description
      ]
  )

fieldInteger :: Text -> Text -> (Text, Aeson.Value)
fieldInteger name description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "integer"
      , "minimum" Aeson..= (0 :: Int)
      , "description" Aeson..= description
      ]
  )

fieldBoolean :: Text -> Text -> (Text, Aeson.Value)
fieldBoolean name description =
  ( name
  , Aeson.object
      [ "type" Aeson..= Aeson.String "boolean"
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
