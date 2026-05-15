{-|
Module      : Bot.Agent.Tools.Web
Description : Agent web search and fetch tools
Stability   : experimental
-}

module Bot.Agent.Tools.Web
  ( webSearchTool
  , webFetchTool
  )
where

import Bot.Agent.Tools.Common
import Bot.Agent.Types
import qualified Bot.Util.Html as Html
import Bot.Prelude
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncoding
import Network.HTTP.Req
import System.IO.Error (userError)
import qualified Text.URI as URI

webSearchTool :: IOE :> es => Tool es
webSearchTool = Tool
  { name = "web_search"
  , description = "Search the web for current information. Returns a JSON object with the query and a results array containing title, url, and snippet."
  , parameters = objectSchema
      [ fieldText "query" "Search query."
      , fieldInteger "max_results" "Maximum number of results to return. Defaults to 5 and is capped at 20."
      ]
      ["query"]
  , noisy = False
  , allowed = \context -> context.toolConfig.webSearchEnable
  , start = \context -> pure \args ->
      withParsedToolArgs (webSearchArgs context.toolConfig.webSearchMaxResults) args \(query, maxResults) -> do
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
  , noisy = False
  , allowed = \context -> context.toolConfig.webFetch
  , start = \context -> do
      checkUseLimit <- newUseLimiter context.toolConfig.webFetchMaxUses
      pure \args ->
        withParsedToolArgs (webFetchArgs context.toolConfig.webFetchMaxContentTokens) args \(url, maxContentTokens) -> do
          checkUseLimit >>= \case
            UseLimitReached currentUses ->
              pure (toolText [i|web_fetch use limit reached for this agent run: #{currentUses}.|])
            UseAllowed -> do
              page <- liftIO (fetchWebPage url maxContentTokens)
              pure (toolText (jsonText page))
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

webSearchSource :: WebSearchApi -> Text
webSearchSource = \case
  WebSearchTavily -> "tavily"
  WebSearchBrave -> "brave"

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
          body = Html.htmlToPlainText (decodeResponseBody (responseBody response))
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
