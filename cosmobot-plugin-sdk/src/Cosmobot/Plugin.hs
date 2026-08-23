{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A standalone SDK for executable cosmobot plugins.
--
-- 'Plugin' deliberately has no 'Monad' instance: declarations are independent
-- and are accumulated applicatively before they are published to the host.
module Cosmobot.Plugin
  ( Plugin
  , Handler
  , Capability (..)
  , Declaration (..)
  , RouteDeclaration (..)
  , ToolDeclaration (..)
  , CommandInvocation (..)
  , MediaResult (..)
  , Arguments
  , command
  , tool
  , text
  , optionalText
  , declarations
  , validationErrors
  , serve
  , serveWith
  , reply
  , referencedMessage
  , complete
  , runAgent
  , resolveMedia
  , io
  , StartupFailure (..)
  , transientStartup
  , transientProcessFailure
  ) where

import Control.Concurrent (ThreadId, forkFinally, killThread, myThreadId, throwTo)
import Control.Concurrent.MVar
import Control.Exception (AsyncException, Exception, SomeException, catch, displayException, finally, fromException, mask, onException, throwIO, try)
import Control.Monad (unless, when)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LazyBS
import Data.Char qualified as Char
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import GHC.Generics (Generic)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (BufferMode (LineBuffering), Handle, hFlush, hSetBuffering, stderr, stdin, stdout)
import System.IO.Error (eofErrorType, mkIOError)
import System.Timeout qualified as Timeout

data Capability = Chat | LLM | Agent | Media
  deriving stock (Eq, Ord, Show, Generic)

instance ToJSON Capability where
  toJSON = String . \case
    Chat -> "chat"
    LLM -> "llm"
    Agent -> "agent"
    Media -> "media"

data CommandInvocation = CommandInvocation
  { invocationId :: Text
  , arguments :: Text
  , message :: Value
  }
  deriving stock (Eq, Show)

data MediaResult = MediaResult
  { canonicalReference :: Text
  , mimeType :: Maybe Text
  , size :: Maybe Integer
  , publicUrl :: Maybe Text
  , localPath :: Maybe FilePath
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON MediaResult

data RouteDeclaration = RouteDeclaration
  { routeId :: Text
  , routeHelp :: Text
  , routeCommand :: Text
  , routeAccess :: Text
  , routeDisposition :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON RouteDeclaration where
  toJSON route =
    object
      [ "id" .= routeId route
      , "help" .= object ["label" .= routeCommand route, "description" .= routeHelp route]
      , "filter" .= (routeId route <> ".filter")
      , "access" .= routeAccess route
      , "disposition" .= routeDisposition route
      ]

data ToolDeclaration = ToolDeclaration
  { toolName :: Text
  , toolDescription :: Text
  , toolSchema :: Value
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ToolDeclaration where
  toJSON definition =
    object
      [ "name" .= toolName definition
      , "description" .= toolDescription definition
      , "schema" .= toolSchema definition
      ]

data Declaration
  = Route RouteDeclaration
  | Tool ToolDeclaration
  deriving stock (Eq, Show)

data Runnable
  = RunnableRoute (CommandInvocation -> Handler Text)
  | forall a. RunnableTool (Arguments a) (a -> Handler Text)

data Entry = Entry Declaration Runnable

data Build a = Build
  { buildValue :: a
  , buildEntries :: [Entry]
  , buildErrors :: [Text]
  }

newtype Plugin a = Plugin {unPlugin :: Build a}

instance Functor Plugin where
  fmap f (Plugin built) = Plugin built {buildValue = f (buildValue built)}

instance Applicative Plugin where
  pure value = Plugin (Build value [] [])
  Plugin f <*> Plugin x =
    Plugin
      Build
        { buildValue = buildValue f (buildValue x)
        , buildEntries = buildEntries f <> buildEntries x
        , buildErrors = buildErrors f <> buildErrors x
        }

declarations :: Plugin a -> [Declaration]
declarations = map entryDeclaration . buildEntries . unPlugin
  where
    entryDeclaration (Entry public _) = public

validationErrors :: Plugin a -> [Text]
validationErrors plugin = buildErrors built <> duplicateErrors (declarations plugin)
  where
    built = unPlugin plugin
    duplicateErrors values = duplicate "route" [routeId x | Route x <- values] <> duplicate "tool" [toolName x | Tool x <- values]
    duplicate kind = map (("duplicate " <> kind <> ": ") <>) . duplicates
    duplicates xs = Map.keys . Map.filter (> (1 :: Int)) $ Map.fromListWith (+) [(x, 1) | x <- xs]

command :: Text -> Text -> (CommandInvocation -> Handler Text) -> Plugin ()
command name help handler =
  declaration errors (Route route) (RunnableRoute handler)
  where
    normalized = fromMaybe name (Text.stripPrefix "!" name)
    errors =
      ["command must start with !" | not ("!" `Text.isPrefixOf` name)]
        <> required "command" normalized
        <> identifierErrors "command" normalized
        <> required "command help" help
    route = RouteDeclaration normalized help ("!" <> normalized) "allowed" "stop"

data Arguments a = Arguments
  { argumentFields :: [(Text, Value, Bool)]
  , parseArguments :: Object -> Either Text a
  }

instance Functor Arguments where
  fmap f spec = spec {parseArguments = fmap f . parseArguments spec}

instance Applicative Arguments where
  pure value = Arguments [] (const (Right value))
  f <*> x =
    Arguments
      (argumentFields f <> argumentFields x)
      (\objectValue -> parseArguments f objectValue <*> parseArguments x objectValue)

text :: Text -> Arguments Text
text name = Arguments [(name, stringSchema, True)] $ \values ->
  case KeyMap.lookup (Key.fromText name) values of
    Just (String value) -> Right value
    Just _ -> Left (name <> " must be a string")
    Nothing -> Left ("missing argument: " <> name)

optionalText :: Text -> Arguments (Maybe Text)
optionalText name = Arguments [(name, stringSchema, False)] $ \values ->
  case KeyMap.lookup (Key.fromText name) values of
    Just (String value) -> Right (Just value)
    Just Null -> Right Nothing
    Just _ -> Left (name <> " must be a string")
    Nothing -> Right Nothing

stringSchema :: Value
stringSchema = object ["type" .= String "string"]

tool :: Text -> Text -> Arguments a -> (a -> Handler Text) -> Plugin ()
tool name description argumentsSpec handler =
  declaration errors (Tool definition) (RunnableTool argumentsSpec handler)
  where
    errors = required "tool name" name <> identifierErrors "tool name" name <> required "tool description" description <> argumentErrors argumentsSpec
    definition = ToolDeclaration name description (argumentsSchema argumentsSpec)

argumentsSchema :: Arguments a -> Value
argumentsSchema spec =
  object
    [ "type" .= String "object"
    , "properties" .= Object (KeyMap.fromList [(Key.fromText name, schema) | (name, schema, _) <- argumentFields spec])
    , "required" .= [name | (name, _, True) <- argumentFields spec]
    , "additionalProperties" .= False
    ]

argumentErrors :: Arguments a -> [Text]
argumentErrors spec =
  concatMap (required "tool argument" . fieldName) (argumentFields spec)
    <> ["duplicate tool argument: " <> name | name <- Map.keys counts, counts Map.! name > (1 :: Int)]
  where
    fieldName (name, _, _) = name
    counts = Map.fromListWith (+) [(name, 1) | (name, _, _) <- argumentFields spec]

required :: Text -> Text -> [Text]
required label value = [label <> " must not be empty" | Text.null (Text.strip value)]

identifierErrors :: Text -> Text -> [Text]
identifierErrors label value =
  [label <> " contains invalid characters: " <> value | not (Text.null value) && not (Text.all valid value)]
    <> [label <> " contains reserved separator __: " <> value | "__" `Text.isInfixOf` value]
  where
    valid character = Char.isAscii character && Char.isAlphaNum character || character == '_' || character == '-'

declaration :: [Text] -> Declaration -> Runnable -> Plugin ()
declaration errors public runnable = Plugin (Build () [Entry public runnable] errors)

data Environment = Environment
  { runtime :: Runtime
  , currentInvocation :: Text
  , invocationAlive :: MVar Bool
  }

newtype Handler a = Handler (Environment -> IO a)

instance Functor Handler where
  fmap f (Handler action) = Handler (fmap f . action)

instance Applicative Handler where
  pure value = Handler (const (pure value))
  Handler f <*> Handler x = Handler $ \environment -> f environment <*> x environment

instance Monad Handler where
  Handler action >>= next = Handler $ \environment -> do
    value <- action environment
    let Handler continuation = next value
    continuation environment

data Runtime = Runtime
  { outputLock :: MVar ()
  , pending :: MVar (Map Integer (MVar (Either RpcFailure Value)))
  , nextRequestId :: MVar Integer
  , capabilities :: Set Capability
  , initialized :: MVar Bool
  , activeHandlers :: MVar (Map ThreadId (MVar ()))
  , registrations :: MVar [Entry]
  , serverThread :: ThreadId
  }

data RpcFailure = RpcFailure Int Text
  deriving stock (Show)

instance Exception RpcFailure

data StartupFailure = TransientStartup Text
  deriving stock (Eq, Show)

instance Exception StartupFailure

newtype ProcessFailure = ProcessFailure Text
  deriving stock (Show)

instance Exception ProcessFailure

-- | Abort initialization and ask the host supervisor to retry this process.
transientStartup :: Text -> IO a
transientStartup = throwIO . TransientStartup

-- | Report an explicitly retryable process failure and exit with EX_TEMPFAIL.
transientProcessFailure :: Text -> IO a
transientProcessFailure = throwIO . ProcessFailure

reply :: Text -> Handler Value
reply body = host Chat "chat.reply" (object ["text" .= body])

referencedMessage :: Handler Value
referencedMessage = host Chat "chat.referenced" Null

complete :: Text -> Handler Text
complete prompt = host LLM "llm.complete" (object ["prompt" .= prompt]) >>= decodeResult

runAgent :: Text -> Handler Text
runAgent prompt = host Agent "agent.run" (object ["prompt" .= prompt]) >>= decodeResult

resolveMedia :: Text -> Handler MediaResult
resolveMedia reference = host Media "media.resolve" (object ["ref" .= reference]) >>= decodeResult

decodeResult :: FromJSON a => Value -> Handler a
decodeResult value = case fromJSON value of
  Error problem -> handlerIO (throwIO (RpcFailure (-32603) (Text.pack problem)))
  Success result -> pure result

host :: Capability -> Text -> Value -> Handler Value
host capability method params = Handler $ \environment -> do
  alive <- readMVar (invocationAlive environment)
  unless alive (throwIO (RpcFailure (-32001) "invocation has expired"))
  unless (capability `Set.member` capabilities (runtime environment)) $
    throwIO (RpcFailure (-32601) "capability was not declared")
  call (runtime environment) method (addInvocation (currentInvocation environment) params)

handlerIO :: IO a -> Handler a
handlerIO action = Handler (const action)

-- | Lift native plugin state or synchronization into an invocation handler.
-- Plugins remain responsible for synchronizing their own mutable state.
io :: IO a -> Handler a
io = handlerIO

addInvocation :: Text -> Value -> Value
addInvocation identifier (Object values) = Object (KeyMap.insert "invocationId" (String identifier) values)
addInvocation identifier value = object ["invocationId" .= identifier, "value" .= value]

serve :: Text -> [Capability] -> Plugin () -> IO ()
serve version requested plugin =
  runServer version requested (validatePlugin plugin) (pure ())
    `catch` exitAfterProcessFailure (pure ())

serveWith :: Text -> [Capability] -> IO state -> (state -> IO ()) -> (state -> Plugin ()) -> IO ()
serveWith version requested initialize finalize declare = do
  state <- newMVar Nothing
  finalized <- newMVar False
  let acquire = mask $ \restore -> do
        value <- restore initialize
        modifyMVar_ state (const (pure (Just value)))
        restore (validatePlugin (declare value))
      finalizeOnce = do
        shouldFinalize <- modifyMVar finalized (\done -> pure (True, not done))
        when shouldFinalize (readMVar state >>= mapM_ finalize)
  (runServer version requested acquire finalizeOnce `catch` exitAfterProcessFailure finalizeOnce)
    `finally` finalizeOnce

exitAfterProcessFailure :: IO () -> ProcessFailure -> IO a
exitAfterProcessFailure finalizePlugin (ProcessFailure messageText) = do
  _ <- try finalizePlugin :: IO (Either SomeException ())
  _ <- try (TextIO.hPutStrLn stderr messageText >> hFlush stderr) :: IO (Either SomeException ())
  exitWith (ExitFailure 75)

validatePlugin :: Plugin () -> IO [Entry]
validatePlugin plugin = do
  let errors = validationErrors plugin
  unless (null errors) (throwIO (userError (Text.unpack (Text.intercalate "; " errors))))
  pure (buildEntries (unPlugin plugin))

runServer :: Text -> [Capability] -> IO [Entry] -> IO () -> IO ()
runServer version requested initializePlugin finalizePlugin = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  mainThread <- myThreadId
  runtimeValue <- Runtime <$> newMVar () <*> newMVar Map.empty <*> newMVar 1 <*> pure (Set.fromList requested) <*> newMVar False <*> newMVar Map.empty <*> newMVar [] <*> pure mainThread
  loop runtimeValue BS.empty `finally` cancelHandlers runtimeValue
  where
    loop runtimeValue buffered = do
      (line, remaining) <- readFrame stdin buffered
      case eitherDecodeStrict' line of
        Left problem -> writeValue runtimeValue (rpcError Null (-32700) (Text.pack problem)) >> loop runtimeValue remaining
        Right value -> do
          shouldContinue <- dispatch version requested initializePlugin finalizePlugin runtimeValue value
          when shouldContinue (loop runtimeValue remaining)

readFrame :: Handle -> BS.ByteString -> IO (BS.ByteString, BS.ByteString)
readFrame handle = consume [] 0
  where
    consume chunks total chunk = case BS.elemIndex 10 chunk of
      Just newline -> do
        let frameLength = total + newline + 1
        when (frameLength > maxLineBytes) frameTooLarge
        pure (BS.concat (reverse (BS.take newline chunk : chunks)), BS.drop (newline + 1) chunk)
      Nothing -> do
        let newTotal = total + BS.length chunk
            newChunks = if BS.null chunk then chunks else chunk : chunks
        when (newTotal >= maxLineBytes) frameTooLarge
        next <- BS.hGetSome handle (min 32768 (maxLineBytes - newTotal))
        if BS.null next
          then if newTotal == 0
            then throwIO (mkIOError eofErrorType "readFrame" (Just handle) Nothing)
            else pure (BS.concat (reverse newChunks), BS.empty)
          else consume newChunks newTotal next
    frameTooLarge = throwIO (RpcFailure (-32700) "JSON-RPC line exceeds 1 MiB")

dispatch :: Text -> [Capability] -> IO [Entry] -> IO () -> Runtime -> Value -> IO Bool
dispatch version requested initializePlugin finalizePlugin runtimeValue = \case
  Object objectValue
    | field "jsonrpc" objectValue /= Just ("2.0" :: Text) ->
        writeValue runtimeValue (rpcError Null (-32600) "expected JSON-RPC 2.0") >> pure True
    | Just method <- field "method" objectValue -> do
        let requestId = KeyMap.lookup "id" objectValue
            params = fromMaybe Null (KeyMap.lookup "params" objectValue)
        case requestId of
          Nothing -> pure True
          Just identifier
            | method == "plugin.shutdown" -> cancelHandlers runtimeValue >> finalizePlugin >> respond runtimeValue identifier Null >> pure False
            | method == "plugin.initialize" -> do
                alreadyInitialized <- swapMVar (initialized runtimeValue) True
                if alreadyInitialized
                  then writeValue runtimeValue (rpcError identifier (-32600) "plugin already initialized") >> pure True
                  else attemptStartup initializePlugin >>= \case
                    Left (isTransient, messageText) ->
                      writeValue runtimeValue (startupError identifier isTransient messageText) >> pure False
                    Right entries -> do
                      modifyMVar_ (registrations runtimeValue) (const (pure entries))
                      respond runtimeValue identifier (manifest version requested entries)
                      pure True
            | otherwise -> do
                ready <- readMVar (initialized runtimeValue)
                if ready
                  then do
                    entries <- readMVar (registrations runtimeValue)
                    forkInvocation runtimeValue identifier (invoke entries runtimeValue method params)
                  else writeValue runtimeValue (rpcError identifier (-32002) "plugin is not initialized")
                pure True
    | Just identifier <- KeyMap.lookup "id" objectValue
    , KeyMap.member "result" objectValue || KeyMap.member "error" objectValue ->
        resolveResponse runtimeValue identifier objectValue >> pure True
  _ -> writeValue runtimeValue (rpcError Null (-32600) "invalid JSON-RPC message") >> pure True

field :: FromJSON a => Key -> Object -> Maybe a
field key values = KeyMap.lookup key values >>= parseMaybe parseJSON

attemptStartup :: IO a -> IO (Either (Bool, Text) a)
attemptStartup action = try action >>= \case
  Right value -> pure (Right value)
  Left exception
    | Just asyncException <- fromException exception -> throwIO (asyncException :: AsyncException)
    | Just processFailure <- fromException exception -> throwIO (processFailure :: ProcessFailure)
    | Just (TransientStartup messageText) <- fromException exception -> pure (Left (True, messageText))
    | otherwise -> pure (Left (False, Text.pack (displayException (exception :: SomeException))))

startupError :: Value -> Bool -> Text -> Value
startupError identifier isTransient messageText =
  object
    [ "jsonrpc" .= String "2.0"
    , "id" .= identifier
    , "error"
        .= object
          [ "code" .= (-32001 :: Int)
          , "message" .= messageText
          , "data" .= object ["transient" .= isTransient]
          ]
    ]

manifest :: Text -> [Capability] -> [Entry] -> Value
manifest version requested entries =
  object
    [ "protocolVersion" .= String "1.0.0"
    , "pluginVersion" .= version
    , "requestedCapabilities" .= requested
    , "routes" .= [route | Entry (Route route) _ <- entries]
    , "filters"
        .= Object
          ( KeyMap.fromList
              [ (Key.fromText (routeId route <> ".filter"), object ["command" .= routeCommand route])
              | Entry (Route route) _ <- entries
              ]
          )
    , "tools" .= [definition | Entry (Tool definition) _ <- entries]
    ]

invoke :: [Entry] -> Runtime -> Text -> Value -> IO Value
invoke entries runtimeValue method params
  | method == "plugin.route.invoke" = do
      invocation <- parseOrThrow parseCommand params
      timeoutSeconds <- invocationTimeout params
      runnable <- findRoute entries =<< param "routeId" params
      runHandler runtimeValue (invocationId invocation) timeoutSeconds (runnable invocation)
  | method == "plugin.tool.invoke" = do
      identifier <- param "invocationId" params
      timeoutSeconds <- invocationTimeout params
      name <- param "tool" params
      values <- param "arguments" params
      runTool entries runtimeValue identifier timeoutSeconds name values
  | otherwise = throwIO (RpcFailure (-32601) ("unknown method: " <> method))

parseCommand :: Value -> Parser CommandInvocation
parseCommand = withObject "route invocation" $ \values ->
  CommandInvocation
    <$> values .: "invocationId"
    <*> values .:? "arguments" .!= ""
    <*> values .: "message"

param :: FromJSON a => Key -> Value -> IO a
param name = parseOrThrow (withObject "parameters" (.: name))

parseOrThrow :: (Value -> Parser a) -> Value -> IO a
parseOrThrow parser value = case parseEither parser value of
  Left problem -> throwIO (RpcFailure (-32602) (Text.pack problem))
  Right result -> pure result

invocationTimeout :: Value -> IO Int
invocationTimeout params = do
  seconds <- param "timeoutSeconds" params
  if seconds > 0 && seconds <= maxBound `div` 1000000
    then pure (seconds * 1000000)
    else throwIO (RpcFailure (-32602) "timeoutSeconds must be a positive integer")

findRoute :: [Entry] -> Text -> IO (CommandInvocation -> Handler Text)
findRoute entries identifier =
  case [handler | Entry (Route route) (RunnableRoute handler) <- entries, routeId route == identifier] of
    handler : _ -> pure handler
    [] -> throwIO (RpcFailure (-32601) ("unknown route: " <> identifier))

runTool :: [Entry] -> Runtime -> Text -> Int -> Text -> Object -> IO Value
runTool entries runtimeValue identifier timeoutMicroseconds name values =
  case [runnable | Entry (Tool definition) runnable <- entries, toolName definition == name] of
    RunnableTool spec handler : _ ->
      case parseArguments spec values of
        Left problem -> throwIO (RpcFailure (-32602) problem)
        Right argumentsValue -> runHandler runtimeValue identifier timeoutMicroseconds (handler argumentsValue)
    _ -> throwIO (RpcFailure (-32601) ("unknown tool: " <> name))

runHandler :: Runtime -> Text -> Int -> Handler Text -> IO Value
runHandler runtimeValue identifier timeoutMicroseconds (Handler action) = do
  alive <- newMVar True
  let environment = Environment runtimeValue identifier alive
  result <- Timeout.timeout timeoutMicroseconds (action environment) `finally` modifyMVar_ alive (const (pure False))
  case result of
    Nothing -> throwIO (RpcFailure (-32000) "plugin invocation timed out")
    Just content -> pure (object ["status" .= String "success", "content" .= content, "imageUrls" .= ([] :: [Text])])

finish :: Runtime -> Value -> Either SomeException Value -> IO ()
finish runtimeValue identifier = \case
  Right result -> respond runtimeValue identifier result
  Left exception
    | Just (_ :: AsyncException) <- fromException exception -> pure ()
    | Just (RpcFailure code messageText) <- fromException exception -> writeValue runtimeValue (rpcError identifier code messageText)
    | otherwise -> writeValue runtimeValue (rpcError identifier (-32000) (Text.pack (displayException exception)))

forkInvocation :: Runtime -> Value -> IO Value -> IO ()
forkInvocation runtimeValue requestId action = do
  thread <- newEmptyMVar
  done <- newEmptyMVar
  threadId <- forkFinally (readMVar thread >> action) $ \result -> do
    let cleanup = do
          ownThread <- readMVar thread
          modifyMVar_ (activeHandlers runtimeValue) (pure . Map.delete ownThread)
          putMVar done ()
    case result of
      Left exception
        | Just processFailure <- fromException exception ->
            cleanup >> throwTo (serverThread runtimeValue) (processFailure :: ProcessFailure)
      _ -> finish runtimeValue requestId result `finally` cleanup
  modifyMVar_ (activeHandlers runtimeValue) (pure . Map.insert threadId done)
  putMVar thread threadId

cancelHandlers :: Runtime -> IO ()
cancelHandlers runtimeValue = do
  handlers <- readMVar (activeHandlers runtimeValue)
  mapM_ killThread (Map.keys handlers)
  mapM_ readMVar (Map.elems handlers)

respond :: Runtime -> Value -> Value -> IO ()
respond runtimeValue identifier result = writeValue runtimeValue (object ["jsonrpc" .= String "2.0", "id" .= identifier, "result" .= result])

rpcError :: Value -> Int -> Text -> Value
rpcError identifier code messageText =
  object
    [ "jsonrpc" .= String "2.0"
    , "id" .= identifier
    , "error" .= object (["code" .= code, "message" .= messageText] <> details)
    ]
  where
    details = case code of
      -32602 -> ["data" .= object ["kind" .= String "permanent_argument"]]
      -32000 -> ["data" .= object ["kind" .= String "transient"]]
      _ -> []

writeValue :: Runtime -> Value -> IO ()
writeValue runtimeValue value = do
  let encoded = LazyBS.toStrict (encode value)
  when (BS.length encoded + 1 > maxLineBytes) (throwIO (RpcFailure (-32603) "JSON-RPC line exceeds 1 MiB"))
  withMVar (outputLock runtimeValue) $ \_ -> BS8.putStrLn encoded

maxLineBytes :: Int
maxLineBytes = 1024 * 1024

call :: Runtime -> Text -> Value -> IO Value
call runtimeValue method params = do
  identifier <- modifyMVar (nextRequestId runtimeValue) $ \next -> pure (next + 1, next)
  response <- newEmptyMVar
  modifyMVar_ (pending runtimeValue) (pure . Map.insert identifier response)
  let forget = modifyMVar_ (pending runtimeValue) (pure . Map.delete identifier)
  writeValue runtimeValue (object ["jsonrpc" .= String "2.0", "id" .= identifier, "method" .= method, "params" .= params]) `onException` forget
  (takeMVar response >>= either throwIO pure) `onException` forget

resolveResponse :: Runtime -> Value -> Object -> IO ()
resolveResponse runtimeValue identifier values = case fromJSON identifier of
  Error _ -> pure ()
  Success numericId -> do
    waiter <- modifyMVar (pending runtimeValue) $ \waiters -> pure (Map.delete numericId waiters, Map.lookup numericId waiters)
    case waiter of
      Nothing -> pure ()
      Just target -> putMVar target $ case KeyMap.lookup "result" values of
        Just result -> Right result
        Nothing -> Left (decodeRpcError (KeyMap.lookup "error" values))

decodeRpcError :: Maybe Value -> RpcFailure
decodeRpcError (Just (Object values)) = RpcFailure (maybe (-32000) id (field "code" values)) (maybe "host call failed" id (field "message" values))
decodeRpcError _ = RpcFailure (-32000) "host call failed"
