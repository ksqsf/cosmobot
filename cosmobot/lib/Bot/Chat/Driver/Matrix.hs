{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Bot.Chat.Driver.Matrix
Description : Matrix ChatDriver implementation
Stability   : experimental
-}

module Bot.Chat.Driver.Matrix
  ( MatrixDriver
  , Config (..)
  , newMatrixDriver
  , chatHandler
  , runMatrixClient
  , incomingMessages
  , SyncResponse (..)
  , JoinedRoom (..)
  , Timeline (..)
  , Event (..)
  , EventContent (..)
  , SendMessageResponse (..)
  , RoomEvent (..)
  , eventToIncomingMessage
  , eventToIncomingMessageWith
  , syncInvitedRoomIds
  , matrixReferencedMessage
  , decryptMatrixEncryptedBytesForTest
  , formatMatrixMarkdown
  , formatMatrixMarkdownWithMentionNames
  )
where

import Bot.Core.Message
import Bot.Chat.Driver.Matrix.Markdown
import qualified Bot.Chat.Driver.Matrix.Protocol as Protocol
import Bot.Chat.Driver.Matrix.Protocol hiding (MatrixDriver, newMatrixDriver, runMatrixClient)
import Bot.Chat.Driver.Matrix.Types (Config (..))
import qualified Bot.Chat.Driver.Types as Driver
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.HTTP as HTTP
import qualified Bot.Effect.Matrix as Matrix
import qualified Bot.Effect.Media as Media
import qualified Bot.Effect.Storage as Storage
import qualified Bot.Media.Mime as Mime
import qualified Bot.Storage.Matrix as MatrixStorage
import Bot.Prelude
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Crypto.Cipher.AES as CryptoAES
import qualified Crypto.Cipher.Types as CryptoCipher
import qualified Crypto.Error as CryptoError
import qualified Crypto.Hash as CryptoHash
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem as FileSystem
import qualified Effectful.Prim.IORef as IORef
import qualified Effectful.Temporary as Temporary
import qualified Streaming as S
import qualified Streaming.ByteString as Q
import qualified Streaming.Prelude as SP
import qualified Streaming.Prelude as S
import System.FilePath ((</>), (<.>), takeFileName)

newtype MatrixDriver = MatrixDriver Protocol.MatrixDriver

newMatrixDriver
  :: (Concurrent :> es, Prim :> es)
  => Config
  -> Eff es MatrixDriver
newMatrixDriver =
  fmap MatrixDriver . Protocol.newMatrixDriver

instance Driver.ChatDriver MatrixDriver where
  type ChatDriverEffects MatrixDriver es =
    (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es, Storage.Storage :> es)

  driverPlatform _ =
    PlatformMatrix

  sendReplyMessage (MatrixDriver driver) =
    replyToMatrix driver

  sendReplyMessages (MatrixDriver driver) =
    replyToMatrixMessages driver

  sendStreamingReplyMessage (MatrixDriver driver) =
    streamingReplyToMatrix driver

  replyAudio (MatrixDriver driver) =
    replyAudioMatrix driver

  uploadFile (MatrixDriver driver) =
    uploadFileMatrix driver

  editMessage (MatrixDriver driver) =
    editMessageMatrix driver

  completeMessageEdit (MatrixDriver driver) =
    completeMessageEditMatrix driver

  deleteMessage (MatrixDriver driver) =
    deleteMessageMatrix driver

  messageOutPolicy _ _ =
    pure (Chat.EditableMessage matrixEditChunkChars matrixStreamingMessageLimit)

  getMessageContent (MatrixDriver driver) =
    getMessageContentMatrix driver

  getSenderMemberInfo (MatrixDriver driver) =
    getSenderMemberInfoMatrix driver

  getMemberInfo (MatrixDriver driver) =
    getMemberInfoMatrix driver

  getUserAvatar (MatrixDriver driver) =
    getUserAvatarMatrix driver

  listGroupMembers (MatrixDriver driver) =
    listGroupMembersMatrix driver

  normalizeMediaRef (MatrixDriver driver) =
    normalizeMatrixMediaRef driver Nothing

  mentionUser (MatrixDriver driver) =
    mentionUserMatrix driver

  setTyping (MatrixDriver driver) message timeoutMs =
    case viaNonEmpty head message.chatAliases of
      Just roomId -> typing driver (matrixRoomId roomId) timeoutMs
      Nothing -> pure ()

chatHandler
  :: Driver.ChatDriverEffects MatrixDriver es
  => MatrixDriver
  -> Chat.ChatHandler es
chatHandler =
  Chat.chatDriverHandler

runMatrixClient
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Maybe MatrixDriver
  -> Eff (Matrix.Matrix : es) a
  -> Eff es a
runMatrixClient driver =
  Protocol.runMatrixClient (unwrap <$> driver)
  where
    unwrap (MatrixDriver protocolDriver) = protocolDriver

incomingMessages
  :: (HTTP.HTTP :> es, Media.Media :> es, KatipE :> es, IOE :> es, Concurrent :> es, Prim :> es, Storage.Storage :> es)
  => MatrixDriver
  -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessages (MatrixDriver driver) =
  incomingMessagesProtocol driver
matrixStreamingMessageLimit :: Int
matrixStreamingMessageLimit = 4000

matrixEditChunkChars :: Int
matrixEditChunkChars = 128

matrixEventMessageId :: MatrixEventId -> MessageId
matrixEventMessageId =
  textMessageId . matrixEventIdText

loadSyncToken :: Storage.Storage :> es => Eff es (Maybe Text)
loadSyncToken =
  MatrixStorage.loadSyncToken

storeSyncToken :: Storage.Storage :> es => Text -> Eff es ()
storeSyncToken =
  MatrixStorage.saveSyncToken

sync :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> Maybe Text -> Eff es (Maybe SyncResponse)
sync driver since = do
  response <- call driver MatrixSync
    { syncSince = since
    , syncMode = matrixSyncMode since
    }
  acceptInvitations driver response
  rememberMatrixRoomState driver.directRoomIds driver.joinedMemberCounts response
  pure (Just response)

matrixSyncMode :: Maybe Text -> MatrixSyncMode
matrixSyncMode = \case
  Nothing ->
    MatrixInitialSync
  Just{} ->
    MatrixLongPollSync

directRooms :: Prim :> es => Protocol.MatrixDriver -> Eff es (Set MatrixRoomId)
directRooms driver =
  IORef.readIORef driver.directRoomIds

joinedMemberCounts :: Prim :> es => Protocol.MatrixDriver -> Eff es (Map MatrixRoomId Int)
joinedMemberCounts driver =
  IORef.readIORef driver.joinedMemberCounts

joinedMemberCount :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> MatrixRoomId -> Eff es (Maybe Int)
joinedMemberCount driver roomId = do
  maybeCall driver (MatrixJoinedMembers roomId) >>= \case
    Nothing ->
      pure Nothing
    Just response -> do
      let count = Map.size response.joinedMembers
      rememberJoinedMemberCount driver.directRoomIds driver.joinedMemberCounts roomId count
      pure (Just count)

sendText :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> MatrixRoomId -> Maybe MatrixReplyTo -> Text -> Eff es (Either Text SendMessageResponse)
sendText driver roomId replyToEventId body =
  sendTextWithMentionsStreamComplete driver roomId replyToEventId body [] True

sendTextWithMentions :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> MatrixRoomId -> Maybe MatrixReplyTo -> Text -> [Text] -> Eff es (Either Text SendMessageResponse)
sendTextWithMentions driver roomId replyToEventId body mentionUserIds =
  sendTextWithMentionsStreamComplete driver roomId replyToEventId body mentionUserIds True

sendTextWithMentionsStreamComplete :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> MatrixRoomId -> Maybe MatrixReplyTo -> Text -> [Text] -> Bool -> Eff es (Either Text SendMessageResponse)
sendTextWithMentionsStreamComplete driver roomId replyToEventId body mentionUserIds complete = do
  let mentions = matrixOutgoingMentionUserIds body mentionUserIds
  mentionNames <- fetchMatrixMentionNames driver roomId mentions
  let displayBody = matrixMentionDisplayBody mentionNames body
      request = SendMessageRequest
        { msgtype = "m.text"
        , body = nonEmptyMatrixBody displayBody
        , formattedBody = formatMatrixMarkdownWithMentionNames mentionNames body
        , replyRelation = replyToEventId
        , mentions = MatrixMentions mentions
        , streamMetadata = MatrixStreamMetadata complete
        }
  response <- eitherCall "send m.room.message" driver (MatrixSendMessage roomId request)
  traverse_ (rememberMatrixEvent driver.eventIds) response
  traverse_ (\sent -> rememberInitialStreamTextMessage driver.streamTextMessages sent.eventId body mentions mentionNames complete) response
  pure response

uploadMedia :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> FilePath -> Text -> Text -> Eff es MatrixUploadResponse
uploadMedia driver path fileName mime =
  call driver (MatrixUploadMedia path fileName mime)

sendFileMessage :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> Text -> Maybe MatrixReplyTo -> MatrixFileMessage -> Eff es (Either Text SendMessageResponse)
sendFileMessage driver roomId replyRelation message@MatrixFileMessage{msgtype = mediaMsgtype} = do
  response <- eitherCall [i|send #{mediaMsgtype}|] driver (MatrixSendFile roomId replyRelation message)
  traverse_ (rememberMatrixEvent driver.eventIds) response
  pure response

editText :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> MatrixRoomId -> MatrixEventId -> Text -> Eff es (Either Text SendMessageResponse)
editText driver roomId eventId body = do
  let mentions = matrixOutgoingMentionUserIds body []
  mentionNames <- fetchMatrixMentionNames driver roomId mentions
  let request = matrixEditMessageRequest eventId body mentions mentionNames False
  response <- sendMatrixTextEdit driver roomId request
  traverse_ (rememberMatrixEvent driver.eventIds) response
  traverse_ (const (rememberStreamTextMessage driver.streamTextMessages eventId request)) response
  pure response

matrixEditMessageRequest :: MatrixEventId -> Text -> [Text] -> Map Text Text -> Bool -> MatrixEditMessageRequest
matrixEditMessageRequest eventId body mentions mentionNames complete =
  MatrixEditMessageRequest
    { body = nonEmptyMatrixBody displayBody
    , formattedBody = formatMatrixMarkdownWithMentionNames mentionNames body
    , mentions = MatrixMentions mentions
    , replacesEventId = eventId
    , streamMetadata = MatrixStreamMetadata complete
    }
  where
    displayBody = matrixMentionDisplayBody mentionNames body

sendMatrixTextEdit :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> MatrixRoomId -> MatrixEditMessageRequest -> Eff es (Either Text SendMessageResponse)
sendMatrixTextEdit driver roomId request =
  eitherCall "edit m.room.message" driver (MatrixEditMessage roomId request)

deleteEvent :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> Text -> MessageId -> Maybe MatrixEventId -> Eff es Bool
deleteEvent driver roomId messageId knownEventId = do
  stored <- IORef.readIORef driver.eventIds
  case knownEventId <|> Map.lookup (messageIdText messageId) stored of
    Nothing ->
      pure False
    Just eventId ->
      isJust <$> maybeCall driver (MatrixRedactEvent roomId eventId)

fetchEvent :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> MatrixRoomId -> MatrixEventId -> Eff es (Maybe Event)
fetchEvent driver roomId eventId =
  maybeCall driver (MatrixFetchEvent roomId eventId)

fetchMember :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> MatrixRoomId -> Text -> Eff es (Maybe MatrixMember)
fetchMember driver roomId userId =
  maybeCall driver (MatrixFetchMember roomId userId)

fetchProfile :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> Text -> Eff es (Maybe MatrixProfile)
fetchProfile driver userId =
  maybeCall driver (MatrixFetchProfile userId)

fetchJoinedMembers :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> MatrixRoomId -> Eff es (Maybe JoinedMembersResponse)
fetchJoinedMembers driver roomId =
  maybeCall driver (MatrixJoinedMembers roomId)

downloadMedia :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> Text -> Eff es (Maybe MatrixDownloadedMedia)
downloadMedia driver mxcRef =
  maybeCall driver (MatrixDownloadMedia mxcRef)

typing :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> MatrixRoomId -> Int -> Eff es ()
typing driver roomId timeoutMs =
  case driver.config.userId of
    Just userId ->
      void $ maybeCall driver (MatrixSetTyping roomId userId timeoutMs)
    Nothing ->
      $(logWarning) [i|Matrix typing notification skipped: bot_id is not configured.|]

rememberMatrixEvent :: Prim :> es => IORef.IORef (Map Text MatrixEventId) -> SendMessageResponse -> Eff es ()
rememberMatrixEvent eventIds response =
  IORef.modifyIORef' eventIds (Map.insert (matrixEventIdText response.eventId) response.eventId)

rememberStreamTextMessage :: Prim :> es => IORef.IORef (Map MatrixEventId MatrixEditMessageRequest) -> MatrixEventId -> MatrixEditMessageRequest -> Eff es ()
rememberStreamTextMessage streamTextMessages eventId request =
  IORef.modifyIORef' streamTextMessages (Map.insert eventId request)

rememberInitialStreamTextMessage :: Prim :> es => IORef.IORef (Map MatrixEventId MatrixEditMessageRequest) -> MatrixEventId -> Text -> [Text] -> Map Text Text -> Bool -> Eff es ()
rememberInitialStreamTextMessage streamTextMessages eventId body mentions mentionNames complete
  | complete =
      pure ()
  | otherwise =
      rememberStreamTextMessage streamTextMessages eventId (matrixEditMessageRequest eventId body mentions mentionNames False)

popStreamTextMessage :: Prim :> es => IORef.IORef (Map MatrixEventId MatrixEditMessageRequest) -> MatrixEventId -> Eff es (Maybe MatrixEditMessageRequest)
popStreamTextMessage streamTextMessages eventId =
  IORef.atomicModifyIORef' streamTextMessages \messages ->
    (Map.delete eventId messages, Map.lookup eventId messages)

rememberMatrixRoomState
  :: Prim :> es
  => IORef.IORef (Set MatrixRoomId)
  -> IORef.IORef (Map MatrixRoomId Int)
  -> SyncResponse
  -> Eff es ()
rememberMatrixRoomState directRoomIdsRef joinedMemberCountsRef response = do
  IORef.modifyIORef' directRoomIdsRef (<> syncDirectRoomIds response)
  for_ (syncJoinedMemberCounts response) \(roomId, count) ->
    rememberJoinedMemberCount directRoomIdsRef joinedMemberCountsRef roomId count

rememberJoinedMemberCount
  :: Prim :> es
  => IORef.IORef (Set MatrixRoomId)
  -> IORef.IORef (Map MatrixRoomId Int)
  -> MatrixRoomId
  -> Int
  -> Eff es ()
rememberJoinedMemberCount directRoomIdsRef joinedMemberCountsRef roomId count = do
  IORef.modifyIORef' joinedMemberCountsRef (Map.insert roomId count)
  IORef.modifyIORef' directRoomIdsRef \directRoomIds ->
    if count == 2
      then Set.insert roomId directRoomIds
      else Set.delete roomId directRoomIds

incomingMessagesProtocol
  :: (HTTP.HTTP :> es, Media.Media :> es, KatipE :> es, IOE :> es, Concurrent :> es, Prim :> es, Storage.Storage :> es)
  => Protocol.MatrixDriver
  -> Stream (Of IncomingMessage) (Eff es) ()
incomingMessagesProtocol driver =
  if matrixAuthConfigured cfg
    then do
      S.lift $ $(logInfo) [i|Matrix sync starting: auth=#{matrixAuthMode cfg}|]
      storedSince <- S.lift loadSyncToken
      case storedSince of
        Just since ->
          syncLoop (Just since)
        Nothing -> do
          S.lift $ $(logInfo) "Matrix sync state is empty; initializing from current homeserver state"
          initializeSyncState
    else S.lift $ $(logInfo) "Matrix driver disabled: no access token, refresh token, or login credentials configured"
  where
    cfg = driver.config

    initializeSyncState = do
      result <- S.lift $ sync driver Nothing `catchSync` \err -> do
        $(logError) [i|Matrix sync initialization failed, retrying: #{show err :: String}|]
        threadDelay matrixRetryDelayMicroseconds
        pure Nothing
      case result of
        Nothing ->
          initializeSyncState
        Just response -> do
          S.lift $ storeSyncToken response.nextBatch
          S.lift $ $(logInfo) "Matrix sync state initialized; skipped initial timeline batch"
          syncLoop (Just response.nextBatch)

    syncLoop since = do
      result <- S.lift $ sync driver since `catchSync` \err -> do
        $(logError) [i|Matrix sync failed, retrying: #{show err :: String}|]
        threadDelay matrixRetryDelayMicroseconds
        pure Nothing
      case result of
        Nothing ->
          syncLoop since
        Just response -> do
          directRoomIds <- S.lift (directRooms driver)
          joinedCounts <- S.lift (joinedMemberCounts driver)
          probedDirectRoomIds <- S.lift (probeDirectRoomIds driver directRoomIds joinedCounts response)
          refreshedDirectRoomIds <- S.lift (directRooms driver)
          let effectiveDirectRoomIds = refreshedDirectRoomIds <> probedDirectRoomIds
              events = syncEvents effectiveDirectRoomIds response
              directCount = Set.size effectiveDirectRoomIds
          S.lift $ $(logDebug) [i|Matrix sync batch: #{length events}; direct_rooms=#{directCount}|]
          for_ events \event ->
            case eventToIncomingMessageWith cfg event of
              Nothing -> do
                let reason = matrixEventIgnoreReason cfg event
                S.lift $ $(logDebug) ("Ignoring Matrix event: " <> reason)
              Just message -> do
                normalized <- S.lift (normalizeMatrixIncomingMessage driver message)
                S.lift $ $(logDebug) ("incoming Matrix message:\n" <> logJsonText normalized)
                S.yield normalized
          S.lift $ storeSyncToken response.nextBatch
          syncLoop (Just response.nextBatch)

syncEvents :: Set MatrixRoomId -> SyncResponse -> [RoomEvent]
syncEvents directRoomIds response =
  [ RoomEvent
      { roomId
      , roomIsDirect = roomId `Set.member` directRoomIds || roomLooksDirect room
      , event
      }
  | (roomIdText, room) <- Map.toList response.rooms.join
  , let roomId = matrixRoomId roomIdText
  , event <- room.timeline.events
  ]

syncDirectRoomIds :: SyncResponse -> Set MatrixRoomId
syncDirectRoomIds =
  Set.fromList . fmap matrixRoomId . (.directRooms) . (.accountData)

syncInvitedRoomIds :: SyncResponse -> [Text]
syncInvitedRoomIds =
  Map.keys . (.invite) . (.rooms)

acceptInvitations
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> SyncResponse
  -> Eff es ()
acceptInvitations driver response =
  for_ (syncInvitedRoomIds response) \roomId -> do
    result <- eitherCall "accept room invitation" driver (MatrixJoinRoom (matrixRoomId roomId))
    case result of
      Right _ -> $(logInfo) [i|Accepted Matrix room invitation: #{roomId}|]
      Left reason -> do
        $(logError) [i|Failed to accept Matrix room invitation #{roomId}: #{reason}|]
        throwIO (MatrixRoomInvitationFailed (matrixRoomId roomId) reason)

syncJoinedMemberCounts :: SyncResponse -> [(MatrixRoomId, Int)]
syncJoinedMemberCounts response =
  [ (roomId, count)
  | (roomIdText, room) <- Map.toList response.rooms.join
  , let roomId = matrixRoomId roomIdText
  , Just count <- [room.summary.joinedMemberCount]
  ]

probeDirectRoomIds
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> Set MatrixRoomId
  -> Map MatrixRoomId Int
  -> SyncResponse
  -> Eff es (Set MatrixRoomId)
probeDirectRoomIds driver knownDirectRoomIds joinedCounts response =
  Set.fromList <$> filterM looksDirect roomIdsToProbe
  where
    roomIdsToProbe =
      [ roomId
      | (roomIdText, room) <- Map.toList response.rooms.join
      , let roomId = matrixRoomId roomIdText
      , not (roomId `Set.member` knownDirectRoomIds)
      , not (roomId `Map.member` joinedCounts)
      , isNothing room.summary.joinedMemberCount
      ]

    looksDirect roomId =
      (== Just 2) <$> joinedMemberCount driver roomId

roomLooksDirect :: JoinedRoom -> Bool
roomLooksDirect room =
  room.summary.joinedMemberCount == Just 2

matrixAuthConfigured :: Config -> Bool
matrixAuthConfigured cfg =
  isJust cfg.loginUser && isJust cfg.loginPassword

matrixAuthMode :: Config -> Text
matrixAuthMode cfg
  | isJust cfg.loginUser && isJust cfg.loginPassword = "login"
  | otherwise = "none"

replyToMatrix
  :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Either Text MessageId)
replyToMatrix driver message body =
  matrixReplyResult <$> replyToMatrixResponses driver True message body

replyToMatrixMessages
  :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> Text
  -> Eff es [Either Text MessageId]
replyToMatrixMessages driver message body =
  map matrixMessageIdResult <$> replyToMatrixResponses driver True message body

streamingReplyToMatrix
  :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Either Text MessageId)
streamingReplyToMatrix driver message body =
  matrixReplyResult <$> replyToMatrixResponses driver False message body

replyToMatrixResponses
  :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> Bool
  -> IncomingMessage
  -> Text
  -> Eff es [Either Text SendMessageResponse]
replyToMatrixResponses driver complete message body =
  case viaNonEmpty head message.chatAliases of
    Just roomId -> do
      let matrixRoom = matrixRoomId roomId
          replyRelation = matrixReplyTo message
          text = Chat.renderReplyBody body
          imageRefs = Chat.replyImageUrls body
      textResponse <- if Text.null (Text.strip text)
        then pure Nothing
        else Just <$> sendMatrixReplyText driver complete matrixRoom replyRelation text
      imageResponses <- traverse (tryMatrixSendImage driver roomId replyRelation) imageRefs
      pure (maybeToList textResponse <> imageResponses)
    _ ->
      pure [Left "Matrix reply requires a Matrix room id."]

sendMatrixReplyText
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> Bool
  -> MatrixRoomId
  -> Maybe MatrixReplyTo
  -> Text
  -> Eff es (Either Text SendMessageResponse)
sendMatrixReplyText driver complete roomId replyRelation text
  | complete =
      sendText driver roomId replyRelation text
  | otherwise =
      sendTextWithMentionsStreamComplete driver roomId replyRelation text [] False

getMessageContentMatrix
  :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> MessageId
  -> Eff es (Maybe ReferencedMessage)
getMessageContentMatrix driver message messageId =
  case viaNonEmpty head message.chatAliases of
    Just roomId -> do
      fetched <- fetchEvent driver (matrixRoomId roomId) (matrixEventId (messageIdText messageId))
      join <$> traverse (normalizeMatrixReferencedEvent driver) fetched
    _ ->
      pure Nothing

getSenderMemberInfoMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> Eff es (Maybe Aeson.Value)
getSenderMemberInfoMatrix driver message =
  case (viaNonEmpty head message.chatAliases, message.senderId) of
    (Just roomId, Just userId) ->
      fmap Aeson.toJSON <$> fetchMember driver (matrixRoomId roomId) userId
    _ ->
      pure Nothing

getMemberInfoMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Maybe Aeson.Value)
getMemberInfoMatrix driver message userId =
  case viaNonEmpty head message.chatAliases of
    Just roomId ->
      fmap Aeson.toJSON <$> fetchMember driver (matrixRoomId roomId) userId
    _ ->
      pure Nothing

getUserAvatarMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> Text
  -> Eff es (Maybe Aeson.Value)
getUserAvatarMatrix driver _ userId =
  fmap matrixProfileAvatarValue <$> fetchProfile driver userId

listGroupMembersMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> Eff es (Maybe Aeson.Value)
listGroupMembersMatrix driver message =
  case viaNonEmpty head message.chatAliases of
    Just roomId ->
      fmap matrixJoinedMembersValue <$> fetchJoinedMembers driver (matrixRoomId roomId)
    _ ->
      pure Nothing

mentionUserMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> Text
  -> Text
  -> Eff es (Either Text MessageId)
mentionUserMatrix driver message userId body =
  case viaNonEmpty head message.chatAliases of
    Just roomId -> do
      let replyRelation = matrixReplyTo message
          text = matrixMentionText userId (Chat.renderReplyBody body)
      response <- sendTextWithMentions driver (matrixRoomId roomId) replyRelation text [userId]
      pure (matrixMessageIdResult response)
    _ ->
      pure (Left "Matrix mention reply requires a Matrix room id.")

matrixMentionText :: Text -> Text -> Text
matrixMentionText userId body =
  let text = Text.strip body
  in if userId `Text.isInfixOf` text
    then text
    else Text.unwords [userId, text]

matrixReferencedMessage :: Event -> Maybe ReferencedMessage
matrixReferencedMessage =
  matrixReferencedMessageFromEvent . applyLatestMatrixReplacement

matrixReferencedMessageFromEvent :: Event -> Maybe ReferencedMessage
matrixReferencedMessageFromEvent event = do
  guard (event.type_ `elem` ["m.room.message", "m.sticker"])
  let body = fromMaybe "" (matrixEventText event)
      imageUrls = matrixEventImageUrls event.raw
      files = map fst (matrixEventFileMediaRefs event.raw)
  guard (not (Text.null body) || not (null imageUrls) || not (null files))
  pure ReferencedMessage
    { messageId = matrixEventMessageId <$> event.eventId
    , senderDisplayName = Just event.sender
    , senderIdentifier = Just event.sender
    , senderIsBot = False
    , text = body
    , imageUrls
    , files
    }

applyLatestMatrixReplacement :: Event -> Event
applyLatestMatrixReplacement event =
  fromMaybe event do
    replacementContent <- Aeson.parseMaybe parseLatestReplacementContent event.raw
    content <- Aeson.parseMaybe Aeson.parseJSON replacementContent
    raw <- case event.raw of
      Aeson.Object fields ->
        Just (Aeson.Object (AesonKeyMap.insert "content" replacementContent fields))
      _ ->
        Nothing
    pure event{content, raw}
  where
    parseLatestReplacementContent =
      Aeson.withObject "Matrix event" \eventObject -> do
        unsigned <- eventObject Aeson..: "unsigned"
        relations <- Aeson.withObject "Matrix unsigned" (Aeson..: "m.relations") unsigned
        replacement <- Aeson.withObject "Matrix relations" (Aeson..: "m.replace") relations
        replacementEventContent <- Aeson.withObject "Matrix replacement" (Aeson..: "content") replacement
        Aeson.withObject "Matrix replacement content" (Aeson..: "m.new_content") replacementEventContent

normalizeMatrixReferencedEvent
  :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> Event
  -> Eff es (Maybe ReferencedMessage)
normalizeMatrixReferencedEvent driver originalEvent =
  traverse withNormalizedImages (matrixReferencedMessageFromEvent event)
  where
    event = applyLatestMatrixReplacement originalEvent

    withNormalizedImages message = do
      imageUrls <- case matrixEventImageMediaRefs event.raw of
        [] ->
          normalizeMatrixMediaRefs driver message.imageUrls
        mediaRefs ->
          normalizeMatrixMediaRefsWithMetadata driver mediaRefs
      files <- normalizeMatrixFiles driver (matrixEventFileMediaRefs event.raw)
      pure ReferencedMessage
        { messageId = message.messageId
        , senderDisplayName = message.senderDisplayName
        , senderIdentifier = message.senderIdentifier
        , senderIsBot = Just event.sender == driver.config.userId
        , text = message.text
        , imageUrls
        , files
        }

normalizeMatrixIncomingMessage :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> IncomingMessage -> Eff es IncomingMessage
normalizeMatrixIncomingMessage driver message = do
  imageUrls <-
    case matrixEventImageMediaRefs message.raw of
      [] ->
        normalizeMatrixMediaRefs driver message.imageUrls
      mediaRefs ->
        normalizeMatrixMediaRefsWithMetadata driver mediaRefs
  files <- normalizeMatrixFiles driver (matrixEventFileMediaRefs message.raw)
  (senderDisplayName, senderGlobalDisplayName) <- matrixSenderNames driver message
  roomDisplayName <- cachedMatrixRoomDisplayName driver message
  let chatDisplayName = roomDisplayName <|> if message.kind == ChatPrivate then senderDisplayName else Nothing
  pure (message :: IncomingMessage){imageUrls, files, chatDisplayName, senderDisplayName, senderGlobalDisplayName}

cachedMatrixRoomDisplayName
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> Eff es (Maybe Text)
cachedMatrixRoomDisplayName driver message = case listToMaybe message.chatAliases of
  Nothing -> pure Nothing
  Just roomIdText -> do
    let roomId = matrixRoomId roomIdText
    cached <- Map.lookup roomId <$> IORef.readIORef driver.roomDisplayNames
    case cached of
      Just displayName -> pure (Just displayName)
      Nothing -> do
        displayName <- either (const Nothing) (Just . (.roomName)) <$> eitherCall "room name" driver (MatrixFetchRoomName roomId)
        for_ displayName \resolved ->
          IORef.modifyIORef' driver.roomDisplayNames (Map.insert roomId resolved)
        pure displayName

matrixSenderNames
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> Eff es (Maybe Text, Maybe Text)
matrixSenderNames driver message = case (message.senderId, listToMaybe message.chatAliases) of
  (Just senderId, Just roomIdText) -> do
    let roomId = matrixRoomId roomIdText
    contextual <- cachedMemberDisplayName driver roomId senderId
    global <- cachedProfileDisplayName driver senderId
    pure (contextual <|> global, global)
  _ -> pure (Nothing, Nothing)

cachedMemberDisplayName
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> MatrixRoomId
  -> Text
  -> Eff es (Maybe Text)
cachedMemberDisplayName driver roomId senderId = do
  cached <- Map.lookup (roomId, senderId) <$> IORef.readIORef driver.memberDisplayNames
  case cached of
    Just displayName -> pure displayName
    Nothing -> do
      displayName <- either (const Nothing) (.memberDisplayName) <$> eitherCall "room member identity" driver (MatrixFetchMember roomId senderId)
      IORef.modifyIORef' driver.memberDisplayNames (Map.insert (roomId, senderId) displayName)
      pure displayName

cachedProfileDisplayName
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> Text
  -> Eff es (Maybe Text)
cachedProfileDisplayName driver senderId = do
  cached <- Map.lookup senderId <$> IORef.readIORef driver.profileDisplayNames
  case cached of
    Just displayName -> pure displayName
    Nothing -> do
      displayName <- either (const Nothing) (.profileDisplayName) <$> eitherCall "profile identity" driver (MatrixFetchProfile senderId)
      IORef.modifyIORef' driver.profileDisplayNames (Map.insert senderId displayName)
      pure displayName

normalizeMatrixMediaRefs :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> [Text] -> Eff es [Text]
normalizeMatrixMediaRefs driver =
  traverse (normalizeMatrixMediaRef driver Nothing)

normalizeMatrixMediaRefsWithMetadata :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> [MatrixMediaRef] -> Eff es [Text]
normalizeMatrixMediaRefsWithMetadata driver =
  traverse \mediaRef ->
    normalizeMatrixMediaRefWithMetadata driver mediaRef

normalizeMatrixMediaRef :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> Maybe Text -> Text -> Eff es Text
normalizeMatrixMediaRef driver preferredMime ref
  = normalizeMatrixMediaRefWithMetadata driver MatrixMediaRef
      { matrixMediaRefUrl = ref
      , matrixMediaRefMimeType = preferredMime
      , matrixMediaRefEncrypted = Nothing
      }

normalizeMatrixMediaRefWithMetadata :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> MatrixMediaRef -> Eff es Text
normalizeMatrixMediaRefWithMetadata driver mediaRef
  | "mxc://" `Text.isPrefixOf` Text.strip ref = do
      let mxcRef = Text.strip ref
      Media.mediaRefForSource mxcRef >>= \case
        Just cachedMediaRef ->
          pure cachedMediaRef
        Nothing ->
          cacheMatrixMediaRef driver mediaRef mxcRef ref
  | otherwise =
      pure ref
  where
    ref = mediaRef.matrixMediaRefUrl

cacheMatrixMediaRef :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> MatrixMediaRef -> Text -> Text -> Eff es Text
cacheMatrixMediaRef driver mediaRef mxcRef fallbackRef = do
  cached <- cacheMatrixMediaRefObject driver mediaRef mxcRef `catchSync` \err -> do
    $(logInfo) [i|Matrix media normalization skipped for #{mxcRef}: #{displayException err}|]
    pure Nothing
  case cached of
    Nothing ->
      pure fallbackRef
    Just cachedMediaRef ->
      pure cachedMediaRef

cacheMatrixMediaRefObject :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> MatrixMediaRef -> Text -> Eff es (Maybe Text)
cacheMatrixMediaRefObject driver mediaRef mxcRef =
  fetchMatrixMediaObject driver mediaRef mxcRef >>= \case
    Nothing ->
      pure Nothing
    Just mediaObject ->
      Media.storeMediaObjectFromSource mxcRef mediaObject

fetchMatrixMediaObject :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es) => Protocol.MatrixDriver -> MatrixMediaRef -> Text -> Eff es (Maybe Media.MediaObject)
fetchMatrixMediaObject driver mediaRef mxcRef = do
  downloadMedia driver mxcRef >>= traverse \media ->
    case mediaRef.matrixMediaRefEncrypted of
      Nothing ->
        pure (matrixMediaObject mediaRef.matrixMediaRefMimeType media)
      Just encrypted ->
        decryptMatrixMediaObject mediaRef.matrixMediaRefMimeType encrypted media

matrixMediaObject :: Maybe Text -> MatrixDownloadedMedia -> Media.MediaObject
matrixMediaObject preferredMime media =
  Media.MediaObject
    { bytes = media.downloadedBytes
    , mimeType = fromMaybe media.downloadedMimeType preferredMime
    , sourceName = media.downloadedName
    }

decryptMatrixMediaObject :: IOE :> es => Maybe Text -> MatrixEncryptedFile -> MatrixDownloadedMedia -> Eff es Media.MediaObject
decryptMatrixMediaObject preferredMime encrypted media = do
  plan <- either (throwIO . MatrixDecryptionPlanFailed) pure (matrixDecryptionPlan encrypted)
  pure Media.MediaObject
    { bytes = decryptMatrixEncryptedByteStream plan media.downloadedBytes
    , mimeType = fromMaybe media.downloadedMimeType preferredMime
    , sourceName = media.downloadedName
    }

matrixDecryptionPlan :: MatrixEncryptedFile -> Either Text MatrixDecryptionPlan
matrixDecryptionPlan encrypted = do
  key <- first ("invalid Matrix encrypted file key: " <>) (decodeBase64UrlText encrypted.encryptedFileKey)
  ivBytes <- first ("invalid Matrix encrypted file IV: " <>) (decodeBase64TextUnpadded encrypted.encryptedFileIv)
  expectedHash <- first ("invalid Matrix encrypted file sha256: " <>) (decodeBase64TextUnpadded encrypted.encryptedFileSha256)
  cipher <- case CryptoError.eitherCryptoError (CryptoCipher.cipherInit key :: CryptoError.CryptoFailable CryptoAES.AES256) of
    Left err ->
      Left [i|invalid Matrix encrypted file cipher key: #{show err :: String}|]
    Right value ->
      Right value
  iv <- maybe (Left "invalid Matrix encrypted file AES-CTR IV.") Right (CryptoCipher.makeIV ivBytes)
  expectedDigest <- case CryptoHash.digestFromByteString expectedHash :: Maybe (CryptoHash.Digest CryptoHash.SHA256) of
    Nothing ->
      Left "invalid Matrix encrypted file sha256 digest length."
    Just expected ->
      Right expected
  pure MatrixDecryptionPlan
    { decryptionCipher = cipher
    , decryptionIv = iv
    , decryptionExpectedHash = expectedDigest
    }

decryptMatrixEncryptedByteStream :: MatrixDecryptionPlan -> Q.ByteStream (ResourceT IO) () -> Q.ByteStream (ResourceT IO) ()
decryptMatrixEncryptedByteStream plan encryptedBytes =
  Q.fromChunks (go CryptoHash.hashInit 0 StrictByteString.empty (Q.toChunks encryptedBytes))
  where
    go
      :: CryptoHash.Context CryptoHash.SHA256
      -> Int
      -> StrictByteString.ByteString
      -> S.Stream (S.Of StrictByteString.ByteString) (ResourceT IO) ()
      -> S.Stream (S.Of StrictByteString.ByteString) (ResourceT IO) ()
    go context blockIndex pending chunks =
      lift (SP.next chunks) >>= \case
        Left () -> do
          let plainText = decryptMatrixChunkAt plan blockIndex pending
              finalContext = CryptoHash.hashUpdate context pending
              digest = CryptoHash.hashFinalize finalContext
          unless (StrictByteString.null plainText) do
            SP.yield plainText
          unless (digest == plan.decryptionExpectedHash) do
            liftIO (Exception.throwIO MatrixEncryptedFileHashMismatch)
        Right (chunk, rest) -> do
          let bytes = pending <> chunk
              readyLength = (StrictByteString.length bytes `div` matrixAesBlockSize) * matrixAesBlockSize
              (ready, nextPending) = StrictByteString.splitAt readyLength bytes
              plainText = decryptMatrixChunkAt plan blockIndex ready
              nextContext = CryptoHash.hashUpdate context ready
              nextBlockIndex = blockIndex + readyLength `div` matrixAesBlockSize
          unless (StrictByteString.null plainText) do
            SP.yield plainText
          go nextContext nextBlockIndex nextPending rest

decryptMatrixChunkAt :: MatrixDecryptionPlan -> Int -> StrictByteString.ByteString -> StrictByteString.ByteString
decryptMatrixChunkAt plan blockIndex bytes =
  CryptoCipher.ctrCombine plan.decryptionCipher (CryptoCipher.ivAdd plan.decryptionIv blockIndex) bytes

decryptMatrixEncryptedBytesForTest :: Text -> Text -> Text -> [StrictByteString.ByteString] -> IO [StrictByteString.ByteString]
decryptMatrixEncryptedBytesForTest key iv sha256 chunks =
  case matrixDecryptionPlan encrypted of
    Left err ->
      Exception.throwIO (MatrixDecryptionPlanFailed err)
    Right plan ->
      runResourceT (SP.toList_ (Q.toChunks (decryptMatrixEncryptedByteStream plan (Q.fromChunks (SP.each chunks)))))
  where
    encrypted = MatrixEncryptedFile
      { encryptedFileUrl = "mxc://example.invalid/test"
      , encryptedFileKey = key
      , encryptedFileIv = iv
      , encryptedFileSha256 = sha256
      }

matrixAesBlockSize :: Int
matrixAesBlockSize =
  16

decodeBase64UrlText :: Text -> Either Text StrictByteString.ByteString
decodeBase64UrlText =
  first Text.pack . Base64URL.decode . TextEncoding.encodeUtf8 . Text.strip

decodeBase64TextUnpadded :: Text -> Either Text StrictByteString.ByteString
decodeBase64TextUnpadded =
  first Text.pack . Base64.decode . padBase64 . TextEncoding.encodeUtf8 . Text.strip

padBase64 :: StrictByteString.ByteString -> StrictByteString.ByteString
padBase64 bytes =
  case StrictByteString.length bytes `mod` 4 of
    0 -> bytes
    2 -> bytes <> "=="
    3 -> bytes <> "="
    _ -> bytes

matrixEventImageUrls :: Aeson.Value -> [Text]
matrixEventImageUrls =
  map (.matrixMediaRefUrl) . matrixEventImageMediaRefs

matrixEventImageMediaRefs :: Aeson.Value -> [MatrixMediaRef]
matrixEventImageMediaRefs =
  fromMaybe [] . Aeson.parseMaybe parse
  where
    parse =
      Aeson.withObject "Matrix event" \eventObject -> do
        content <- eventObject Aeson..:? "content" Aeson..!= Aeson.Object mempty
        Aeson.withObject "Matrix event content" parseContent content

    parseContent contentObject = do
      msgtype <- contentObject Aeson..:? "msgtype" Aeson..!= ("" :: Text)
      url <- contentObject Aeson..:? "url"
      encrypted <- contentObject Aeson..:? "file" >>= traverse parseEncryptedFile
      mimeType <- contentObject Aeson..:? "info" Aeson..!= Aeson.Object mempty >>=
        Aeson.withObject "Matrix image info" (Aeson..:? "mimetype")
      pure
        [ MatrixMediaRef
            { matrixMediaRefUrl = imageUrl
            , matrixMediaRefMimeType = nonEmptyText =<< mimeType
            , matrixMediaRefEncrypted = encryptedFile
            }
        | msgtype == "m.image"
        , (imageUrl, encryptedFile) <- maybeToList (matrixImageContentRef url encrypted)
        ]

    matrixImageContentRef url encrypted =
      case url of
        Just imageUrl ->
          Just (imageUrl, Nothing)
        Nothing -> do
          encryptedFile <- encrypted
          Just (encryptedFile.encryptedFileUrl, Just encryptedFile)

matrixEventFileMediaRefs :: Aeson.Value -> [(MessageFile, MatrixMediaRef)]
matrixEventFileMediaRefs =
  fromMaybe [] . Aeson.parseMaybe (Aeson.withObject "Matrix event" \eventObject -> do
    content <- eventObject Aeson..:? "content" Aeson..!= Aeson.Object mempty
    Aeson.withObject "Matrix event content" parseContent content)
  where
    parseContent content = do
      msgtype <- content Aeson..:? "msgtype" Aeson..!= ("" :: Text)
      explicitName <- content Aeson..:? "filename"
      body <- content Aeson..:? "body"
      let name = explicitName <|> body
      url <- content Aeson..:? "url"
      encrypted <- content Aeson..:? "file" >>= traverse parseEncryptedFile
      mimeType <- content Aeson..:? "info" Aeson..!= Aeson.Object mempty >>=
        Aeson.withObject "Matrix file info" (Aeson..:? "mimetype")
      pure do
        guard (msgtype `elem` ["m.file", "m.audio"])
        fileName <- maybeToList (name >>= nonEmptyText)
        (mediaUrl, encryptedFile) <- maybeToList (contentRef url encrypted)
        let mediaRef = MatrixMediaRef mediaUrl (nonEmptyText =<< mimeType) encryptedFile
        pure (MessageFile{name = fileName, ref = mediaUrl}, mediaRef)

    contentRef (Just url) _ = Just (url, Nothing)
    contentRef Nothing encrypted = do
      file <- encrypted
      pure (file.encryptedFileUrl, Just file)

normalizeMatrixFiles
  :: (HTTP.HTTP :> es, Media.Media :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> [(MessageFile, MatrixMediaRef)]
  -> Eff es [MessageFile]
normalizeMatrixFiles driver =
  traverse \(file, mediaRef) -> do
    ref <- normalizeMatrixMediaRefWithMetadata driver mediaRef
    pure MessageFile{name = file.name, ref}

parseEncryptedFile :: Aeson.Value -> Aeson.Parser MatrixEncryptedFile
parseEncryptedFile =
  Aeson.withObject "Matrix encrypted file" \fileObject -> do
    url <- fileObject Aeson..: "url"
    iv <- fileObject Aeson..: "iv"
    sha256 <- fileObject Aeson..: "hashes" >>=
      Aeson.withObject "Matrix encrypted file hashes" (Aeson..: "sha256")
    key <- fileObject Aeson..: "key" >>=
      Aeson.withObject "Matrix encrypted file key" \keyObject -> do
        alg <- keyObject Aeson..:? "alg" Aeson..!= ("" :: Text)
        unless (alg == "A256CTR") do
          fail [i|unsupported Matrix encrypted file algorithm: #{alg}|]
        keyObject Aeson..: "k"
    pure MatrixEncryptedFile
      { encryptedFileUrl = url
      , encryptedFileKey = key
      , encryptedFileIv = iv
      , encryptedFileSha256 = sha256
      }

matrixProfileAvatarValue :: MatrixProfile -> Aeson.Value
matrixProfileAvatarValue profile =
  Aeson.object
    [ "platform" Aeson..= ("matrix" :: Text)
    , "user_id" Aeson..= profile.profileUserId
    , "displayname" Aeson..= profile.profileDisplayName
    , "avatar_url" Aeson..= profile.profileAvatarUrl
    ]

matrixJoinedMembersValue :: JoinedMembersResponse -> Aeson.Value
matrixJoinedMembersValue response =
  Aeson.object
    [ "joined" Aeson..= response.joinedMembers
    ]

matrixReplyTo :: IncomingMessage -> Maybe MatrixReplyTo
matrixReplyTo message = do
  messageId <- message.messageId
  pure (MatrixReplyTo (fromMaybe (matrixEventId (messageIdText messageId)) (matrixRawEventId message.raw)))

uploadFileMatrix
  :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> FilePath
  -> Maybe Text
  -> Eff es (Either Text MessageId)
uploadFileMatrix driver message path requestedFileName =
  case viaNonEmpty head message.chatAliases of
    Just roomId ->
      matrixUserFacingEither "file upload" do
        let fileName = Driver.uploadFileName path requestedFileName
            mime = Mime.mimeFromName (Text.pack path)
        size <- FileSystem.getFileSize path
        uploaded <- uploadMedia driver path fileName mime
        response <- sendFileMessage driver roomId (matrixReplyTo message) MatrixFileMessage
          { msgtype = matrixFileMsgtype mime
          , body = fileName
          , filename = fileName
          , url = uploaded.contentUri
          , info = MatrixFileInfo
              { mimetype = mime
              , size = size
              }
          }
        pure (matrixMessageIdResult response)
    _ ->
      pure (Left "Matrix file upload requires a Matrix room id.")

sendMatrixImage
  :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> Text
  -> Maybe MatrixReplyTo
  -> Text
  -> Eff es (Either Text SendMessageResponse)
sendMatrixImage driver roomId replyRelation imageRef =
  case matrixMxcRef imageRef of
    Just contentUri ->
      sendMatrixImageMessage driver roomId replyRelation "image" contentUri "application/octet-stream" 0
    Nothing -> do
      let scope = matrixMediaScope driver
      Media.platformMediaRef "matrix" scope imageRef >>= \case
        Just contentUri ->
          sendMatrixImageMessage driver roomId replyRelation "image" contentUri "application/octet-stream" 0
        Nothing ->
          withMatrixImageFile imageRef \path fileName mime -> do
            size <- FileSystem.getFileSize path
            uploaded <- uploadMedia driver path fileName mime
            Media.storePlatformMediaRef "matrix" scope imageRef uploaded.contentUri
            sendMatrixImageMessage driver roomId replyRelation fileName uploaded.contentUri mime size

tryMatrixSendImage
  :: (HTTP.HTTP :> es, Media.Media :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> Text
  -> Maybe MatrixReplyTo
  -> Text
  -> Eff es (Either Text SendMessageResponse)
tryMatrixSendImage driver roomId replyRelation imageRef = do
  result <- trySync (sendMatrixImage driver roomId replyRelation imageRef)
  pure case result of
    Left err ->
      Left (matrixUserFacingExceptionText [i|image reply for #{imageRef}|] err)
    Right response ->
      response

matrixMediaScope :: Protocol.MatrixDriver -> Text
matrixMediaScope driver =
  fromMaybe driver.config.homeserver driver.config.userId

sendMatrixImageMessage
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> Text
  -> Maybe MatrixReplyTo
  -> Text
  -> Text
  -> Text
  -> Integer
  -> Eff es (Either Text SendMessageResponse)
sendMatrixImageMessage driver roomId replyRelation fileName contentUri mime size =
  sendFileMessage driver roomId replyRelation MatrixFileMessage
    { msgtype = "m.image"
    , body = fileName
    , filename = fileName
    , url = contentUri
    , info = MatrixFileInfo
        { mimetype = mime
        , size = size
        }
    }

replyAudioMatrix
  :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> Text
  -> Maybe Text
  -> Eff es (Either Text MessageId)
replyAudioMatrix driver message audioRef caption =
  case viaNonEmpty head message.chatAliases of
    Just roomId ->
      sendMatrixAudio driver roomId audioRef caption
    _ ->
      pure (Left "Matrix audio reply requires a Matrix room id.")

sendMatrixAudio
  :: (HTTP.HTTP :> es, FileSystem :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> Text
  -> Text
  -> Maybe Text
  -> Eff es (Either Text MessageId)
sendMatrixAudio driver roomId audioRef caption =
  matrixUserFacingEither "audio reply" do
    case matrixMxcRef audioRef of
      Just contentUri -> do
        let fileName = "audio"
        response <- sendFileMessage driver roomId Nothing (matrixAudioMessage caption fileName contentUri "application/octet-stream" 0)
        pure (matrixMessageIdResult response)
      Nothing ->
        withMatrixAudioFile audioRef \path fileName mime -> do
          size <- FileSystem.getFileSize path
          uploaded <- uploadMedia driver path fileName mime
          response <- sendFileMessage driver roomId Nothing (matrixAudioMessage caption fileName uploaded.contentUri mime size)
          pure (matrixMessageIdResult response)

matrixUserFacingEither
  :: KatipE :> es
  => Text
  -> Eff es (Either Text a)
  -> Eff es (Either Text a)
matrixUserFacingEither label action =
  action `catch` \(err :: MatrixApiException) -> do
    $(logWarning) [i|#{matrixApiExceptionMessage err}|]
    pure (Left (matrixUserFacingApiError label err))

matrixMessageIdResult :: Either Text SendMessageResponse -> Either Text MessageId
matrixMessageIdResult =
  fmap (matrixEventMessageId . (.eventId))

matrixReplyResult :: [Either Text SendMessageResponse] -> Either Text MessageId
matrixReplyResult responses =
  case errors of
    err : _ ->
      Left (matrixReplyFailureText sentIds err)
    [] ->
      case sentIds of
        sent : _ ->
          Right sent
        [] ->
          Left "Matrix reply did not send any message."
  where
    sentIds = [matrixEventMessageId response.eventId | Right response <- responses]
    errors = [err | Left err <- responses]

matrixReplyFailureText :: [MessageId] -> Text -> Text
matrixReplyFailureText sentIds err =
  case sentIds of
    [] ->
      err
    sent : _ ->
      [i|#{err} Text message was sent as #{messageIdText sent}, but one or more image messages failed.|]

logMatrixSendErrors :: KatipE :> es => [Either Text SendMessageResponse] -> Eff es ()
logMatrixSendErrors responses =
  traverse_ $(logWarning) [err | Left err <- responses]

matrixAudioMessage :: Maybe Text -> Text -> Text -> Text -> Integer -> MatrixFileMessage
matrixAudioMessage caption fileName contentUri mime size =
  MatrixFileMessage
    { msgtype = "m.audio"
    , body = fromMaybe fileName (caption >>= nonEmptyText)
    , filename = fileName
    , url = contentUri
    , info = MatrixFileInfo
        { mimetype = mime
        , size = size
        }
    }

matrixUploadFileName :: FilePath -> Text
matrixUploadFileName path =
  let name = Text.pack (takeFileName path)
  in if Text.null name then "file" else name

matrixFileMsgtype :: Text -> Text
matrixFileMsgtype mime
  | "image/" `Text.isPrefixOf` clean = "m.image"
  | "audio/" `Text.isPrefixOf` clean = "m.audio"
  | "video/" `Text.isPrefixOf` clean = "m.video"
  | otherwise = "m.file"
  where
    clean = Text.toLower (Text.takeWhile (/= ';') mime)

matrixMxcRef :: Text -> Maybe Text
matrixMxcRef ref =
  let stripped = Text.strip ref
  in stripped <$ guard ("mxc://" `Text.isPrefixOf` stripped)

withMatrixImageFile
  :: (Media.Media :> es, FileSystem :> es, IOE :> es)
  => Text
  -> (FilePath -> Text -> Text -> Eff es a)
  -> Eff es a
withMatrixImageFile imageRef action =
  Media.localMediaPath imageRef >>= \case
    Just path -> do
      mediaInfo <- Media.mediaFileInfoByRef imageRef
      let fileName = fromMaybe (matrixUploadFileName path) (mediaInfo >>= (.sourceName))
          mime = maybe (matrixImageMimeType path) (.mimeType) mediaInfo
      action path fileName mime
    Nothing ->
      case matrixLocalPath imageRef of
        Just path ->
          action path (matrixUploadFileName path) (matrixImageMimeType path)
        Nothing ->
          case matrixDataImage imageRef of
            Just (mime, bytes) ->
              withTemporaryMatrixImage mime bytes \path ->
                action path (matrixUploadFileName path) mime
            Nothing ->
              throwIO (InvalidMatrixImageReference imageRef)

withMatrixAudioFile
  :: (FileSystem :> es, IOE :> es)
  => Text
  -> (FilePath -> Text -> Text -> Eff es (Either Text MessageId))
  -> Eff es (Either Text MessageId)
withMatrixAudioFile audioRef action =
  case matrixLocalPath audioRef of
    Just path ->
      action path (matrixUploadFileName path) (matrixAudioMimeType path)
    Nothing ->
      case matrixDataAudio audioRef of
        Just (mime, bytes) ->
          withTemporaryMatrixAudio mime bytes \path ->
            action path (matrixUploadFileName path) mime
        Nothing ->
          pure (Left "Matrix audio reply requires a file://, data:audio/*, or mxc:// audio reference.")

matrixLocalPath :: Text -> Maybe FilePath
matrixLocalPath ref =
  let stripped = Text.strip ref
  in case Text.stripPrefix "file://" stripped of
    Just path ->
      Just (Text.unpack path)
    Nothing
      | isLocalPathRef stripped ->
          Just (Text.unpack stripped)
      | otherwise ->
          Nothing

isLocalPathRef :: Text -> Bool
isLocalPathRef ref =
  "/" `Text.isPrefixOf` ref || "./" `Text.isPrefixOf` ref || "../" `Text.isPrefixOf` ref

matrixDataAudio :: Text -> Maybe (Text, Q.ByteStream (ResourceT IO) ())
matrixDataAudio ref = do
  rest <- Text.stripPrefix "data:audio/" (Text.strip ref)
  let (subtype, encodedWithMarker) = Text.breakOn ";base64," rest
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  bytes <- either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encoded))
  pure ("audio/" <> subtype, Q.fromStrict bytes)

matrixDataImage :: Text -> Maybe (Text, Q.ByteStream (ResourceT IO) ())
matrixDataImage ref = do
  rest <- Text.stripPrefix "data:image/" (Text.strip ref)
  let (subtype, encodedWithMarker) = Text.breakOn ";base64," rest
  encoded <- Text.stripPrefix ";base64," encodedWithMarker
  bytes <- either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encoded))
  pure ("image/" <> subtype, Q.fromStrict bytes)

withTemporaryMatrixImage
  :: (FileSystem :> es, IOE :> es)
  => Text
  -> Q.ByteStream (ResourceT IO) ()
  -> (FilePath -> Eff es a)
  -> Eff es a
withTemporaryMatrixImage mime bytes action = do
  Temporary.runTemporary $
    Temporary.withSystemTempDirectory "cosmobot-matrix-" \dir -> do
      let path = dir </> ("matrix-image" <.> matrixImageExtension mime)
      liftIO (runResourceT (Q.writeFile path bytes))
      raise (action path)

withTemporaryMatrixAudio
  :: (FileSystem :> es, IOE :> es)
  => Text
  -> Q.ByteStream (ResourceT IO) ()
  -> (FilePath -> Eff es a)
  -> Eff es a
withTemporaryMatrixAudio mime bytes action = do
  Temporary.runTemporary $
    Temporary.withSystemTempDirectory "cosmobot-matrix-" \dir -> do
      let path = dir </> ("matrix-audio" <.> matrixAudioExtension mime)
      liftIO (runResourceT (Q.writeFile path bytes))
      raise (action path)

matrixAudioMimeType :: FilePath -> Text
matrixAudioMimeType =
  Mime.mimeFromName . Text.pack

matrixImageMimeType :: FilePath -> Text
matrixImageMimeType =
  Mime.mimeFromName . Text.pack

matrixImageExtension :: Text -> String
matrixImageExtension mime =
  Text.unpack (Text.dropWhile (== '.') (Mime.extensionFromMime mime))

matrixAudioExtension :: Text -> String
matrixAudioExtension mime =
  Text.unpack (Text.dropWhile (== '.') (Mime.extensionFromMime mime))

nonEmptyText :: Text -> Maybe Text
nonEmptyText text =
  let stripped = Text.strip text
  in if Text.null stripped then Nothing else Just stripped

editMessageMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> MessageId
  -> Text
  -> Eff es Bool
editMessageMatrix driver message messageId body =
  case viaNonEmpty head message.chatAliases of
    Just roomId -> do
      case matrixEditableEditText body of
        Nothing ->
          pure True
        Just text -> do
          response <- editText driver (matrixRoomId roomId) (matrixEventId (messageIdText messageId)) text
          logMatrixSendErrors [response]
          pure (either (const False) (const True) response)
    _ ->
      pure False

completeMessageEditMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> MessageId
  -> Eff es Bool
completeMessageEditMatrix driver message messageId =
  case viaNonEmpty head message.chatAliases of
    Just roomId -> do
      let eventId = matrixEventId (messageIdText messageId)
      popStreamTextMessage driver.streamTextMessages eventId >>= \case
        Nothing ->
          pure True
        Just request -> do
          let completeRequest = completeMatrixEditMessageRequest request
          response <- eitherCall "complete stream m.room.message" driver (MatrixCompleteStreamMessage (matrixRoomId roomId) completeRequest)
          logMatrixSendErrors [response]
          pure (either (const False) (const True) response)
    _ ->
      pure False

matrixEditableEditText :: Text -> Maybe Text
matrixEditableEditText body
  | Text.null (Text.strip text) && not (null imageRefs) =
      Nothing
  | otherwise =
      Just text
  where
    text = Chat.renderReplyBody body
    imageRefs = Chat.replyImageUrls body

deleteMessageMatrix
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> IncomingMessage
  -> MessageId
  -> Eff es Bool
deleteMessageMatrix driver message messageId =
  case viaNonEmpty head message.chatAliases of
    Just roomId ->
      deleteEvent driver roomId messageId (currentRawEventId message messageId)
    _ ->
      pure False

currentRawEventId :: IncomingMessage -> MessageId -> Maybe MatrixEventId
currentRawEventId message messageId = do
  guard (message.messageId == Just messageId)
  matrixRawEventId message.raw

matrixRawEventId :: Aeson.Value -> Maybe MatrixEventId
matrixRawEventId =
  Aeson.parseMaybe (Aeson.withObject "Matrix event" \o -> matrixEventId <$> o Aeson..: "event_id")

eventToIncomingMessage :: RoomEvent -> Maybe IncomingMessage
eventToIncomingMessage =
  eventToIncomingMessageWith defaultConfig

eventToIncomingMessageWith :: Config -> RoomEvent -> Maybe IncomingMessage
eventToIncomingMessageWith cfg RoomEvent{roomId, roomIsDirect, event}
  | event.type_ == "m.room.redaction" = do
      redactedEventId <- matrixRedactedEventId event.raw
      pure IncomingMessage
        { eventKind = IncomingMessageDeleted
        , platform = PlatformMatrix
        , kind = if roomIsDirect then ChatPrivate else ChatGroup
        , chatId = Just (textChatId (matrixRoomIdText roomId))
        , chatAliases = [matrixRoomIdText roomId]
        , chatDisplayName = Nothing
        , digest = matrixMessageDigest cfg roomId event ""
        , senderId = Just event.sender
        , senderUsername = Just event.sender
        , senderDisplayName = Nothing
        , senderGlobalDisplayName = Nothing
        , messageId = Just (matrixEventMessageId redactedEventId)
        , replyToMessageId = Nothing
        , mentions = []
        , mentionUsernames = []
        , imageUrls = []
        , files = []
        , text = ""
        , raw = event.raw
        }
  | otherwise = do
      guard (not (isOwnEvent cfg event))
      guard (not (matrixStreamIncomplete event.raw))
      body <- matrixEventText event
      pure IncomingMessage
        { eventKind = IncomingMessageCreated
        , platform = PlatformMatrix
        , kind = if roomIsDirect then ChatPrivate else ChatGroup
        , chatId = Just (textChatId (matrixRoomIdText roomId))
        , chatAliases = [matrixRoomIdText roomId]
        , chatDisplayName = Nothing
        , digest = matrixMessageDigest cfg roomId event body
        , senderId = Just event.sender
        , senderUsername = Just event.sender
        , senderDisplayName = Nothing
        , senderGlobalDisplayName = Nothing
        , messageId = matrixEventMessageId <$> event.eventId
        , replyToMessageId = matrixEventMessageId <$> event.content.replyToEventId
        , mentions = []
        , mentionUsernames = matrixMentions cfg event.content body
        , imageUrls = matrixEventImageUrls event.raw
        , files = map fst (matrixEventFileMediaRefs event.raw)
        , text = if event.type_ == "m.sticker" then body else Text.strip (matrixReplyBody event.content body)
        , raw = event.raw
        }

matrixEventText :: Event -> Maybe Text
matrixEventText event
  | event.type_ == "m.room.message" = event.content.body >>= nonEmptyText
  | event.type_ == "m.sticker" =
      Just (maybe "[sticker]" (\label -> "[sticker: " <> label <> "]") (event.content.body >>= nonEmptyText))
  | otherwise = Nothing

matrixReplyBody :: EventContent -> Text -> Text
matrixReplyBody content body
  | isJust content.replyToEventId
  , firstLine : _ <- Text.lines body
  , isMatrixReplyFallbackFirstLine firstLine =
      Text.unlines (dropWhile (Text.isPrefixOf "> ") (Text.lines body))
  | otherwise =
      body

isMatrixReplyFallbackFirstLine :: Text -> Bool
isMatrixReplyFallbackFirstLine line =
  case Text.words line of
    ">" : "*" : userId : _ -> isFallbackUserId userId
    ">" : userId : _ -> isFallbackUserId userId
    _ -> False
  where
    isFallbackUserId token =
      maybe False isMatrixUserId (Text.stripPrefix "<" token >>= Text.stripSuffix ">")

matrixRedactedEventId :: Aeson.Value -> Maybe MatrixEventId
matrixRedactedEventId =
  Aeson.parseMaybe $ Aeson.withObject "Matrix redaction event" \o ->
    (matrixEventId <$> o Aeson..: "redacts")
      <|> do
        content <- o Aeson..: "content"
        Aeson.withObject "Matrix redaction content" (fmap matrixEventId . (Aeson..: "redacts")) content

matrixEventIgnoreReason :: Config -> RoomEvent -> Text
matrixEventIgnoreReason cfg RoomEvent{roomId, event}
  | eventType `notElem` ["m.room.message", "m.sticker"] =
      [i|unsupported event type #{eventType}; #{context}|]
  | isOwnEvent cfg event =
      [i|own event; #{context}|]
  | isEditEvent event =
      [i|edit event; #{context}|]
  | matrixStreamIncomplete event.raw =
      [i|incomplete stream event; #{context}|]
  | isNothing event.content.body =
      [i|missing content.body; #{context}|]
  | Text.null (Text.strip (fromMaybe "" event.content.body)) =
      [i|blank content.body; #{context}|]
  | otherwise =
      [i|unknown reason; #{context}|]
  where
    eventType :: Text
    eventType = event.type_

    eventSender :: Text
    eventSender = event.sender

    eventIdText :: Text
    eventIdText = maybe "<none>" matrixEventIdText event.eventId

    eventMsgtype :: Text
    eventMsgtype = fromMaybe "<none>" event.content.msgtype

    context :: Text
    context =
      [i|room=#{roomId} sender=#{eventSender} event_id=#{eventIdText} msgtype=#{eventMsgtype}|]

matrixMessageDigest :: Config -> MatrixRoomId -> Event -> Text -> MessageDigest
matrixMessageDigest cfg roomId event _body =
  MessageDigest
    { chatIsAllowed = roomAllowed
    , senderIsAllowed = senderSuperuser
    , senderIsSuperuser = senderSuperuser
    , mentionsBot = maybe False (\botId -> botId `elem` event.content.mentions) cfg.userId
    , botId = cfg.userId
    }
  where
    roomAllowed =
      matrixRoomIdText roomId `elem` cfg.allowedRooms
    senderSuperuser =
      event.sender `elem` cfg.superusers

matrixMentions :: Config -> EventContent -> Text -> [Text]
matrixMentions cfg content body =
  case content.mentions of
    [] ->
      [ userId
      | Just userId <- [cfg.userId]
      , userId `Text.isInfixOf` body
      ]
    mentions ->
      mentions

isOwnEvent :: Config -> Event -> Bool
isOwnEvent cfg event =
  cfg.userId == Just event.sender

isEditEvent :: Event -> Bool
isEditEvent event =
  matrixRelationType event.raw == Just "m.replace"

matrixStreamIncomplete :: Aeson.Value -> Bool
matrixStreamIncomplete =
  fromMaybe False . Aeson.parseMaybe parse
  where
    parse =
      Aeson.withObject "Matrix event" \eventObject -> do
        content <- eventObject Aeson..:? "content" Aeson..!= Aeson.Object mempty
        Aeson.withObject "Matrix event content" parseContent content

    parseContent contentObject = do
      metadata <- contentObject Aeson..:? "com.pfeiwu.ai.stream" Aeson..!= MatrixStreamMetadata True
      pure (not metadata.streamComplete)

matrixRelationType :: Aeson.Value -> Maybe Text
matrixRelationType =
  Aeson.parseMaybe $
    Aeson.withObject "Matrix event" \eventObject -> do
      content <- eventObject Aeson..: "content"
      Aeson.withObject "Matrix content" (\contentObject -> contentObject Aeson..: "m.relates_to") content >>=
        Aeson.withObject "Matrix relation" (Aeson..: "rel_type")

defaultConfig :: Config
defaultConfig = Config
  { homeserver = "https://matrix.org"
  , loginUser = Nothing
  , loginPassword = Nothing
  , deviceId = Nothing
  , directRooms = []
  , userId = Nothing
  , allowedRooms = []
  , superusers = []
  }

matrixOutgoingMentionUserIds :: Text -> [Text] -> [Text]
matrixOutgoingMentionUserIds body explicitUserIds =
  Set.toList (Set.fromList (filter isMatrixUserId (explicitUserIds <> matrixUserIdsInText body)))

fetchMatrixMentionNames
  :: (HTTP.HTTP :> es, IOE :> es, KatipE :> es, Concurrent :> es, Prim :> es)
  => Protocol.MatrixDriver
  -> MatrixRoomId
  -> [Text]
  -> Eff es (Map Text Text)
fetchMatrixMentionNames _ _ [] =
  pure Map.empty
fetchMatrixMentionNames driver roomId mentionUserIds = do
  result <- trySync (maybeCall driver (MatrixJoinedMembers roomId))
  case result of
    Left err -> do
      $(logInfo) [i|Matrix mention display names unavailable: #{displayException err}|]
      pure Map.empty
    Right Nothing ->
      pure Map.empty
    Right (Just members) ->
      pure (matrixMentionNames mentionUserIds members)

matrixMentionNames :: [Text] -> JoinedMembersResponse -> Map Text Text
matrixMentionNames mentionUserIds members =
  Map.fromList
    [ (userId, name)
    | userId <- mentionUserIds
    , Just member <- [Map.lookup userId members.joinedMembers]
    , Just name <- [matrixMentionDisplayName =<< member.memberDisplayName]
    ]
data MatrixMediaRef = MatrixMediaRef
  { matrixMediaRefUrl :: !Text
  , matrixMediaRefMimeType :: !(Maybe Text)
  , matrixMediaRefEncrypted :: !(Maybe MatrixEncryptedFile)
  }

data MatrixEncryptedFile = MatrixEncryptedFile
  { encryptedFileUrl :: !Text
  , encryptedFileKey :: !Text
  , encryptedFileIv :: !Text
  , encryptedFileSha256 :: !Text
  }

data MatrixDecryptionPlan = MatrixDecryptionPlan
  { decryptionCipher :: !CryptoAES.AES256
  , decryptionIv :: !(CryptoCipher.IV CryptoAES.AES256)
  , decryptionExpectedHash :: !(CryptoHash.Digest CryptoHash.SHA256)
  }
