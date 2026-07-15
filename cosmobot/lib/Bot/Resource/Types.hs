{-# LANGUAGE AllowAmbiguousTypes #-}

module Bot.Resource.Types
  ( ResourceId
  , ResourceOwner (..)
  , ResourceAccess (..)
  , ResourceScope (..)
  , Init (..)
  , ResourceObject (..)
  , ResourcePersistence (..)
  , ResourceLoader (..)
  , resourceLoader
  , SomeResourceObject (..)
  , ResourceError (..)
  , ownerFromMessage
  , accessFromMessage
  )
where

import Bot.Core.Message
import Bot.Prelude

type ResourceId = Text

data ResourceOwner = ResourceOwner
  { platform :: !ChatPlatform
  , chatId :: !Text
  , senderId :: !Text
  }
  deriving stock (Eq, Ord, Show)

data ResourceAccess = ResourceAccess
  { owner :: !ResourceOwner
  , superuser :: !Bool
  }
  deriving stock (Eq, Show)

data ResourceScope = PersonResource | ChatResource
  deriving stock (Eq, Show)

data Init a = Init
  { message :: !IncomingMessage
  , arguments :: !a
  }

class Typeable a => ResourceObject m a where
  type CreationArgs a
  resourceTypeName :: proxy a -> Text
  resourceScope :: proxy a -> ResourceScope
  resourceScope _ = PersonResource
  resourcePersistence :: proxy a -> ResourcePersistence m a
  resourcePersistence _ = EphemeralResource
  createResourceObject :: Init (CreationArgs a) -> m (Either Text a)
  destroyResourceObject :: a -> m (Either Text ())
  describeResourceObject :: a -> Either Text Text -> m Text
  probeResourceObject :: a -> m (Either Text Text)
  detailResourceObject :: Monad m => a -> m Text
  detailResourceObject object = probeResourceObject object >>= describeResourceObject object

data ResourcePersistence m a
  = EphemeralResource
  | PersistentResource
      { encodeResource :: a -> Text
      , restoreResource :: Text -> m (Either Text a)
      }

data ResourceLoader m where
  ResourceLoader :: ResourceObject m a => Proxy a -> ResourceLoader m

resourceLoader :: forall a m. ResourceObject m a => ResourceLoader m
resourceLoader = ResourceLoader (Proxy @a)

data SomeResourceObject = SomeResourceObject
  { resourceId :: !ResourceId
  , resourceType :: !Text
  , sessionId :: !(Maybe Text)
  , description :: !Text
  , probeResult :: !(Either Text Text)
  }
  deriving stock (Eq, Show)

data ResourceError
  = MissingResourceIdentity
  | ResourceNotFoundOrNotOwned
  | ResourceTypeMismatch
  | ResourceUnavailable
  | InvalidResourceName
  | ResourceNameAlreadyExists
  | ResourceCreationFailed !Text
  | ResourceRenameFailed !Text
  | ResourceCleanupFailed !Text
  deriving stock (Eq, Show)

ownerFromMessage :: IncomingMessage -> Either ResourceError ResourceOwner
ownerFromMessage message =
  ResourceOwner message.platform
    <$> maybeToRight MissingResourceIdentity (show <$> message.chatId <|> listToMaybe message.chatAliases)
    <*> maybeToRight MissingResourceIdentity message.senderId

accessFromMessage :: IncomingMessage -> Either ResourceError ResourceAccess
accessFromMessage message =
  ResourceAccess <$> ownerFromMessage message <*> pure message.digest.senderIsSuperuser
