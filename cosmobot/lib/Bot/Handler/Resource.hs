{-|
Module      : Bot.Handler.Resource
Description : Commands for chat-owned long-running resources
Stability   : experimental
-}
module Bot.Handler.Resource
  ( resourceHandlers
  , resourceIds
  , removeResources
  )
where

import Bot.Core.Message
import Bot.Core.Route
import qualified Bot.Effect.Chat as Chat
import qualified Bot.Effect.Resource as Resource
import Bot.Prelude
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

resourceHandlers :: (Chat.Chat :> es, Resource.Resource :> es) => [RouteHandler es]
resourceHandlers =
  [ stopOn (command "!res/ls") handleList
  , stopOn (command "!res/rm") handleRemove
  ]

resourceIds :: Text -> [Resource.ResourceId]
resourceIds = List.nub . Text.words

handleList :: (Chat.Chat :> es, Resource.Resource :> es) => IncomingMessage -> Text -> Eff es ()
handleList message _ =
  withAccess message \access -> Resource.list access >>= \case
    [] -> reply message "No resources."
    resources -> reply message (renderResources resources)

handleRemove :: (Chat.Chat :> es, Resource.Resource :> es) => IncomingMessage -> Text -> Eff es ()
handleRemove message input =
  case resourceIds input of
    [] -> reply message "Usage: !res/rm <resource_id>..."
    resourceIds_ ->
      withAccess message \access -> do
        results <- removeResources access resourceIds_
        reply message (Text.unlines results)

removeResources :: Resource.Resource :> es => Resource.ResourceAccess -> [Resource.ResourceId] -> Eff es [Text]
removeResources access = traverse \resourceId ->
  Resource.destroy access resourceId <&> \case
    Right () -> "- `" <> resourceId <> "`: removed"
    Left Resource.ResourceCleanupFailed{} -> "- `" <> resourceId <> "`: cleanup failure"
    Left _ -> "- `" <> resourceId <> "`: not found/not owned"

withAccess
  :: Chat.Chat :> es
  => IncomingMessage
  -> (Resource.ResourceAccess -> Eff es ())
  -> Eff es ()
withAccess message action =
  either (const (reply message identityRequired)) action (Resource.accessFromMessage message)

reply :: Chat.Chat :> es => IncomingMessage -> Text -> Eff es ()
reply message = void . Chat.replyTo message

identityRequired :: Text
identityRequired = "Resource operations require chat and sender identity."

renderResources :: [Resource.SomeResourceObject] -> Text
renderResources resources =
  Text.intercalate "\n" $ map renderGroup $ Map.toAscList $ Map.fromListWith (<>)
    [(resource.resourceType, [resource]) | resource <- resources]
  where
    renderGroup (resourceType, entries) =
      Text.unlines $
        ("Resource type " <> resourceType <> ":")
          : map renderEntry (List.sortOn (.resourceId) entries)
    renderEntry resource =
      "- `" <> resource.resourceId <> "` (agent_id=`" <> resource.agentId <> "`): " <> resource.description
