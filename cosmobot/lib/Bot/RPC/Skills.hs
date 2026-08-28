{-|
Module      : Bot.RPC.Skills
Description : Loaded skill inspection JSON-RPC methods
Stability   : experimental
-}

module Bot.RPC.Skills
  ( skillsRpcCallbacks
  )
where

import qualified Bot.Effect.Skills as Skills
import qualified Bot.JSONRPC as RPC
import Bot.Prelude
import Bot.RPC.Server (RpcServerCallbacks (..), noRpcServerCallbacks)
import qualified Bot.Skills as SkillsStore
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text

skillsRpcCallbacks :: Skills.Skills :> es => RpcServerCallbacks es
skillsRpcCallbacks =
  noRpcServerCallbacks
    { skillsMethod = dispatchSkillsMethod
    , supportedMethods = ["skills.list", "skills.get", "skills.remove"]
    }

dispatchSkillsMethod
  :: Skills.Skills :> es
  => RPC.RpcRequest
  -> Eff es (Maybe (Either RPC.RpcError Aeson.Value))
dispatchSkillsMethod request =
  case RPC.requestMethod request of
    "skills.list" -> Just <$> parseParams request parseNoParams (const listSkills)
    "skills.get" -> Just <$> parseParams request parseSkillName getSkill
    "skills.remove" -> Just <$> parseParams request parseSkillName removeSkill
    _ -> pure Nothing

listSkills :: Skills.Skills :> es => Eff es Aeson.Value
listSkills = do
  skills <- Skills.listSkills
  pure $ Aeson.object ["skills" Aeson..= map skillSummaryValue skills]

getSkill :: Skills.Skills :> es => Text -> Eff es Aeson.Value
getSkill name = do
  content <- Skills.loadSkill name
  pure $ maybe Aeson.Null (\value -> Aeson.object ["name" Aeson..= name, "content" Aeson..= value]) content

removeSkill :: Skills.Skills :> es => Text -> Eff es Aeson.Value
removeSkill name = do
  removed <- Skills.removeSkill name
  pure (Aeson.object ["name" Aeson..= name, "removed" Aeson..= removed])

skillSummaryValue :: SkillsStore.SkillMetadata -> Aeson.Value
skillSummaryValue skill =
  Aeson.object
    [ "name" Aeson..= skill.name
    , "description" Aeson..= skill.description
    ]

parseParams
  :: RPC.RpcRequest
  -> (Aeson.Value -> AesonTypes.Parser a)
  -> (a -> Eff es Aeson.Value)
  -> Eff es (Either RPC.RpcError Aeson.Value)
parseParams request parser action =
  case AesonTypes.parseEither parser (RPC.requestParams request) of
    Left err -> pure (Left (RPC.rpcError "invalid_params" (toText err)))
    Right value -> Right <$> action value

parseNoParams :: Aeson.Value -> AesonTypes.Parser ()
parseNoParams Aeson.Null = pure ()
parseNoParams value = Aeson.withObject "empty params" (\o -> unless (null o) (fail "params must be empty")) value

parseSkillName :: Aeson.Value -> AesonTypes.Parser Text
parseSkillName = Aeson.withObject "skills.get params" \o -> do
  name <- Text.strip <$> o Aeson..: "name"
  when (Text.null name) (fail "name must not be empty")
  pure name
