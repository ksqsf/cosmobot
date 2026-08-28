{-|
Module      : Bot.Effect.Skills
Description : Startup skill metadata capability facade
Stability   : experimental
-}

module Bot.Effect.Skills
  ( Skills
  , skillsSystemPrompt
  , loadSkill
  , loadSkillFile
  , listSkills
  , removeSkill
  , reloadSkills
  , runSkills
  )
where

import Bot.Prelude
import qualified Bot.Skills as SkillsStore
import qualified Effectful.Prim.IORef as IORef
import qualified Effectful.Concurrent.MVar as MVar
import Effectful.FileSystem (FileSystem)

data Skills :: Effect where
  SkillsSystemPrompt :: Skills m Text
  LoadSkill :: Text -> Skills m (Maybe Text)
  LoadSkillFile :: Text -> FilePath -> Skills m (Maybe Text)
  ListSkills :: Skills m [SkillsStore.SkillMetadata]
  RemoveSkill :: Text -> Skills m Bool
  ReloadSkills :: Skills m ()

type instance DispatchOf Skills = Dynamic

skillsSystemPrompt :: Skills :> es => Eff es Text
skillsSystemPrompt =
  send SkillsSystemPrompt

loadSkill :: Skills :> es => Text -> Eff es (Maybe Text)
loadSkill =
  send . LoadSkill

loadSkillFile :: Skills :> es => Text -> FilePath -> Eff es (Maybe Text)
loadSkillFile name path =
  send (LoadSkillFile name path)

listSkills :: Skills :> es => Eff es [SkillsStore.SkillMetadata]
listSkills =
  send ListSkills

removeSkill :: Skills :> es => Text -> Eff es Bool
removeSkill =
  send . RemoveSkill

reloadSkills :: Skills :> es => Eff es ()
reloadSkills =
  send ReloadSkills

runSkills
  :: (Concurrent :> es, FileSystem :> es, Prim :> es)
  => SkillsStore.SkillsConfig
  -> Eff (Skills : es) a
  -> Eff es a
runSkills cfg action = do
  promptRef <- IORef.newIORef =<< SkillsStore.loadSkillsPrompt cfg
  lock <- MVar.newMVar ()
  interpret (\_ -> \case
    SkillsSystemPrompt -> withSkillsLock lock ((.systemPrompt) <$> IORef.readIORef promptRef)
    LoadSkill name -> withSkillsLock lock (SkillsStore.skillContent name <$> IORef.readIORef promptRef)
    LoadSkillFile name path -> withSkillsLock lock do
      prompt <- IORef.readIORef promptRef
      case find ((== name) . (.name)) prompt.metadata of
        Nothing -> pure Nothing
        Just skill -> SkillsStore.loadSkillFile skill path
    ListSkills -> withSkillsLock lock ((.metadata) <$> IORef.readIORef promptRef)
    RemoveSkill name -> withSkillsLock lock do
      prompt <- IORef.readIORef promptRef
      case find ((== name) . (.name)) prompt.metadata of
        Nothing -> pure False
        Just skill -> do
          removed <- SkillsStore.removeSkill cfg skill
          IORef.writeIORef promptRef =<< SkillsStore.loadSkillsPrompt cfg
          pure removed
    ReloadSkills -> withSkillsLock lock (IORef.writeIORef promptRef =<< SkillsStore.loadSkillsPrompt cfg)
    ) action

withSkillsLock :: Concurrent :> es => MVar.MVar () -> Eff es a -> Eff es a
withSkillsLock lock operation =
  MVar.withMVar lock (const operation)
