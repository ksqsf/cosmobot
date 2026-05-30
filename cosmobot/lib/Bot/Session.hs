{-
Module      : Bot.Session
Description : Session lifecycle
Stability   : experimental
-}

module Bot.Session
  -- ( -- * Session lifecycle
  --   Session(..)
  -- , SessionOption(..)
  -- , SessionWheel
  -- , createSession
  -- , destroySession
  -- , kickSession
  -- )
where

-- import Relude
-- import Bot.Agent.Core

-- -- | Session options
-- data Option = Option
--   { initialSystemPrompt :: Text
--   , enableSkills :: Bool
--   }

-- defaultOptions :: Option
-- defaultOptions = Option
--   { initialSystemPrompt = ""
--   , enableSkills = True
--   , enableMemory = True
--   }

-- -- | Session object.
-- data Session es = Session
--   { program :: AgentProgram es
--   , option :: Option
--   }

-- -- | For each session, we create a 
-- createSession :: AgentProgram -> Option -> Eff es Session
-- createSession program opts = do
--   pure Session
--     { option = opts
--     , program = program
--     }

