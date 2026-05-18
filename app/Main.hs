-- | Command-line entry point for cosmobot.
module Main (main) where

import Bot.Prelude
import qualified Bot.Main as BotMain
import Options.Applicative

main :: IO ()
main =
  execParser commandInfo >>= \case
    Serve configPath -> BotMain.mainWithConfig configPath

data Command = Serve !FilePath

commandInfo :: ParserInfo Command
commandInfo =
  info (commandParser <**> helper) $
    fullDesc
      <> progDesc "Run cosmobot commands"
      <> header "cosmobot"

configPathParser :: Parser FilePath
configPathParser =
  strOption $
    long "config"
      <> metavar "FILE"
      <> value "config.toml"
      <> showDefaultWith (const "cwd/config.toml")
      <> help "Path to the config TOML file"

commandParser :: Parser Command
commandParser =
  subparser $
    command "serve" $
      info (serveParser <**> helper) $
        progDesc "Start the bot using config.toml from the current working directory"

serveParser :: Parser Command
serveParser = Serve <$> configPathParser
