-- | Command-line entry point for cosmobot.
module Main (main) where

import Bot.Prelude
import qualified Bot.Main as BotMain
import qualified Bot.RPC.Client as RpcClient
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Options.Applicative

main :: IO ()
main =
  execParser commandInfo >>= \case
    Serve configPath -> BotMain.mainWithConfig configPath
    Rpc configPath rpcCommand -> RpcClient.runRpcClientCommand configPath rpcCommand

data Command
  = Serve !FilePath
  | Rpc !FilePath !RpcClient.RpcClientCommand

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
    command "serve"
      ( info (serveParser <**> helper) $
          progDesc "Start the bot using config.toml from the current working directory"
      )
      <> command "rpc"
        ( info (rpcParser <**> helper) $
            progDesc "Call the local cosmobot RPC websocket service"
        )

serveParser :: Parser Command
serveParser = Serve <$> configPathParser

rpcParser :: Parser Command
rpcParser =
  Rpc
    <$> configPathParser
    <*> rpcCommandParser

rpcCommandParser :: Parser RpcClient.RpcClientCommand
rpcCommandParser =
  subparser $
    command "audit"
      ( info (rpcAuditParser <**> helper) $
          progDesc "Query agent audit RPC methods"
      )
      <> command "call"
        ( info (rpcCallParser <**> helper) $
            progDesc "Call an arbitrary RPC method with JSON params"
        )

rpcAuditParser :: Parser RpcClient.RpcClientCommand
rpcAuditParser =
  subparser $
    command "recent"
      ( info (rpcAuditRecentParser <**> helper) $
          progDesc "Show recent agent audit tool uses"
      )
      <> command "show"
        ( info (rpcAuditShowParser <**> helper) $
            progDesc "Show one agent audit tool use by id"
        )
      <> command "thread"
        ( info (rpcAuditThreadParser <**> helper) $
            progDesc "Show agent audit records for a thread message"
        )

rpcAuditRecentParser :: Parser RpcClient.RpcClientCommand
rpcAuditRecentParser =
  RpcClient.RpcAuditRecent
    <$> option auto
      ( long "limit"
          <> metavar "N"
          <> value 20
          <> showDefault
          <> help "Maximum number of audit entries to return"
      )

rpcAuditShowParser :: Parser RpcClient.RpcClientCommand
rpcAuditShowParser =
  RpcClient.RpcAuditShow
    <$> argument auto (metavar "ID")

rpcAuditThreadParser :: Parser RpcClient.RpcClientCommand
rpcAuditThreadParser =
  RpcClient.RpcAuditThread . Text.pack
    <$> argument str (metavar "MESSAGE_ID")

rpcCallParser :: Parser RpcClient.RpcClientCommand
rpcCallParser =
  RpcClient.RpcCall
    <$> (Text.pack <$> argument str (metavar "METHOD"))
    <*> argument jsonReader (metavar "JSON")

jsonReader :: ReadM Aeson.Value
jsonReader = eitherReader \input ->
  Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 (Text.pack input))
