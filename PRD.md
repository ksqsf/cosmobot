## Goal

- Implement a minimal viable Agent Client Protocol support for the Cosmobot server.
- Remove the "chat" custom protocol in the RPC server.

## Protocol specification

- https://agentclientprotocol.com/llms.txt

## Validation

Start a local server:

```
cabal run cosmobot -- server
```


It listens on 0.0.0.0:38765 (RPC) and 38766 (ACP).

You may use pre-provided `config.toml`, it has real API keys.

Connect to the server using ACPX (installed).

## Requirements

- After each phase, validate and make a commit.
- Always use fast-feedback in the haskell skill.
- Do not mark DONE unless you validated your implementation.
- Follow the coding style very strictly.
- Do not catch SomeException unless you are sure async exceptions should be caught.

## Plan

### DONE Phase 1: Initialization and authentication

- Implement the very basic authentication.
- Start up the ACP server along .
- Use the RPC chat server as a reference.
- We do not need to support logout.

Organization:
- Bot.ACP.Config: Support [acp] in config.toml. '[acp].token' specifies the authentication token.
- Bot.ACP.Types: Basic ACP protocol types.
- Bot.ACP.Server: The very basic JSONRPC server that listens and handles the ACP requests, and it should use [acp].token as the authentication method.

Envisioned changes:
- Bot.Main may initialize and start the ACP server.

Goal: ACPX may connect to the server.

### TODO Phase 2: Session creation and session deletion

- Reuse facilities from Bot.Session.
- Create sessions.
- Delete sessions.
- You can send messages and receive text responses.
- Streaming outputs.

Goal: You can receive responses from the server, streaming.

### TODO Phase 3: Tool calling

- Support tool calling.
- It may Just Work, but you need to validate it, including the image generation tool.
