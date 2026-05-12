# Cosmobot

Cosmobot 是一个小型 AI agent 框架，接受来自 QQ/OneBot、Telegram 和 Matrix 的消息，同时也可以作为聊天机器人使用。

## 功能

- 支持 QQ/OneBot、Telegram、Matrix 三种聊天入口。
- 支持 OpenAI-compatible Chat Completions 和 streaming 输出接口。
- 支持私聊和群组。
- 支持多种触发方式：命令触发、`@`、机器人名字、
- 支持多轮对话、回复续聊等。
- 支持简易的管理员鉴权。
- 内置 agent 工具：文件读取、目录列表、聊天记录查询、网页搜索/抓取、时间、图片生成、消息发送、群成员查询、计划任务、记忆管理等。
- 内置多种简单命令工具：todo 列表、saucenao 搜图等。
- 支持使用 SQLite 持久化聊天记录、agent trace、会话、计划任务等。

## 构建

目前仅支持 GHC 9.6 和 Cabal 3.14。

```bash
cabal build cosmobot
```

## 配置

Cosmobot 启动时从当前工作目录读取 `config.toml`。

```bash
cp config.example.toml config.toml
```

然后按需要填写平台、LLM、权限和 handler 配置。`[handler.ask]` 必须提供 `system_prompt`，例如：

```toml
[handler.ask]
command = "!ask"
draw_command = "!draw"
system_prompt = "You are a helpful assistant."
agent_max_turns = 4
```

### 主要配置段

- `[log]`：日志级别。
- `[storage]`：SQLite 数据库路径。
- `[driver.qq]`：OneBot websocket 地址、token、机器人 QQ、允许的群/用户和管理员。
- `[driver.telegram]`：Telegram bot token、bot id、允许的 chat 和管理员。
- `[driver.matrix]`：Matrix homeserver、access token、用户 id、允许的 room 和管理员。
- `[llm]`：OpenAI-compatible endpoint、API key、模型、reasoning effort，以及图片生成模型配置。
- `[memory]`：持久记忆目录。
- `[tool]`、`[tool.web_fetch]`、`[tool.web_search]`：agent 工具开关和限制。
- `[handler.ask]`：问答/画图命令、系统提示词和 agent 最大轮数。
- `[handler.saucenao]`：SauceNAO API key。

访问控制按平台配置：

- QQ 会话权限在 `allowed_groups` 和 `allowed_users`。
- Telegram 会话权限在 `allowed_chats`。
- Matrix 会话权限在 `allowed_rooms`。
- 管理员权限在各平台 driver 的 `superusers`。

## 运行

准备好 `config.toml` 后：

```bash
cabal run cosmobot
```

日志中出现 `Cosmobot stand by!` 表示主循环已经启动。

## 对话命令

### 对话

- `!ask <问题>`：发起 LLM 对话。
- `!draw <提示词>`：调用图片生成。
- 回复机器人消息：继续对应会话。
- 回复正在生成的机器人消息并发送 `!halt`：尝试中止该会话。
- 私聊机器人：直接以消息内容作为问题。
- 群聊中提及（`@`）机器人：以消息内容作为问题。

如果 `[handler.ask]` 配置了 `name`，也可以用该前缀触发 ask flow。

### Todo

Todo 按 `platform + senderId` 隔离：

- `!todo <内容>`：添加 todo；不带内容时显示列表。
- `!list`：显示 todo list。
- `!done <编号>`：标记完成。
- `!rm <编号1> <编号2> ...`：删除若干项。
- `!clear`：清空列表。

### SauceNAO

回复一条包含图片的消息并发送：

```text
!saucenao
```

需要在 `[handler.saucenao]` 中配置 `api_key`。当前只返回相似度大于 `90%` 的结果。

### Audit

Audit 命令仅限各平台 `superusers` 使用：

- `!audit`：输出最近 50 条 agent tool use，按从旧到新的顺序排列。
- 回复一条 agent conversation 消息并发送 `!audit`：按 tool call 输出该消息对应的 agent trace。
- 回复一条 agent conversation 消息并发送 `!audit all`：按 tool call 输出该 conversation tree 中所有消息关联的 agent trace。
- 回复一条 agent conversation 消息并发送 `!audit log`：输出原始 agent trace event log。
- `!audit <id>`：输出某条 tool use 的详细信息，包括参数和结果。

列表中的 `id` 是 cosmobot 的 audit id，用来查询详情；`request` 是 LLM tool-call request id，即模型返回的 tool call id。

## Agent 工具

内置工具由 `Bot.Agent.Tools` 聚合，具体实现按领域放在 `Bot.Agent.Tools.*`：

- `Files`：列目录、读文件。
- `Chat`：查询聊天记录、发送回复、提及用户、读取群成员信息。
- `Web`：网页搜索和网页抓取。
- `Time`：日期时间。
- `Image`：图片生成。
- `Schedule`：计划任务。
- `Memory`：个人/群聊记忆管理。
- `Shell`：受控 shell 执行。

部分工具需要配置开关或 API key。例如网页搜索需要启用 `[tool.web_search]`，并选择 `tavily` 或 `brave`。

## Agent Trace

Agent 运行时会记录结构化 trace event：run start/finish、model turn start/finish、tool call start/finish。可以使用 `!audit` 命令查询。
