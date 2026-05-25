# Cosmobot

Cosmobot 是一个小型 AI agent 框架，接受来自 QQ/OneBot、Telegram、Matrix 和 Discord 的消息，同时也可以作为聊天机器人使用。

## 功能

- 支持 QQ/OneBot、Telegram、Matrix、Discord 四种聊天入口。
- 支持 OpenAI-compatible Chat Completions 和 streaming 输出接口。
- 支持私聊和群组。
- 支持多种触发方式：命令触发、`@`、机器人名字等。
- 支持多轮对话、回复续聊等。
- Agent 会在上下文过长时自动整理较早的对话历史，保留最近上下文。
- 支持简易的管理员鉴权。
- 内置 agent 工具：文件读取、目录列表、聊天记录查询、网页搜索/抓取、时间、图片生成、消息发送、群成员查询、计划任务、记忆管理等。
- 内置多种简单命令工具：todo 列表、saucenao 搜图等。
- 支持使用 SQLite 持久化聊天记录、agent audit、会话、计划任务等。

## 构建

目前仅支持 GHC 9.10 和 Cabal 3.14，使用 Stackage LTS 24.42。

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
- `[driver.discord]`：Discord bot token、bot id、允许的 guild/channel/user 和管理员。
- `[llm]`：选择聊天和图片 provider；provider 表里配置 OpenAI-compatible endpoint、API key、模型和请求参数。
- `[memory]`：持久记忆目录。
- `[tool]`、`[tool.web_fetch]`、`[tool.web_search]`：agent 工具开关和限制。
- `[handler.ask]`：问答/画图命令、系统提示词和 agent 最大轮数。
- `[handler.saucenao]`：SauceNAO API key。

访问控制按平台配置：

- QQ 会话权限在 `allowed_groups` 和 `allowed_users`。
- Telegram 会话权限在 `allowed_chats`。
- Matrix 会话权限在 `allowed_rooms`。
- Discord 会话权限在 `allowed_guilds`、`allowed_channels` 和 `allowed_users`。
- 管理员权限在各平台 driver 的 `superusers`。

### Discord 配置

Discord driver 使用 Gateway v10 接收 `MESSAGE_CREATE`，并通过 REST API 发送、编辑、删除、读取消息和上传附件。需要在 Discord Developer Portal 给 bot 开启对应 intents；如果需要读取普通消息正文，还需要启用 Message Content Intent。

```toml
[driver.discord]
bot_token = ""
bot_id = ""
application_id = ""
allowed_guilds = []
allowed_channels = []
allowed_users = []
superusers = []
gateway_host = "gateway.discord.gg"
gateway_path = "/?v=10&encoding=json"
```

`bot_id`、`allowed_users` 和 `superusers` 使用 Discord snowflake 字符串；`allowed_guilds` 和 `allowed_channels` 可写整数 snowflake。Discord 没有 QQ title 的等价能力，所以 `!title` 不支持 Discord 成员头衔设置。

### LLM provider 配置

`[llm]` 使用 `chat = "..."` 和 `image = "..."` 选择 provider。聊天 provider 写在 `[llm.chat_provider.<name>]`，图片 provider 写在 `[llm.image_provider.<name>]`。

```toml
[llm]
chat = "openrouter"
image = "openai"

[llm.chat_provider.openrouter]
base_url = "https://openrouter.ai/api/v1"
api_key = ""
model = "openai/gpt-4o-mini"
reasoning_effort = "low"
timeout = 60

[llm.image_provider.openai]
base_url = "https://api.openai.com/v1"
api_key = ""
model = "gpt-image-1.5"
can_generate = true
can_edit = false
timeout = 300
quality = "medium"
size = "1024x1024"
aspect_ratio = "1:1"
background = "auto"
moderation = "auto"
output_format = "webp"
output_compression = 80
```

`llm.chat_provider.<name>.timeout` 用于普通文本 LLM 请求。非 streaming 请求按总耗时限制；streaming 请求按等待首段或下一段数据的 idle 时间限制。

`llm.image_provider.<name>.timeout` 用于图片生成和编辑请求。`can_generate` 控制是否允许生成图片，`can_edit` 控制是否允许调用图片编辑接口；只有确认当前模型和兼容端点支持图片编辑时才设为 `true`。图片 provider 还可以配置 `quality`、`size`、`aspect_ratio`、`background`、`moderation`、`output_format` 和 `output_compression`。

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

长对话会自动进行上下文整理：当 agent 发送给 LLM 的历史达到 50 条消息时，cosmobot 会保留最近 20 条消息，并把更早的历史总结成一条上下文摘要。触发整理时，机器人会先发送：

```text
正在整理较早的对话上下文...
```

部分耗时工具也会发送进度提示，例如图片生成工具会提示正在调用工具，并附带 audit id，方便用 `!audit <id>` 查询详情。

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
- `Emacs`：通过 `emacsclient --socket-name cosmobot --eval` 在 cosmobot 管理的 Emacs daemon 中执行 Emacs Lisp。

部分工具需要配置开关或 API key。例如网页搜索需要启用 `[tool.web_search]`，并选择 `tavily` 或 `brave`。

## Agent Middleware

Agent 的核心循环负责模型请求、工具调用和 conversation 推进；横切行为由 `Bot.Agent.Middleware.*` 提供：

- `ContextCompaction`：在历史达到 50 条消息时整理较早上下文，保留最近 20 条消息。
- `Observation`：记录 agent run、model turn、tool call 和 conversation link 事件，并把 audit id 放入 typed middleware context。
- `Tools`：处理工具调用失败、工具轮数限制，以及 noisy tool 的用户可见进度提示。

Middleware 之间通过 typed `MiddlewareContext` 传递数据，避免把临时字段塞进 agent core 或工具上下文。

## Agent Audit

Agent 运行时会记录结构化 trace event：run start/finish、model turn start/finish、tool call start/finish。可以使用 `!audit` 命令查询。
