# Cosmobot

Cosmobot is a lightweight and extensible AI agent framework. It can receive user messages from [Matrix](https://matrix.org/), Telegram, QQ (OneBot), and Discord. It can even act like a ChatBot in a group chat for fun!

Beware: Cosmobot is just a hobby project. Won't be big and professional like OpenClaw. /jk

## Features

- **Multiple platforms**: Matrix, Telegram, QQ (OneBot), or Discord.
- **Multiple interfaces**: Both Private Chat and Group Chat are supported.
- **Any LLM provider**: OpenAI-compatible API is supported.
- **Capable**: Image generation/editing; Shell scripting; File sending; and the compulsory Web searching and fetching...
- **Extendable**: Carefully designed abstractions allowing for super easy extension.
- **C/S architecture**: Build peripheral devices upon the core.
- **Observable & Auditable**: Audit traces, observability over RPC
- **Lean**: less CPU and RAM consumption; VPS-friendly.

## Building

Currenly, only GHC 9.10.3 and Cabal 3.14 are supported. Package versions are pinned to [Stackage LTS 24.42](https://www.stackage.org/lts-24.42). After [setting up your toolchain](https://www.haskell.org/ghcup/), to build the whole project, run:

```bash
cabal build -j all
```

Then you can find the executable path by running:

```bash
cabal list-bin exe:cosmobot
```

By default, the binary is statically linked with Haskell dependencies, so you can probably deploy it by simply `rsync`'ing. The cost is the size of the binary. ;-)

## **WARNING**

Cosmobot is still in infancy. We do not recommend you use Cosmobot in any critical scenario.

Regarding **Security**, currently, Cosmobot has ZERO security features except `superusers`! And we strongly advise you against running the agent unprotected!

- Whenever the agent reacts to a message sender, the list of available tools is determined by whether they are a superuser. Privileged tools are only visible to the agent if it is responding to a superuser request.
- However, this does NOT prevent prompt injection. A malicious chat message or webpage is very much possible to fool your agent to do dangerous things!

Regarding **Privacy**, we have done our best effort. For example, Alice can never query Bob's chat log. But that's about it.

## Cosmobot Deployment

Cosmobot reads `config.yaml` from the current working directory. If it's listening for messages, you should see "Cosmobot stand by!" in the logs.

There is a template config to get you started.

```
cp cosmobot/config.example.toml config.toml
```

But, unfortunately, you need to set Cosmobot up correctly before it can do anything useful.

### 1. Chat interfaces

You need to connect Cosmobot to one of the chat platforms. Or, if you like, all of them.

```toml
[driver.matrix]
homeserver = "https://matrix.org"
bot_id = "@your_bot_id:matrix.org"
login_user = "your_bot_id"
login_password = "your_bot_password"
device_id = "the_device_id_shown_in_the_device_list"
superusers = ["@your_id:matrix.org"]
allowed_rooms = ["!aBcDeFgH:matrix.org"]  # Bots can receive messages from members in these rooms

[driver.telegram]
bot_token = "1111111111:AAaaaaaaaaaaaaaaaaaa_PPPPPPPPPPPPPP"  # Get your bot token from @BotFather
bot_id = "MyOwnBot"                # Bot ID
superusers = ["ksqsf"]             # Your own id
allowed_chats = ["some_group_chat", -100000000]  # Bot can receive messages from these chats

[driver.qq]
host = "127.0.0.1"                  # OneBot server host
port = 3001                         # OneBot server port
path = "/"                          # OneBot server API path
token = "some token"                # Configured token
bot_id = 114514114514               # Your bot's QQ number
superusers = [23333333]             # Your own QQ number
allowed_groups = [11111111, 222222] # Bot can receive messages from members in these groups
allowed_users = [33333333]          # Bot can receive messages from these private chats

[driver.discord]
bot_token = "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM"
bot_id = "1500000000000000000"
application_id = "1555555555555555555"
allowed_guilds = []
allowed_channels = []
allowed_users = []
superusers = ["111111111111111111"]    # Your own user ID
gateway_host = "gateway.discord.gg"    # You probably don't need to change
gateway_path = "/?v=10&encoding=json"  # Ditto
```

Common options:

- `superusers`: A list of platform user IDs. These users are considered *you*, and can utilize the full capabilities. **Specify correctly and verify thrice!!**
- `bot_id`: Bot's own ID on that platform. Used for identifying whether Bot is mentioned. Please specify correctly.
- `allowed_{rooms,chats,groups,users}`: Bot is allowed to react to messages from these private or group chats. If it is a group chat, Bot can react to all members in the group. This is a Cosmobot peculiarity because it can also act as a shared chatbot. If you only use Cosmobot privately, just don't set them; `superusers` is enough.

Special notes:

- Currently, Matrix and Telegram are the best supported platforms. QQ support is mature. You may experience bugs with Discord support.
- **Matrix**: 
  + Cosmobot by itself does not support Matrix's E2EE. You need a proxy to do encryption and decryption for it (e.g. [Pantalaimon](https://github.com/matrix-org/pantalaimon)).
- **QQ**: 
  + [NapCat](https://napneko.github.io/) is recommended. Only "WebSocket Server" mode is supported.
  + QQ does not support streaming output and "bot is typing" notification.
- **Discord**: (Gateway v10)
  + Make sure to enable Intents in Discord Developer Portal. For example, you really need to enable Message Content Intent as the bot needs to read message contents.


Cosmobot on Matrix is highly recommended. It has the best Markdown support (including tables!), streaming, typing feedback, and all Cosmobot features. With some effort, you can talk to Cosmobot in an E2EE room which gives you superior privacy protection.


### LLM providers

Cosmobot supports three types of models, Chat (or Text), Image, and Audio. Chat is required. You need to select one provider for each type after defining a Provider.

```toml
[llm]
chat = "openai"       # that is, [llm.chat_provider.openai],  *required*
image = "gptimage2"   # that is, [llm.image_provider.gptimage2], *optional*
audio = "zz"          # that is, [llm.audio_provider.test], *optional*

[llm.chat_provider.openai]
base_url = "https://api.openai.com/v1"
api_key = "sk-something"
model = "gpt-5.5"
reasoning_effort = "low"
timeout = 120

[llm.chat_provider.openrouter]  # You can define multiple providers to easily switch between them
base_url = "https://api.openrouter.ai/v1"
api_key = "sk-something"
model = "openai/gpt-5.5"
reasoning_effort = "low"
timeout = 120

[llm.image_provider.gptimage2]
base_url = "https://api.openai.com/v1"
api_key = "sk-something"
model = "gpt-image-2"
can_generate = true    # does this model support images/generation?
can_edit = true        # does this model support images/edit?
timeout = 600
quality = "high"       # default quality
size = "auto"          # default size

[llm.audio_provider.zz]
base_url = "https://something"
api_key = "sk-something"
model = "gemini-2.5-flash-tts"
voice = "zephyr"
response_format = "mp3"
timeout = 300
speed = 1.1
instructions = ""
```

### Personalize your agent

Cosmobot follows a modular design and the Agent is only contacted through a Handler called `Ask`. Therefore, you need to configure `[handler.ask]`:

```toml
name = "Doraemon"                # Your agent's name
command = "!ask"                 # The "handler command" way to start a conversation
draw_command = "!draw"           # The "handler command" way to draw
agent_max_turns = 12             # Max number of tool turms
context_compaction_threshold_ktokens = 1000 # Compact after provider-reported usage reaches 1M tokens
system_prompt = "You are Doraemon, an AI agent powered by Cosmobot. Respond concisely."
```

You must provide `system_prompt`. (Actually, this is not the full system prompt provided to the agent. The Ask handler will also add information about the chat, the sender, memories, skills, and tools. For typical uses, it's not necessary to be too verbose here. A few sentences will do.)

So far, you should be able to contact your agent and get it to respond! Next, we will equip the agent with a lot of tools.

### Configure tools

Enable date time:

```toml
[tool]
datetime = true
```

Enable web searching:

```toml
[tool.web_search]
enable = true
api = "tavily"       # tavily | brave
max_results = 20
tavily_api_key = "tvly-dev-something"
brave_api_key = "some-token"
```

Enable web fetching (does not require an API):

```toml
[tool.web_fetch]
enable = true
max_uses = 5         # Max number of calling to this tool
max_content_tokens = 50000
```

Note: To use the `typst_render` tool, you need to have a binary called `typst` on `PATH`.

### Memory

Cosmobot has a simple "memory" system. Memory is scoped by either a message sender, or a chat. That is, you can have Bot recognize you, or behave differently in different group chats, or even just you but in different chats!

By default, a memory is a Markdown file stored at `<dir>/<platform>/<id>.md`. You can symlink files to share memory.

```toml
[memory]
dir = "memory"
```

### Skills

Cosmobot has a simple "skill" system. It basically follows the agentskills.io spec.

```toml
[skills]
dir = "skills"
```

### Media object cache

Different platforms have different policies regarding media files, like images or videos. Cosmobot handles this with a local media cache system: each incoming media file will be cached locally, and has a public URL for external access. This means we need to take care of garbage collection as well as finding a public URL.

You can configure [Cloudflare R2](https://www.cloudflare.com/products/r2/) or any S3-compatible service, so that media files get uploaded automatically, and you get public URLs. Or you can disable S3 uploading, and serve these files from your server.

```toml
[media]
cache_dir = "./cache"
public_base_url = "https://s3.your_custom_domain.com/"
compression_format = "webp"
compression_level = 95

[media.gc]
enabled = true
older_than_days = 7      # any media files whose last use date is 7 days ago are deleted locally
interval_hours = 24      # runs gc every day

[media.s3]
enabled = true           # Do you want S3 uploading?
bucket = "cosmobot-cache" # Bucket name
region = "auto"           # Region
endpoint = "https://???.r2.cloudflarestorage.com/"  # Endpoint
prefix = "cosmobot/media"
public_read_acl = false
addressing_style = "path"
access_key_id = "xxxxxxxxxxxxxx"
secret_access_key = "yyyyyyyyyyyyy"
```

If you enable S3, the public URL of each media file will be `public_base_url + prefix + object_id.ext`. Otherwise, it is just `public_base_url + object_id.ext`. Make sure your server config points to the correct root.

### RPC

RPC provides an alternative way to interact with Cosmobot. We recommend you to enable it, as it provides observablity and advanced management tools.

```
[rpc]
enabled = true
host = "127.0.0.1"         # listen to host
port = 38765               # listen to port
token = "a random string"  # keep it secure!
```

Keep `token` really secure, as it gives unrestricted access to Cosmobot. We strongly advise you against setting `host` to `0.0.0.0`. Instead, use solutions like [SSH port forwarding](https://www.digitalocean.com/community/tutorials/ssh-port-forwarding) or [ZeroTier](https://www.zerotier.com/) to access your server's specific port.

## Interact with Cosmobot

The primary interface currently is Chat. Equipped with many tools, Cosmobot is actually more capable than a typical AI chat app.

### Chat with Cosmobot

In an allowed room/chat/group, or directly send messages from superuser/an allowed user,

- start a new conversation
  + `!ask <anything>`
  + `<bot_name> <anything>`
  + `<anything> @botid <anything>` (mention bot)
  + (only in private chats) directly send `<anything>`
- continue/fork a conversation
  + reply to a bot's response

### Audit (Superuser-only)

- send `!audit` directly
- reply to a bot's response with `!audit`
- `!audit <id>` for details

### RPC

See

```bash
cabal run exe:cosmobot -- rpc --help
```

## Other features

The following can happen automatically:

- Recording all chat messages so Bot can search
- Context compaction
- Tool result compaction
- "Noisy" tools can send notifications (with audit ID)

## Other Handlers

### Todo list

Todos are scoped by `(platform,senderId)`:

- `!todo <task>`: add a todo item
- `!list`
- `!done <id>`
- `!rm <id1> <id2> ...`
- `!clear`

### SauceNAO

Reply to a message that contains an image:

```text
!saucenao
```

You need to provide `api_key` in `[handler.saucenao]`. Currently, only the top result with similarity > 90% is reported.

## Agent tools

Currently, the following tools are available:

| Category | Name              | Privileged? |
|----------|-------------------|-------------|
| Files    | `list_directory`  | Yes         |
| Files    | `read_file`       | Yes         |
| Chat     | `chat_log`        |             |
| Chat     | `sender_chat_log` |             |
| Chat     | `send_reply`      |             |
| Chat     | `send_file`       | Yes         |
| Chat     | `mention_user`    |             |
| Chat     | `sender_info`     |             |
| Chat     | `member_info`     |             |
| Chat     | `user_avatar`     |             |
| Chat     | `group_members`   |             |
| Chat     | `message_info`    |             |
| Emacs    | `emacs_eval`      | Yes         |
| Audio    | `audio_generate`  |             |
| Image    | `image_generate`  |             |
| Image    | `image_edit`      |             |
| Image    | `image_cache`     |             |
| Media    | `media_text`      |             |
| Memory   | `sender_memory`   |             |
| Memory   | `chat_memory`     |             |
| Schedule | `schedule`        |             |
| Schedule | `delete_schedule` |             |
| Schedule | `list_schedules`  |             |
| Shell    | `run_bash`        | Yes         |
| Time     | `now`             |             |
| Typst    | `typst_render`    |             |
| Web      | `search_web`      |             |
| Web      | `fetch_url`       |             |

Note:

- If the current message sender is not superuser, the priviledged tools will be not visible to the agent.
- Some tools may be turned off in the config.
- Most tools are always available.
