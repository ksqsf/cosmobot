import type { Activity, LogEntry, PlatformSummary, Plugin, Task } from '@/types/domain'

export const tasks: Task[] = [
  { id: '1842', label: 'agent.run', detail: 'Review storage refactor', owner: 'run_8f2c', platform: 'RPC', status: 'running', started: '14:28:16', elapsed: '04:12' },
  { id: '1841', label: 'telegram.reader', detail: 'Long-running driver task', owner: 'system', platform: 'Telegram', status: 'waiting', started: '09:41:02', elapsed: '04:51:26' },
  { id: '1840', label: 'matrix.sync', detail: 'Room event stream', owner: 'system', platform: 'Matrix', status: 'running', started: '09:41:02', elapsed: '04:51:26' },
  { id: '1839', label: 'qq.dispatch', detail: 'Incoming message stream', owner: 'system', platform: 'QQ', status: 'waiting', started: '09:41:04', elapsed: '04:51:24' },
  { id: '1838', label: 'discord.reader', detail: 'Gateway connection', owner: 'system', platform: 'Discord', status: 'running', started: '09:41:03', elapsed: '04:51:25' },
  { id: '1831', label: 'tool.web_fetch', detail: 'Request to example.org', owner: 'run_2e09', platform: 'RPC', status: 'failed', started: '14:23:09', elapsed: '00:08' },
]

export const activity: Activity[] = [
  { id: 'a1', kind: 'Agent completed', summary: '“Summarize release notes”', source: 'RPC · run_8f2c', time: 'just now', tone: 'success' },
  { id: 'a2', kind: 'Tool called', summary: 'query_chat_log', source: 'Telegram', time: '32 seconds ago', tone: 'info' },
  { id: 'a3', kind: 'Message received', summary: 'from @paperboat', source: 'Matrix', time: '1 minute ago', tone: 'info' },
  { id: 'a4', kind: 'Tool failed', summary: 'HTTP request timed out', source: 'Discord', time: '4 minutes ago', tone: 'danger' },
  { id: 'a5', kind: 'Plugin reloaded', summary: 'cosmobot-weather', source: 'System', time: '12 minutes ago', tone: 'warning' },
]

export const platforms: PlatformSummary[] = [
  { id: 'qq', name: 'QQ', state: 'Connected', messages: 482 },
  { id: 'telegram', name: 'Telegram', state: 'Connected', messages: 391 },
  { id: 'matrix', name: 'Matrix', state: 'Connected', messages: 274 },
  { id: 'discord', name: 'Discord', state: 'Degraded', messages: 137 },
]

export const plugins: Plugin[] = [
  { id: 'echo', name: 'Echo tools', description: 'Echo and transformation tools.', version: '1.3.0', tools: 2, routes: 1, status: 'Loaded' },
  { id: 'github', name: 'GitHub', description: 'Repository lookup, issues, pull requests, and workflow status.', version: '0.8.2', tools: 7, routes: 2, status: 'Loaded' },
  { id: 'weather', name: 'Weather', description: 'Current conditions and forecasts.', version: '0.4.0', tools: 2, routes: 1, status: 'Stopped', error: 'Missing WEATHER_API_KEY' },
  { id: 'codesearch', name: 'Code search', description: 'Indexed symbol and text search over registered workspaces.', version: '2.1.1', tools: 4, routes: 1, status: 'Loaded' },
]

export const logs: LogEntry[] = [
  { id: 'l1', time: '14:32:31.108', level: 'INFO', source: 'Bot.Agent', message: 'Agent run completed', fields: 'run_id=run_8f2c turns=2 tools=5' },
  { id: 'l2', time: '14:32:29.704', level: 'DEBUG', source: 'Bot.RPC.Server', message: 'RPC request completed', fields: 'method=audit.recent duration_ms=18' },
  { id: 'l3', time: '14:32:14.008', level: 'INFO', source: 'Bot.Chat.Driver.Telegram', message: 'Incoming message normalized', fields: 'chat_id=629104 sender_id=8872' },
  { id: 'l4', time: '14:31:57.991', level: 'WARN', source: 'Bot.Chat.Driver.Discord', message: 'Gateway heartbeat delayed', fields: 'latency_ms=1840' },
  { id: 'l5', time: '14:30:41.612', level: 'ERROR', source: 'Bot.Agent.Tools.Web', message: 'HTTP request timed out', fields: 'tool_call_id=call_d88 timeout_seconds=30' },
]
