import type { LogEntry } from '@/types/domain'

export const logs: LogEntry[] = [
  { id: 'l1', time: '14:32:31.108', level: 'INFO', source: 'Bot.Agent', message: 'Agent run completed', fields: 'run_id=run_8f2c turns=2 tools=5' },
  { id: 'l2', time: '14:32:29.704', level: 'DEBUG', source: 'Bot.RPC.Server', message: 'RPC request completed', fields: 'method=audit.recent duration_ms=18' },
  { id: 'l3', time: '14:32:14.008', level: 'INFO', source: 'Bot.Chat.Driver.Telegram', message: 'Incoming message normalized', fields: 'chat_id=629104 sender_id=8872' },
  { id: 'l4', time: '14:31:57.991', level: 'WARN', source: 'Bot.Chat.Driver.Discord', message: 'Gateway heartbeat delayed', fields: 'latency_ms=1840' },
  { id: 'l5', time: '14:30:41.612', level: 'ERROR', source: 'Bot.Agent.Tools.Web', message: 'HTTP request timed out', fields: 'tool_call_id=call_d88 timeout_seconds=30' },
]
