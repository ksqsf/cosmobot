import { z } from 'zod'

export const chatMethods = [
  'chat.open_session',
  'chat.list_sessions',
  'chat.get_session',
  'chat.history',
  'chat.fork',
  'chat.rename_session',
  'chat.delete_session',
  'chat.upload_attachment',
  'chat.send',
  'chat.subscribe',
  'chat.unsubscribe',
  'media.delete',
] as const

export const liveAdminMethods = [
  'concurrency.list',
  'concurrency.lookup',
  'concurrency.cancel',
  'audit.recent',
  'audit.count',
  'audit.search',
  'audit.get',
  'audit.run',
  'audit.thread',
  'audit.subscribe',
  'thread.list',
  'thread.get',
  'thread.resolve_run',
  'thread.active',
  'thread.halt',
  'memory.list',
  'memory.get',
  'memory.history',
  'memory.get_revision',
  'memory.revert',
  'skills.list',
  'skills.get',
  'skills.remove',
  'chat_log.list',
  'chat_log.window',
  'media.stats',
  'media.search',
  'media.get',
  'media.gc',
  'resource.list',
  'resource.detail',
  'resource.destroy',
  'resource.rename',
  'resource.keep_alive',
  'resource.make_permanent',
  'resource.list_associated',
  'resource.destroy_associated',
  'plugin.list',
  'plugin.load',
  'plugin.reload',
  'plugin.unload',
  ...chatMethods,
] as const
export type LiveAdminMethod = typeof liveAdminMethods[number]

export const rpcErrorSchema = z.object({
  code: z.number(),
  message: z.string(),
  data: z.object({ code: z.string() }).loose().optional(),
})
export const rpcResponseSchema = z.union([
  z.object({ jsonrpc: z.literal('2.0'), id: z.number(), result: z.unknown() }).strict(),
  z.object({ jsonrpc: z.literal('2.0'), id: z.number(), error: rpcErrorSchema }).strict(),
])
export const rpcNotificationSchema = z.object({
  jsonrpc: z.literal('2.0'),
  method: z.string(),
  params: z.unknown().optional(),
}).strict()

export const capabilitiesSchema = z.object({
  serverVersion: z.string(),
  methods: z.array(z.string()),
  topics: z.array(z.string()),
  permissions: z.array(z.string()),
  features: z.record(z.string(), z.string()),
})
export type Capabilities = z.infer<typeof capabilitiesSchema>

export class RpcCallError extends Error {
  readonly code: string

  constructor(code: string, message: string) {
    super(message)
    this.name = 'RpcCallError'
    this.code = code
  }
}

export class RpcProtocolError extends Error {
  constructor(message: string) {
    super(message)
    this.name = 'RpcProtocolError'
  }
}
