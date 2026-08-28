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
  'audit.recent',
  'audit.subscribe',
  'resource.list',
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
