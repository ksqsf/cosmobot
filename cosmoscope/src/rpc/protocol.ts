import { z } from 'zod'

export const rpcIdSchema = z.union([z.number(), z.string(), z.null()])
export const rpcErrorSchema = z.object({
  code: z.number(),
  message: z.string(),
  data: z.object({ code: z.string() }).loose().optional(),
})
export const rpcResponseSchema = z.union([
  z.object({ jsonrpc: z.literal('2.0'), id: rpcIdSchema, result: z.unknown() }),
  z.object({ jsonrpc: z.literal('2.0'), id: rpcIdSchema, error: rpcErrorSchema }),
])
export const rpcNotificationSchema = z.object({
  jsonrpc: z.literal('2.0'),
  method: z.string(),
  params: z.unknown().optional(),
})

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
