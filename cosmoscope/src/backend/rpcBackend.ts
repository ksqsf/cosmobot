import { Effect } from 'effect'
import { RpcBackendError, type AdminBackend } from './AdminBackend'
import { mockBackend } from './mockBackend'
import { auditRecordSchema, chatDeleteSchema, chatHistorySchema, chatMessageDoneSchema, chatMessageSchema, chatOpenSchema, chatRenameSchema, chatSendSchema, chatSessionsSchema, chatUploadSchema, concurrencyListSchema, mediaDeleteSchema, recentAuditSchema, resourceListSchema } from '@/rpc/schemas'
import type { RpcClient } from '@/rpc/client'
import type { LiveAdminMethod } from '@/rpc/protocol'
import type { BackendEffect } from './AdminBackend'
import type { Task } from '@/types/domain'

export function makeRpcBackend(client: RpcClient, methods: ReadonlySet<string>): AdminBackend {
  const supports = (method: LiveAdminMethod): boolean => methods.has(method)
  return {
    system: mockBackend.system,
    tasks: {
      list: supports('concurrency.list') ? () => Effect.tryPromise({
        try: async () => concurrencyListSchema.parse(await client.request('concurrency.list')).entries.map(toTask),
        catch: () => new RpcBackendError({ message: 'Could not load the task snapshot.' }),
      }) : mockBackend.tasks.list,
    },
    audit: {
      recent: supports('audit.recent') ? () => Effect.tryPromise({
        try: async () => recentAuditSchema.parse(await client.request('audit.recent', { limit: 20 })),
        catch: () => new RpcBackendError({ message: 'Could not load recent audit activity.' }),
      }) : mockBackend.audit.recent,
      subscribe: supports('audit.subscribe') ? (refresh, handler) => Effect.sync(() => client.subscribe(
        'overview.audit', 'audit.subscribe', 'audit.unsubscribe', {}, ['audit.event'], refresh,
        (_method, params) => handler(auditRecordSchema.parse(params)),
      )) : mockBackend.audit.subscribe,
    },
    chat: {
      sessionCount: supports('chat.list_sessions') ? () => Effect.tryPromise({
        try: async () => chatSessionsSchema.parse(await client.request('chat.list_sessions')).sessions.length,
        catch: () => new RpcBackendError({ message: 'Could not load the session count.' }),
      }) : mockBackend.chat.sessionCount,
      list: () => rpcEffect('Could not load chat sessions.', async () => chatSessionsSchema.parse(await client.request('chat.list_sessions')).sessions),
      open: (label) => rpcEffect('Could not create the chat session.', async () => chatOpenSchema.parse(await client.request('chat.open_session', { label })).session),
      history: (sessionId) => rpcEffect('Could not load the chat transcript.', async () => chatHistorySchema.parse(await client.request('chat.history', { sessionId })).messages),
      fork: (sessionId, messageId, label) => rpcEffect('Could not fork the chat session.', async () => chatOpenSchema.parse(await client.request('chat.fork', { sessionId, messageId, label })).session),
      rename: (sessionId, label) => rpcEffect('Could not rename the chat session.', async () => chatRenameSchema.parse(await client.request('chat.rename_session', { sessionId, label })).session),
      delete: (sessionId) => rpcEffect('Could not delete the chat session.', async () => chatDeleteSchema.parse(await client.request('chat.delete_session', { sessionId })).deleted),
      upload: (file) => rpcEffect('Could not upload the attachment.', async () => chatUploadSchema.parse(await client.request('chat.upload_attachment', {
        name: file.name,
        mediaType: file.type || 'application/octet-stream',
        kind: file.type.startsWith('image/') ? 'image' : file.type.startsWith('audio/') ? 'audio' : 'file',
        size: file.size,
        data: await fileBase64(file),
      }))),
      discardAttachment: (attachmentId) => rpcEffect('Could not discard the attachment.', async () => mediaDeleteSchema.parse(await client.request('media.delete', { mediaId: attachmentId })).deleted),
      send: (message) => rpcEffect('Could not send the message.', async () => chatSendSchema.parse(await client.request('chat.send', message)).messageId),
      subscribe: (sessionId, refresh, handler, done) => Effect.sync(() => client.subscribe(
        `chat:${sessionId}`, 'chat.subscribe', 'chat.unsubscribe', { sessionId },
        ['chat.message', 'chat.message_update', 'chat.message_done'], refresh,
        (method, params) => {
          switch (method) {
            case 'chat.message':
            case 'chat.message_update': handler(chatMessageSchema.parse(params)); break
            case 'chat.message_done': done(chatMessageDoneSchema.parse(params).messageId); break
          }
        },
      )),
    },
    resources: {
      count: supports('resource.list') ? () => Effect.tryPromise({
        try: async () => resourceListSchema.parse(await client.request('resource.list')).resources.length,
        catch: () => new RpcBackendError({ message: 'Could not load the resource count.' }),
      }) : mockBackend.resources.count,
    },
    plugins: mockBackend.plugins,
    logs: mockBackend.logs,
  }
}

function rpcEffect<A>(fallback: string, action: () => Promise<A>): BackendEffect<A> {
  return Effect.tryPromise({
    try: action,
    catch: (cause) => new RpcBackendError({ message: cause instanceof Error ? cause.message : fallback }),
  })
}

function fileBase64(file: File): Promise<string> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onerror = () => reject(reader.error ?? new Error('Could not read the attachment.'))
    reader.onload = () => {
      if (typeof reader.result !== 'string') { reject(new Error('Could not read the attachment.')); return }
      resolve(reader.result.slice(reader.result.indexOf(',') + 1))
    }
    reader.readAsDataURL(file)
  })
}

function toTask(entry: ReturnType<typeof concurrencyListSchema.parse>['entries'][number]): Task {
  const elapsedUntil = entry.finishedAt === null ? Date.now() : Date.parse(entry.finishedAt)
  const elapsedSeconds = Math.max(0, Math.round((elapsedUntil - Date.parse(entry.startedAt)) / 1_000))
  return {
    id: String(entry.id),
    label: entry.label,
    detail: entry.error ?? 'Managed cosmobot task (owner metadata unavailable)',
    owner: 'Unavailable',
    platform: 'runtime',
    status: entry.status === 'cancelled' ? 'stopped' : entry.status,
    started: new Date(entry.startedAt).toLocaleTimeString(),
    elapsed: `${String(elapsedSeconds)}s`,
  }
}
