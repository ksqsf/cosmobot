import { Effect } from 'effect'
import { ZodError } from 'zod'
import { RpcBackendError, type AdminBackend } from './AdminBackend'
import {
  activeThreadListSchema,
  auditDetailSchema,
  auditCountSchema,
  auditRecordSchema,
  auditThreadSchema,
  chatDeleteSchema,
  chatHistorySchema,
  chatLogListSchema,
  chatLogWindowSchema,
  chatMessageDoneSchema,
  chatMessageSchema,
  chatOpenSchema,
  chatRenameSchema,
  chatSendSchema,
  chatSessionsSchema,
  chatUploadSchema,
  concurrencyCancelSchema,
  concurrencyListSchema,
  concurrencyLookupSchema,
  haltThreadSchema,
  mediaDeleteSchema,
  mediaDetailSchema,
  mediaGcSchema,
  mediaSearchSchema,
  mediaSnapshotSchema,
  memoryDetailSchema,
  memoryHistorySchema,
  memoryListSchema,
  memoryRevertSchema,
  pluginLifecycleSchema,
  pluginListSchema,
  pluginUnloadSchema,
  recentAuditSchema,
  resourceDestroyAssociatedSchema,
  resourceDestroySchema,
  resourceDetailSchema,
  resourceKeepAliveSchema,
  resourceListAssociatedSchema,
  resourceListSchema,
  resourceMakePermanentSchema,
  resourceRenameSchema,
  skillDetailSchema,
  skillListSchema,
  skillRemoveSchema,
  threadDetailSchema,
  threadListSchema,
  threadRunTargetSchema,
} from '@/rpc/schemas'
import type { RpcClient } from '@/rpc/client'
import { RpcCallError, type LiveAdminMethod } from '@/rpc/protocol'
import type { BackendEffect } from './AdminBackend'
import type { AuditPlatform } from '@/types/domain'

export function makeRpcBackend(client: RpcClient, methods: ReadonlySet<string>): AdminBackend {
  const supports = (method: LiveAdminMethod): boolean => methods.has(method)
  return {
    tasks: {
      list: supports('concurrency.list') ? () => rpcEffect(
        'Could not load the task snapshot.',
        async () => concurrencyListSchema.parse(await client.request('concurrency.list')).entries,
      ) : unsupported('concurrency.list'),
      lookup: (id) => rpcEffect('Could not load the task.', async () => concurrencyLookupSchema.parse(await client.request('concurrency.lookup', { id })).entry),
      cancel: (id) => rpcEffect('Could not cancel the task.', async () => concurrencyCancelSchema.parse(await client.request('concurrency.cancel', { id })).cancelled),
      associated: (id) => rpcEffect('Could not load associated resources.', async () => resourceListAssociatedSchema.parse(await client.request('resource.list_associated', { id })).resources),
      destroyAssociated: (id) => rpcEffect('Could not destroy associated resources.', async () => resourceDestroyAssociatedSchema.parse(await client.request('resource.destroy_associated', { id })).results),
    },
    audit: {
      recent: supports('audit.recent') ? (limit = 20) => rpcEffect(
        'Could not load recent audit activity.',
        async () => recentAuditSchema.parse(await client.request('audit.recent', { limit })),
      ) : unsupported('audit.recent'),
      count: supports('audit.count') ? () => rpcEffect(
        'Could not count audit activity.',
        async () => auditCountSchema.parse(await client.request('audit.count')),
      ) : unsupported('audit.count'),
      search: (query, limit = 500) => rpcEffect(
        'Could not search audit activity.',
        async () => recentAuditSchema.parse(await client.request('audit.search', { query, limit })),
      ),
      get: supports('audit.get') ? (id) => rpcEffect(
        'Could not load the audit event.',
        async () => auditDetailSchema.parse(await client.request('audit.get', { id })),
      ) : unsupported('audit.get'),
      run: (runId) => rpcEffect(
        'Could not load the agent run audit.',
        async () => auditThreadSchema.parse(await client.request('audit.run', { runId })),
      ),
      thread: supports('audit.thread') ? (key) => rpcEffect(
        'Could not load the related audit thread.',
        async () => auditThreadSchema.parse(await client.request('audit.thread', {
          platform: auditPlatformKey(key.platform),
          chat_id: key.chatId,
          message_id: key.messageId,
        })),
      ) : unsupported('audit.thread'),
      threadMessages: (keys) => {
        const first = keys[0]
        if (first === undefined) return Effect.succeed([])
        const messageIds = keys
          .filter(({ platform, chatId }) => platform === first.platform && chatId === first.chatId)
          .map(({ messageId }) => messageId)
        return rpcEffect(
          'Could not load thread statistics.',
          async () => auditThreadSchema.parse(await client.request('audit.thread_messages', {
            platform: auditPlatformKey(first.platform),
            chat_id: first.chatId,
            message_ids: messageIds,
          })),
        )
      },
      subscribe: supports('audit.subscribe') ? (refresh, handler) => Effect.sync(() => client.subscribe(
        'overview.audit', 'audit.subscribe', 'audit.unsubscribe', {}, ['audit.event'], refresh,
        (_method, params) => handler(auditRecordSchema.parse(params)),
      )) : unsupported('audit.subscribe'),
    },
    threads: {
      list: (query) => rpcEffect('Could not load threads.', async () => threadListSchema.parse(await client.request('thread.list', {
        offset: query.offset,
        limit: query.limit,
        ...(query.query === undefined ? {} : { query: query.query }),
        ...(query.platform === undefined ? {} : { platform: auditPlatformKey(query.platform) }),
      }))),
      get: (id) => rpcEffect('Could not load the thread.', async () => threadDetailSchema.nullable().parse(await client.request('thread.get', { threadId: id }))),
      resolveRun: (runId) => rpcEffect('Could not find the agent thread.', async () => threadRunTargetSchema.parse(await client.request('thread.resolve_run', { runId }))),
      active: () => rpcEffect('Could not load active threads.', async () => activeThreadListSchema.parse(await client.request('thread.active')).threads),
      halt: (taskId) => rpcEffect('Could not halt the active thread.', async () => haltThreadSchema.parse(await client.request('thread.halt', { taskId })).halted),
    },
    memory: {
      list: () => rpcEffect('Could not load memories.', async () => memoryListSchema.parse(await client.request('memory.list')).memories),
      get: (key) => rpcEffect('Could not load memory.', async () => memoryDetailSchema.nullable().parse(await client.request('memory.get', key))),
      history: (key) => rpcEffect('Could not load memory history.', async () => memoryHistorySchema.parse(await client.request('memory.history', key)).history),
      getRevision: (key, revision) => rpcEffect('Could not load memory revision.', async () => memoryDetailSchema.nullable().parse(await client.request('memory.get_revision', { ...key, revision }))),
      revert: (key, revision) => rpcEffect('Could not revert memory.', async () => memoryRevertSchema.parse(await client.request('memory.revert', { ...key, revision })).memory),
    },
    skills: {
      list: () => rpcEffect('Could not load skills.', async () => skillListSchema.parse(await client.request('skills.list')).skills),
      get: (name) => rpcEffect('Could not load skill.', async () => skillDetailSchema.nullable().parse(await client.request('skills.get', { name }))),
      remove: (name) => rpcEffect('Could not remove skill.', async () => skillRemoveSchema.parse(await client.request('skills.remove', { name })).removed),
    },
    chat: {
      sessionCount: supports('chat.list_sessions') ? () => Effect.tryPromise({
        try: async () => chatSessionsSchema.parse(await client.request('chat.list_sessions')).sessions.length,
        catch: () => new RpcBackendError({ message: 'Could not load the session count.' }),
      }) : unsupported('chat.list_sessions'),
      list: () => rpcEffect('Could not load chat sessions.', async () => chatSessionsSchema.parse(await client.request('chat.list_sessions')).sessions),
      open: (label) => rpcEffect('Could not create the chat session.', async () => chatOpenSchema.parse(await client.request('chat.open_session', { label })).session),
      history: (sessionId, beforeMessageId, limit = 100) => rpcEffect('Could not load the chat transcript.', async () => chatHistorySchema.parse(await client.request('chat.history', { sessionId, beforeMessageId, limit }))),
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
    chatLogs: {
      list: () => rpcEffect('Could not load chat logs.', async () => chatLogListSchema.parse(await client.request('chat_log.list')).chats),
      window: (query) => rpcEffect('Could not load chat messages.', async () => chatLogWindowSchema.parse(await client.request('chat_log.window', query))),
    },
    resources: {
      count: supports('resource.list') ? () => Effect.tryPromise({
        try: async () => resourceListSchema.parse(await client.request('resource.list')).resources.length,
        catch: () => new RpcBackendError({ message: 'Could not load the resource count.' }),
      }) : unsupported('resource.list'),
      list: () => rpcEffect('Could not load resources.', async () => resourceListSchema.parse(await client.request('resource.list')).resources),
      detail: (id) => rpcEffect('Could not load the resource detail.', async () => resourceDetailSchema.parse(await client.request('resource.detail', { id })).detail),
      destroy: (id) => rpcEffect('Could not destroy the resource.', async () => { resourceDestroySchema.parse(await client.request('resource.destroy', { id })) }),
      rename: (id, newId) => rpcEffect('Could not rename the resource.', async () => resourceRenameSchema.parse(await client.request('resource.rename', { id, newId })).id),
      keepAlive: (id) => rpcEffect('Could not refresh the resource lifetime.', async () => { resourceKeepAliveSchema.parse(await client.request('resource.keep_alive', { id })) }),
      makePermanent: (id) => rpcEffect('Could not make the resource permanent.', async () => { resourceMakePermanentSchema.parse(await client.request('resource.make_permanent', { id })) }),
    },
    media: {
      list: (limit = 200) => rpcEffect('Could not load media.', async () => mediaSnapshotSchema.parse(await client.request('media.stats', { limit }))),
      search: (search) => rpcEffect('Could not search media.', async () => mediaSearchSchema.parse(await client.request('media.search', search)).files),
      get: (id) => rpcEffect('Could not load media details.', async () => mediaDetailSchema.parse(await client.request('media.get', { mediaId: id }))),
      delete: (id) => rpcEffect('Could not delete media.', async () => mediaDeleteSchema.parse(await client.request('media.delete', { mediaId: id })).deleted),
      gc: (maxAgeSeconds) => rpcEffect('Could not garbage collect media.', async () => mediaGcSchema.parse(await client.request('media.gc', maxAgeSeconds === undefined ? {} : { maxAgeSeconds }))),
    },
    plugins: {
      list: () => rpcEffect('Could not load plugins.', async () => pluginListSchema.parse(await client.request('plugin.list')).plugins),
      load: (id) => rpcEffect('Could not load the plugin.', async () => pluginLifecycleSchema.parse(await client.request('plugin.load', { pluginId: id }))),
      reload: (id) => rpcEffect('Could not reload the plugin.', async () => pluginLifecycleSchema.parse(await client.request('plugin.reload', { pluginId: id }))),
      unload: (id) => rpcEffect('Could not unload the plugin.', async () => { pluginUnloadSchema.parse(await client.request('plugin.unload', { pluginId: id })) }),
    },
  }
}

type RpcAuditPlatform = 'qq' | 'telegram' | 'matrix' | 'discord' | 'rpc' | 'acp'

function auditPlatformKey(platform: AuditPlatform): RpcAuditPlatform {
  return ({
    PlatformQQ: 'qq',
    PlatformTelegram: 'telegram',
    PlatformMatrix: 'matrix',
    PlatformDiscord: 'discord',
    PlatformRPC: 'rpc',
    PlatformACP: 'acp',
  } satisfies Record<AuditPlatform, RpcAuditPlatform>)[platform]
}

function rpcEffect<A>(fallback: string, action: () => Promise<A>): BackendEffect<A> {
  return Effect.tryPromise({
    try: action,
    catch: (cause) => new RpcBackendError({ message: rpcErrorMessage(cause, fallback) }),
  })
}

function unsupported<A>(method: LiveAdminMethod): () => BackendEffect<A> {
  return () => Effect.fail(new RpcBackendError({ message: `The server does not support ${method}.` }))
}

function rpcErrorMessage(cause: unknown, fallback: string): string {
  const issue = cause instanceof ZodError ? cause.issues[0] : undefined
  if (issue !== undefined) return `Invalid RPC payload at ${issue.path.join('.') || 'result'} (${issue.code}).`
  return cause instanceof RpcCallError ? cause.message : fallback
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
