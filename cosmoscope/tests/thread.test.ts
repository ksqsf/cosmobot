import { describe, expect, it } from 'vitest'
import { threadMessageChatKey, threadMessageKeyId, threadPathTo, threadToolActivity } from '@/backend/thread'
import type { AuditRecord, ThreadMessageKey, ThreadNode } from '@/types/domain'

const key = (messageId: string): ThreadMessageKey => ({ platform: 'PlatformRPC', chatId: null, messageId })
const node = (messageId: string, parentMessageId: string | null): ThreadNode => ({
  messageKey: key(messageId),
  inputMessageKey: null,
  parentMessageKey: parentMessageId === null ? null : key(parentMessageId),
  messages: [],
})

describe('threadPathTo', () => {
  it('returns only the selected branch from the root', () => {
    const leaf = node('leaf', 'left')
    const nodes = [node('root', null), node('left', 'root'), node('right', 'root'), leaf]
    const lookup = new Map(nodes.map((entry) => [threadMessageKeyId(entry.messageKey), entry]))

    expect(threadPathTo(leaf, lookup).map(({ messageKey }) => messageKey.messageId)).toEqual(['root', 'left', 'leaf'])
  })

  it('links only real user and final assistant chat messages', () => {
    const inputMessageKey = key('input')
    const outputMessageKey = key('output')
    const thread: ThreadNode = {
      messageKey: outputMessageKey,
      inputMessageKey,
      parentMessageKey: null,
      messages: [
        { role: 'user', content: 'prompt' },
        { role: 'assistant', tool_calls: [{ id: 'call-1', type: 'function', function: { name: 'image_generate', arguments: '{}' } }] },
        { role: 'tool', content: '{"result":"ok"}' },
        { role: 'synthetic', content: [{ type: 'image_url', image_url: 'media:mf_image' }] },
        { role: 'assistant', content: 'done' },
      ],
    }

    expect(thread.messages.map((_, index) => threadMessageChatKey(thread, index))).toEqual([
      inputMessageKey, undefined, undefined, undefined, outputMessageKey,
    ])
  })

  it('projects running and completed tool calls from live audit events', () => {
    const records: AuditRecord[] = [
      { id: 1, occurredAt: '', event: { tag: 'ToolCallStarted', runId: 'run', turn: 1, toolCall: { id: 'call-1', name: 'search', arguments: '{"q":"test"}' } } },
      { id: 2, occurredAt: '', event: { tag: 'ToolCallStarted', runId: 'run', turn: 1, toolCall: { id: 'call-2', name: 'fetch', arguments: '{}' } } },
      { id: 3, occurredAt: '', event: { tag: 'ToolCallFinished', runId: 'run', turn: 1, toolCallId: 'call-1', toolName: 'search', status: 'ok', result: 'found', resultLength: 5, messageIds: [] } },
    ]

    expect(threadToolActivity(records)).toEqual([
      { id: 'call-1', name: 'search', arguments: '{"q":"test"}', turn: 1, status: 'ok', result: 'found' },
      { id: 'call-2', name: 'fetch', arguments: '{}', turn: 1, status: 'running' },
    ])
  })
})
