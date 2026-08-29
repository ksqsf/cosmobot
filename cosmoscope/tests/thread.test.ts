import { describe, expect, it } from 'vitest'
import { threadMessageKeyId, threadPathTo } from '@/backend/thread'
import type { ThreadMessageKey, ThreadNode } from '@/types/domain'

const key = (messageId: string): ThreadMessageKey => ({ platform: 'PlatformRPC', chatId: null, messageId })
const node = (messageId: string, parentMessageId: string | null): ThreadNode => ({
  messageKey: key(messageId),
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
})
