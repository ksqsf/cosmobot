import type { ChatLogItem, ChatMessage } from '@/types/domain'

export interface BoundedChatLogItems {
  readonly entries: readonly ChatLogItem[]
  readonly pruned: boolean
}

export function mergeChatMessage(messages: readonly ChatMessage[], incoming: ChatMessage): ChatMessage[] {
  return messages.some(({ messageId }) => messageId === incoming.messageId)
    ? messages.map((message) => message.messageId === incoming.messageId ? incoming : message)
    : [...messages, incoming]
}

export function mergeChatLogItems(
  current: readonly ChatLogItem[],
  incoming: readonly ChatLogItem[],
  direction: 'older' | 'newer',
  limit: number,
): BoundedChatLogItems {
  const combined = [...new Map([...current, ...incoming].map((item) => [item.rowId, item])).values()]
    .sort((left, right) => left.rowId - right.rowId)
  return {
    entries: direction === 'older' ? combined.slice(0, limit) : combined.slice(-limit),
    pruned: combined.length > limit,
  }
}

export function safeImageUrl(value: string, baseUrl: string): string | undefined {
  if (/^data:image\/(?:png|jpeg|gif|webp);base64,/i.test(value)) return value
  return safeDownloadUrl(value, baseUrl)
}

export function safeDownloadUrl(value: string, baseUrl: string): string | undefined {
  try {
    const url = new URL(value, baseUrl)
    return url.protocol === 'http:' || url.protocol === 'https:' ? url.href : undefined
  } catch { return undefined }
}
