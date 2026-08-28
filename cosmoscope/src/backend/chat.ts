import type { ChatMessage } from '@/types/domain'

export function mergeChatMessage(messages: readonly ChatMessage[], incoming: ChatMessage): ChatMessage[] {
  return messages.some(({ messageId }) => messageId === incoming.messageId)
    ? messages.map((message) => message.messageId === incoming.messageId ? incoming : message)
    : [...messages, incoming]
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
