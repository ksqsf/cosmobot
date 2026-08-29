import type { ThreadMessageKey, ThreadNode } from '@/types/domain'

export function threadMessageKeyId(key: ThreadMessageKey): string {
  return `${key.platform}\u0000${key.chatId ?? ''}\u0000${key.messageId}`
}

export function threadPathTo(node: ThreadNode, nodes: ReadonlyMap<string, ThreadNode>): ThreadNode[] {
  const path: ThreadNode[] = []
  const visited = new Set<string>()
  let current: ThreadNode | undefined = node
  while (current !== undefined && !visited.has(threadMessageKeyId(current.messageKey))) {
    visited.add(threadMessageKeyId(current.messageKey))
    path.unshift(current)
    current = current.parentMessageKey === null ? undefined : nodes.get(threadMessageKeyId(current.parentMessageKey))
  }
  return path
}

export function threadMessageChatKey(node: ThreadNode, messageIndex: number): ThreadMessageKey | undefined {
  const message = node.messages[messageIndex]
  if (message?.role === 'user') {
    return messageIndex === node.messages.findIndex(({ role }) => role === 'user') ? node.inputMessageKey ?? undefined : undefined
  }
  if (message?.role !== 'assistant' || message.tool_calls?.length) return undefined
  const response = [...node.messages].reverse().find((candidate) =>
    candidate.role === 'assistant' && !candidate.tool_calls?.length && messageText(candidate.content) !== '',
  )
  return message === response ? node.messageKey : undefined
}

function messageText(content: ThreadNode['messages'][number]['content']): string {
  if (typeof content === 'string') return content.trim()
  if (!Array.isArray(content)) return ''
  return content.flatMap((part) => part.text ?? []).join(' ').trim()
}
