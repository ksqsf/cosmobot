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
