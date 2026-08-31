import { threadMessageKeyId } from '@/backend/thread'
import { readableMessageText } from '@/domain/threadTranscript'
import type { ActiveThread, ThreadNode } from '@/types/domain'

export interface ThreadTreeNode {
  readonly key: string
  readonly label: string
  readonly icon: string
  readonly styleClass?: string
  children: ThreadTreeNode[]
}

export function activeTreeKey(taskId: number): string {
  return `active:${String(taskId)}`
}

export function buildTree(nodes: readonly ThreadNode[]): ThreadTreeNode[] {
  const byKey = new Map(nodes.map((node) => [threadMessageKeyId(node.messageKey), {
    key: threadMessageKeyId(node.messageKey),
    label: nodeLabel(node),
    icon: node.parentMessageKey === null ? 'pi pi-comments' : 'pi pi-reply',
    children: [] as ThreadTreeNode[],
  } satisfies ThreadTreeNode]))
  const roots: ThreadTreeNode[] = []
  for (const node of nodes) {
    const item = byKey.get(threadMessageKeyId(node.messageKey))
    if (item === undefined) continue
    const parent = node.parentMessageKey === null ? undefined : byKey.get(threadMessageKeyId(node.parentMessageKey))
    if (parent === undefined) roots.push(item)
    else parent.children.push(item)
  }
  return roots.length === 0 ? [...byKey.values()] : roots
}

export function buildActiveTree(nodes: readonly ThreadNode[], activeThreads: readonly ActiveThread[]): ThreadTreeNode[] {
  const roots = buildTree(nodes)
  for (const active of activeThreads) {
    const running: ThreadTreeNode = {
      key: activeTreeKey(active.taskId),
      label: active.prompt || 'Active thread',
      icon: 'pi pi-spinner pi-spin',
      styleClass: 'thread-tree-active',
      children: [],
    }
    const parentKey = active.parentMessageKey === null ? undefined : threadMessageKeyId(active.parentMessageKey)
    const parent = parentKey === undefined ? undefined : findTreeNode(roots, parentKey)
    if (parent === undefined) roots.push(running)
    else parent.children.push(running)
  }
  return roots
}

export function findTreeNode(nodes: readonly ThreadTreeNode[], key: string): ThreadTreeNode | undefined {
  for (const node of nodes) {
    if (node.key === key) return node
    const found = findTreeNode(node.children, key)
    if (found !== undefined) return found
  }
  return undefined
}

function nodeLabel(node: ThreadNode): string {
  const visibleMessages = node.messages.filter(({ role }) => role !== 'synthetic')
  const preferred = [...visibleMessages].reverse().find((message) => message.role === 'user' && readableMessageText(message) !== '')
    ?? [...visibleMessages].reverse().find((message) => readableMessageText(message) !== '')
  if (preferred === undefined) return node.messageKey.messageId
  const text = readableMessageText(preferred)
  return text.length > 64 ? `${text.slice(0, 61)}…` : text
}
