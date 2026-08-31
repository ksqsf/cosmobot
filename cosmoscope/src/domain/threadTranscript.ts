import { mediaRefsInText } from '@/markdown'
import type { StoredThreadMessage, ThreadNode } from '@/types/domain'

export interface ThreadTranscriptEntry {
  readonly node: ThreadNode
  readonly message: StoredThreadMessage
  readonly messageIndex: number
}

export function transcriptEntries(path: readonly ThreadNode[]): ThreadTranscriptEntry[] {
  return path.flatMap((node) => node.messages.map((message, messageIndex) => ({ node, message, messageIndex })))
}

export function messageText(message: StoredThreadMessage): string {
  const { content } = message
  if (typeof content === 'string') return content
  if (content === undefined || content === null) return message.tool_calls?.map((call) => `Calls ${call.function.name}`).join('\n') ?? '(No text content)'
  return content.map((part) => {
    if (part.text !== undefined) return part.text
    const image = typeof part.image_url === 'string' ? part.image_url : part.image_url?.url
    return image === undefined ? `[${part.type}]` : `[Image] ${image}`
  }).join('\n')
}

export function readableMessageText(message: StoredThreadMessage): string {
  return messageText(message).replace(/\s+/g, ' ').trim()
}

export function mediaRefs(message: StoredThreadMessage): string[] {
  const text = [messageText(message), ...(message.tool_calls?.map((call) => call.function.arguments) ?? [])].join('\n')
  const contentRefs = Array.isArray(message.content)
    ? message.content.flatMap((part) => {
        const image = typeof part.image_url === 'string' ? part.image_url : part.image_url?.url
        return image?.startsWith('media:mf_') === true ? [image] : []
      })
    : []
  return [...new Set([...contentRefs, ...mediaRefsInText(text)])]
}

export function roleLabel(role: string): string {
  return ({ user: 'User', assistant: 'Assistant', system: 'System', tool: 'Tool', synthetic: 'Synthetic' } as Readonly<Record<string, string>>)[role] ?? role
}

export function formatDuration(milliseconds: number, unreported = 0): string {
  const value = milliseconds < 1000 ? `${String(milliseconds)} ms` : `${(milliseconds / 1000).toFixed(1)} s`
  return unreported === 0 ? value : `${value} · ${String(unreported)} unreported`
}
