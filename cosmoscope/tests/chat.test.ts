import { Effect } from 'effect'
import { describe, expect, it, vi } from 'vitest'
import { makeRpcBackend } from '@/backend/rpcBackend'
import { mergeChatLogItems, mergeChatMessage, safeDownloadUrl, safeImageUrl } from '@/backend/chat'
import { highlightCode, mediaRefsInText, renderMarkdown } from '@/markdown'
import { RpcClient } from '@/rpc/client'
import { liveAdminMethods } from '@/rpc/protocol'
import type { ChatMessage } from '@/types/domain'

const message: ChatMessage = {
  sessionId: 'session-1',
  messageId: 'message-1',
  sender: 'assistant',
  text: 'draft',
  imageUrls: [],
  attachments: [],
  replyToMessageId: null,
  parentMessageId: null,
}

describe('chat projection', () => {
  it('requests bounded history pages with a stable cursor', async () => {
    const client = new RpcClient()
    const request = vi.spyOn(client, 'request').mockResolvedValue({ sessionId: 'session-1', messages: [message], hasOlder: true })
    const history = await Effect.runPromise(makeRpcBackend(client, new Set(liveAdminMethods)).chat.history('session-1', 'message-20', 50))

    expect(history).toEqual({ sessionId: 'session-1', messages: [message], hasOlder: true })
    expect(request).toHaveBeenCalledWith('chat.history', { sessionId: 'session-1', beforeMessageId: 'message-20', limit: 50 })
  })

  it('keeps platform chat logs as a bounded sliding window', () => {
    const item = (rowId: number) => ({ rowId, threadId: null, entry: { platform: 'PlatformRPC', kind: 'ChatPrivate', chatId: '1', recordedAt: null, senderId: null, senderUsername: null, senderDisplayName: null, messageId: String(rowId), replyToMessageId: null, isBot: false, mentions: [], mentionUsernames: [], imageUrls: [], files: [], text: '' } } as const)
    expect(mergeChatLogItems([item(3), item(4)], [item(1), item(2), item(3)], 'older', 3)).toEqual({ entries: [item(1), item(2), item(3)], pruned: true })
    expect(mergeChatLogItems([item(1), item(2)], [item(2), item(3), item(4)], 'newer', 3)).toEqual({ entries: [item(2), item(3), item(4)], pruned: true })
  })

  it('deduplicates complete streaming snapshots in place', () => {
    expect(mergeChatMessage([message], { ...message, text: 'new draft' })).toEqual([{ ...message, text: 'new draft' }])
  })

  it('allows browser-safe image and download URLs only', () => {
    const base = 'https://cosmoscope.example/chat'
    expect(safeImageUrl('/media/image.webp', base)).toBe('https://cosmoscope.example/media/image.webp')
    expect(safeImageUrl('data:image/png;base64,AA==', base)).toBe('data:image/png;base64,AA==')
    expect(safeImageUrl('data:image/svg+xml,<svg/>', base)).toBeUndefined()
    expect(safeDownloadUrl('javascript:alert(1)', base)).toBeUndefined()
    expect(safeDownloadUrl('https://files.example/report.pdf', base)).toBe('https://files.example/report.pdf')
  })

  it('renders CommonMark and KaTeX without enabling raw HTML', () => {
    const rendered = renderMarkdown('## Result\n\n$E = mc^2$\n\n\\[a^2+b^2=c^2\\]\n\n<script>alert(1)</script>')
    expect(rendered).toContain('<h2>Result</h2>')
    expect(rendered).toContain('class="katex"')
    expect(rendered).toContain('class="katex-display"')
    expect(rendered).not.toContain('[a^2+b^2=c^2]')
    expect(rendered).toContain('&lt;script&gt;')
    expect(rendered).not.toContain('<script>')
  })

  it('syntax-highlights code without allowing HTML through', () => {
    expect(renderMarkdown('```ts\nconst answer: number = 42\n```')).toContain('hljs-keyword')
    expect(highlightCode('{"value":"<script>"}', 'json')).toContain('&lt;script&gt;')
  })

  it('renders tables and treats frontmatter as metadata', () => {
    const rendered = renderMarkdown('---\nname: example\ndescription: Test\n---\n\n| A | B |\n| - | - |\n| 1 | 2 |')
    expect(rendered).toContain('<table>')
    expect(rendered).toContain('<td>1</td>')
    expect(rendered).not.toContain('description: Test')
  })

  it('links media references embedded in tool result text', () => {
    const ref = 'media:mf_5StthYV0IIB0-yoo1w5DRw'
    const rendered = renderMarkdown(`Generated image. Media ids: ${ref}`)
    expect(rendered).toContain(`href="/media/${encodeURIComponent(ref)}"`)
    expect(rendered).toContain(`data-media-ref="${ref}"`)
  })

  it('links compacted tool-result media ids using their canonical reference', () => {
    const id = 'mf_Q2JHMRvDjVlW_MceWO3S8g'
    const ref = `media:${id}`
    const source = `[tool result omitted; media_id=${id}, mime=image/png]`

    expect(renderMarkdown(source)).toContain(`data-media-ref="${ref}"`)
    expect(mediaRefsInText(source)).toEqual([ref])
  })
})
