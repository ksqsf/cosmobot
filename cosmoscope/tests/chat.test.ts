import { describe, expect, it } from 'vitest'
import { mergeChatMessage, safeDownloadUrl, safeImageUrl } from '@/backend/chat'
import { highlightCode, renderMarkdown } from '@/markdown'
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
})
