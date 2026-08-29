import MarkdownIt from 'markdown-it'
import { katex } from '@mdit/plugin-katex'
import hljs from 'highlight.js/lib/common'

const mediaReferencePattern = /media:mf_[A-Za-z0-9_-]{7,}|(?<=\bmedia_id=)mf_[A-Za-z0-9_-]{7,}/g

function mediaReferenceMatches(source: string): { index: number; label: string; ref: string }[] {
  return [...source.matchAll(mediaReferencePattern)].map((match) => ({
    index: match.index,
    label: match[0],
    ref: match[0].startsWith('media:') ? match[0] : `media:${match[0]}`,
  }))
}

const markdown = new MarkdownIt('commonmark', {
  html: false,
  linkify: true,
  highlight: highlightCode,
}).use(katex, {
  delimiters: 'all',
  throwOnError: false,
  strict: 'ignore',
}).enable('table')

markdown.core.ruler.after('inline', 'media_refs', (state) => {
  for (const block of state.tokens) {
    if (block.type !== 'inline' || block.children === null) continue
    let linkDepth = 0
    block.children = block.children.flatMap((token) => {
      if (token.type === 'link_open') { linkDepth += 1; return [token] }
      if (token.type === 'link_close') { linkDepth -= 1; return [token] }
      if (token.type !== 'text' || linkDepth > 0) return [token]
      const replacement = []
      let offset = 0
      for (const match of mediaReferenceMatches(token.content)) {
        const index = match.index
        if (index > 0 && /[A-Za-z0-9_]/.test(token.content[index - 1] ?? '')) continue
        if (index > offset) {
          const text = new state.Token('text', '', 0)
          text.content = token.content.slice(offset, index)
          replacement.push(text)
        }
        const { label: refLabel, ref } = match
        const link = new state.Token('link_open', 'a', 1)
        link.attrSet('href', `/media/${encodeURIComponent(ref)}`)
        link.attrSet('data-media-ref', ref)
        const label = new state.Token('code_inline', 'code', 0)
        label.content = refLabel
        replacement.push(link, label, new state.Token('link_close', 'a', -1))
        offset = index + refLabel.length
      }
      if (offset === 0) return [token]
      if (offset < token.content.length) {
        const text = new state.Token('text', '', 0)
        text.content = token.content.slice(offset)
        replacement.push(text)
      }
      return replacement
    })
  }
})

const defaultLinkOpen = markdown.renderer.rules['link_open']
markdown.renderer.rules['link_open'] = (tokens, index, options, env, self) => {
  const token = tokens[index]
  const href = token?.attrGet('href')
  if (typeof href !== 'string' || !href.startsWith('/')) {
    token?.attrSet('target', '_blank')
    token?.attrSet('rel', 'noopener noreferrer')
  }
  return defaultLinkOpen?.(tokens, index, options, env, self) ?? self.renderToken(tokens, index, options)
}

export function renderMarkdown(source: string): string {
  return markdown.render(withoutFrontMatter(source))
}

export function mediaRefFromClick(event: MouseEvent): string | undefined {
  const target = event.target instanceof Element ? event.target.closest<HTMLAnchorElement>('a[data-media-ref]') : null
  return target?.dataset['mediaRef']
}

export function mediaRefsInText(source: string): string[] {
  return [...new Set(mediaReferenceMatches(source).map(({ ref }) => ref))]
}

export function withoutFrontMatter(source: string): string {
  if (!source.startsWith('---\n') && !source.startsWith('---\r\n')) return source
  const lines = source.split(/\r?\n/)
  const end = lines.indexOf('---', 1)
  if (end < 2 || !lines.slice(1, end).some((line) => /^[A-Za-z][\w-]*\s*:/.test(line))) return source
  return lines.slice(end + 1).join('\n').replace(/^\n/, '')
}

export function highlightCode(source: string, language = ''): string {
  return language !== '' && hljs.getLanguage(language) !== undefined
    ? hljs.highlight(source, { language, ignoreIllegals: true }).value
    : hljs.highlightAuto(source).value
}
