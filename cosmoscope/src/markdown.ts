import MarkdownIt from 'markdown-it'
import { katex } from '@mdit/plugin-katex'
import hljs from 'highlight.js/lib/common'

const markdown = new MarkdownIt('commonmark', {
  html: false,
  linkify: true,
  highlight: highlightCode,
}).use(katex, {
  delimiters: 'all',
  throwOnError: false,
  strict: 'ignore',
}).enable('table')

markdown.inline.ruler.before('text', 'media_ref', (state, silent) => {
  if (state.pos > 0 && /[A-Za-z0-9_]/.test(state.src[state.pos - 1] ?? '')) return false
  const match = /^media:mf_[A-Za-z0-9_-]{7,}/.exec(state.src.slice(state.pos))
  if (match === null) return false
  const ref = match[0]
  if (!silent) {
    const link = state.push('link_open', 'a', 1)
    link.attrSet('href', `/media/${encodeURIComponent(ref)}`)
    link.attrSet('data-media-ref', ref)
    const label = state.push('code_inline', 'code', 0)
    label.content = ref
    state.push('link_close', 'a', -1)
  }
  state.pos += ref.length
  return true
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
