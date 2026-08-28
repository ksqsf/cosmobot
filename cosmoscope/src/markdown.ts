import MarkdownIt from 'markdown-it'
import { katex } from '@mdit/plugin-katex'

const markdown = new MarkdownIt('commonmark', {
  html: false,
  linkify: true,
}).use(katex, {
  throwOnError: false,
  strict: 'ignore',
})

const defaultLinkOpen = markdown.renderer.rules['link_open']
markdown.renderer.rules['link_open'] = (tokens, index, options, env, self) => {
  tokens[index]?.attrSet('target', '_blank')
  tokens[index]?.attrSet('rel', 'noopener noreferrer')
  return defaultLinkOpen?.(tokens, index, options, env, self) ?? self.renderToken(tokens, index, options)
}

export function renderMarkdown(source: string): string {
  return markdown.render(source)
}
