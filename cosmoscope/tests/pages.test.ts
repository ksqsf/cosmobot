import { describe, expect, it } from 'vitest'
import { pages } from '@/app/pages'

describe('page registry', () => {
  it('has unique names and paths with complete navigation metadata', () => {
    expect(new Set(pages.map(({ name }) => name)).size).toBe(pages.length)
    expect(new Set(pages.map(({ path }) => path)).size).toBe(pages.length)
    for (const page of pages) {
      expect(page.path).toMatch(/^\//)
      expect(page.title).not.toBe('')
      expect(page.icon).toMatch(/^pi pi-/)
      expect(['workspace', 'operations']).toContain(page.navigationGroup)
    }
  })
})
