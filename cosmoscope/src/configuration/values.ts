import type { ConfigOption } from '@/rpc/schemas'

export function displayConfigValue(value: unknown): string {
  if (value === null || value === undefined) return 'unset'
  if (Array.isArray(value)) return value.map(displayListItem).join(', ')
  if (typeof value === 'string') return value
  if (typeof value === 'number' || typeof value === 'boolean' || typeof value === 'bigint') return String(value)
  if (typeof value === 'object') return JSON.stringify(value)
  return ''
}

function displayListItem(value: unknown): string {
  if (typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean') return String(value)
  return typeof value === 'object' && value !== null ? JSON.stringify(value) : ''
}

export function configTextInputValue(value: unknown, kind: ConfigOption['type']['kind']): string {
  if (kind === 'secret' || value === null || value === undefined) return ''
  return typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean' ? String(value) : ''
}

export function configListItemKind(option: ConfigOption): 'text' | 'integer' | 'identity' {
  if (option.type.kind === 'identity_list') return 'identity'
  return option.type.kind === 'list' && option.type.values[0] === 'integer' ? 'integer' : 'text'
}
