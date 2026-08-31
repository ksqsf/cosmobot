import type { MediaItem, MediaSourceKind } from '@/types/domain'

const platformIcons: Readonly<Record<string, string>> = {
  telegram: 'pi pi-send', matrix: 'pi pi-th-large', qq: 'pi pi-comments', discord: 'pi pi-comments', rpc: 'pi pi-desktop', acp: 'pi pi-code',
}
const platformLabels: Readonly<Record<string, string>> = {
  matrix: 'Matrix', qq: 'QQ', telegram: 'Telegram', discord: 'Discord', rpc: 'RPC', acp: 'ACP',
}

export const sourceKindLabels: Readonly<Record<MediaSourceKind, string>> = { chat: 'Chat', 'generated-image': 'Generated image', 'tool-result': 'Tool result', sandbox: 'Sandbox file' }
export const sourceKindIcons: Readonly<Record<MediaSourceKind, string>> = { chat: 'pi pi-comments', 'generated-image': 'pi pi-image', 'tool-result': 'pi pi-wrench', sandbox: 'pi pi-box' }

export function formatMediaTime(seconds: number): string { return new Date(seconds * 1000).toLocaleString() }
export function effectiveSourceKinds(item: Pick<MediaItem, 'sourceKinds'>): readonly MediaSourceKind[] { return item.sourceKinds.length === 0 ? ['chat'] : item.sourceKinds }
export function mediaIcon(mimeType: string): string {
  if (mimeType.startsWith('image/')) return 'pi pi-image'
  if (mimeType.startsWith('audio/')) return 'pi pi-volume-up'
  if (mimeType.startsWith('video/')) return 'pi pi-video'
  return 'pi pi-file'
}
export function sourcePlatform(source: string): string {
  const normalized = source.trim().toLowerCase()
  if (normalized.includes('qq.com') || normalized.startsWith('qq:') || normalized.startsWith('qqfile:')) return 'qq'
  if (normalized.startsWith('mxc://') || normalized.startsWith('matrix:') || normalized.includes('matrix.to')) return 'matrix'
  if (normalized.startsWith('telegram:') || normalized.includes('telegram.org') || normalized.includes('t.me/')) return 'telegram'
  if (normalized.startsWith('discord:') || ['discord.com', 'discordapp.com', 'discordapp.net'].some((host) => normalized.includes(host))) return 'discord'
  return 'web'
}
export function platformIcon(platform: string): string { return platformIcons[platform.toLowerCase()] ?? 'pi pi-link' }
export function platformLabel(platform: string): string { return platformLabels[platform.toLowerCase()] ?? platform }
