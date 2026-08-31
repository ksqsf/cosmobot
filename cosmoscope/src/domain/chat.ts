import { safeDownloadUrl, safeImageUrl } from '@/backend/chat'
import type { MessageContentAttachment } from '@/components/messageContent'
import type { ChatAttachment, ChatMessage, ChatSession } from '@/types/domain'

export function chatSessionName(session: ChatSession): string {
  const label = session.label?.trim()
  return label === undefined || label === '' ? session.sessionId : label
}

export function chatMessageImages(message: ChatMessage, base: string): readonly string[] {
  return [...new Set([...message.imageUrls, ...message.attachments.filter(({ kind }) => kind === 'image').map(({ url }) => url)])]
    .flatMap((url) => safeImageUrl(url, base) ?? [])
}

export function chatMessageAttachments(message: ChatMessage, base: string): readonly MessageContentAttachment[] {
  return message.attachments
    .filter(({ kind }) => kind !== 'image')
    .map((attachment) => ({
      key: attachment.attachmentId,
      name: attachment.name,
      detail: `${attachment.mediaType} · ${String(Math.ceil(attachment.size / 1024))} KiB`,
      // Chat attachments remain download cards; message media uses richer audio/video previews.
      mimeType: 'application/octet-stream',
      ...downloadUrl(attachment, base),
    }))
}

function downloadUrl(attachment: ChatAttachment, base: string): { readonly url?: string } {
  const url = safeDownloadUrl(attachment.url, base)
  return url === undefined ? {} : { url }
}
