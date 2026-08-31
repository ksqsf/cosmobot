import { ref, type Ref } from 'vue'
import { getMedia } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import { safeDownloadUrl, safeImageUrl } from '@/backend/chat'
import type { MessageContentAttachment } from '@/components/messageContent'
import { mediaRefs } from '@/domain/threadTranscript'
import { formatBytes } from '@/format'
import type { MediaDetail, StoredThreadMessage } from '@/types/domain'

interface TranscriptMedia {
  readonly mediaByRef: Ref<ReadonlyMap<string, MediaDetail>>
  readonly previewImage: Ref<string | undefined>
  readonly reset: () => void
  readonly loadMediaForMessages: (messages: readonly StoredThreadMessage[], current?: () => boolean) => Promise<void>
  readonly imageUrls: (message: StoredThreadMessage) => string[]
  readonly messageAttachments: (message: StoredThreadMessage) => MessageContentAttachment[]
}

export function useTranscriptMedia(): TranscriptMedia {
  const mediaByRef = ref<ReadonlyMap<string, MediaDetail>>(new Map())
  const previewImage = ref<string>()

  function reset(): void {
    mediaByRef.value = new Map()
  }

  async function loadMediaForMessages(messages: readonly StoredThreadMessage[], current: () => boolean = () => true): Promise<void> {
    const refs = [...new Set(messages.flatMap(mediaRefs))].filter((ref) => !mediaByRef.value.has(ref))
    if (refs.length === 0) return
    const results = await Promise.all(refs.map(async (ref) => [ref, await runBackend(getMedia(ref))] as const))
    if (!current()) return
    mediaByRef.value = new Map([...mediaByRef.value, ...results.flatMap(([ref, result]) => result._tag === 'Success' ? [[ref, result.value] as const] : [])])
  }

  function mediaDetails(message: StoredThreadMessage): MediaDetail[] {
    return mediaRefs(message).flatMap((ref) => {
      const media = mediaByRef.value.get(ref)
      return media === undefined ? [] : [media]
    })
  }

  function imageUrls(message: StoredThreadMessage): string[] {
    const contentUrls = Array.isArray(message.content) ? message.content.flatMap((part) => {
      const ref = typeof part.image_url === 'string' ? part.image_url : part.image_url?.url
      return ref === undefined ? [] : [mediaByRef.value.get(ref)?.publicUrl ?? ref]
    }) : []
    const mediaUrls = mediaDetails(message)
      .filter(({ exists, mimeType }) => exists && mimeType.startsWith('image/'))
      .map(({ publicUrl }) => publicUrl)
    return [...new Set([...contentUrls, ...mediaUrls].flatMap((resolved) => {
      const safe = safeImageUrl(resolved, window.location.href)
      return safe === undefined ? [] : [safe]
    }))]
  }

  function messageAttachments(message: StoredThreadMessage): MessageContentAttachment[] {
    return mediaDetails(message)
      .filter(({ mimeType }) => !mimeType.startsWith('image/'))
      .map((media) => {
        const url = media.exists ? safeDownloadUrl(media.publicUrl, window.location.href) : undefined
        return {
          key: media.mediaId,
          name: media.sourceName ?? media.mediaId,
          detail: `${media.mimeType} · ${formatBytes(media.size)}`,
          mimeType: media.mimeType,
          mediaId: media.mediaId,
          ...(url === undefined ? {} : { url }),
        }
      })
  }

  return { mediaByRef, previewImage, reset, loadMediaForMessages, imageUrls, messageAttachments }
}
