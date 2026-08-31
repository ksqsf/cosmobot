import { computed, onScopeDispose, reactive, ref, type ComputedRef, type Ref, type WritableComputedRef } from 'vue'
import { useToast } from 'primevue/usetoast'
import { discardChatAttachment, sendChatMessage, uploadChatAttachment } from '@/backend/AdminBackend'
import { runBackend } from '@/backend/runBackend'
import type { ChatAttachment, ChatMessage } from '@/types/domain'
import type { ChatSelection } from '@/composables/useChatSession'

const maxAttachmentBytes = 25 * 1024 * 1024

interface ChatComposerOptions {
  readonly error: Ref<string>
  readonly selectedId: ComputedRef<string | undefined>
  readonly captureSelection: () => ChatSelection | undefined
  readonly isCurrentSelection: (selection: ChatSelection) => boolean
  readonly mergeMessage: (message: ChatMessage) => void
}

export interface ChatComposerState {
  readonly draft: WritableComputedRef<string>
  readonly pendingAttachments: Ref<readonly ChatAttachment[]>
  readonly sending: Ref<boolean>
  readonly uploading: Ref<boolean>
  readonly attachFiles: (files: readonly File[]) => Promise<void>
  readonly discardAttachment: (attachment: ChatAttachment) => Promise<void>
  readonly send: () => Promise<void>
  readonly prepareSelectionChange: () => Promise<boolean>
  readonly selectionChanged: () => void
  readonly selectionInvalidated: () => void
  readonly forgetSession: (sessionId: string) => void
}

export function useChatComposer(options: ChatComposerOptions): ChatComposerState {
  const toast = useToast()
  const drafts = reactive(new Map<string, string>())
  const pendingAttachments = ref<readonly ChatAttachment[]>([])
  const sending = ref(false)
  const uploading = ref(false)
  let composerGeneration = 0
  let sendGeneration = 0
  let disposed = false

  const draft = computed({
    get: () => options.selectedId.value === undefined ? '' : drafts.get(options.selectedId.value) ?? '',
    set: (value: string) => { if (options.selectedId.value !== undefined) drafts.set(options.selectedId.value, value) },
  })

  function selectionChanged(): void {
    sendGeneration += 1
    sending.value = false
  }

  function selectionInvalidated(): void {
    composerGeneration += 1
    selectionChanged()
    uploading.value = false
  }

  async function attachFiles(files: readonly File[]): Promise<void> {
    const oversized = files.find(({ size }) => size > maxAttachmentBytes)
    if (oversized !== undefined) {
      toast.add({ severity: 'error', summary: `${oversized.name} exceeds the 25 MiB limit`, life: 3500 })
      return
    }
    const sessionId = options.selectedId.value
    const generation = composerGeneration
    if (sessionId === undefined) return
    uploading.value = true
    for (const file of files) {
      const result = await runBackend(uploadChatAttachment(file))
      if (generation !== composerGeneration || options.selectedId.value !== sessionId) {
        if (result._tag === 'Success') await runBackend(discardChatAttachment(result.value.attachmentId))
        break
      }
      if (result._tag === 'Failure') { options.error.value = result.error.message; break }
      pendingAttachments.value = [...pendingAttachments.value, result.value]
    }
    if (generation === composerGeneration) uploading.value = false
  }

  async function discardAttachment(attachment: ChatAttachment): Promise<void> {
    const sessionId = options.selectedId.value
    const composer = composerGeneration
    if (sessionId === undefined || !pendingAttachments.value.some(({ attachmentId }) => attachmentId === attachment.attachmentId)) return
    pendingAttachments.value = pendingAttachments.value.filter(({ attachmentId }) => attachmentId !== attachment.attachmentId)
    const result = await runBackend(discardChatAttachment(attachment.attachmentId))
    if (result._tag === 'Failure' && !disposed && composer === composerGeneration && options.selectedId.value === sessionId) {
      pendingAttachments.value = [...pendingAttachments.value, attachment]
      options.error.value = result.error.message
    }
  }

  async function discardPendingAttachments(): Promise<boolean> {
    const sessionId = options.selectedId.value
    const composer = composerGeneration
    const attachments = pendingAttachments.value
    pendingAttachments.value = []
    const results = await Promise.all(attachments.map(({ attachmentId }) => runBackend(discardChatAttachment(attachmentId))))
    const retained = attachments.filter((_attachment, index) => results[index]?._tag === 'Failure')
    if (!disposed && composer === composerGeneration && options.selectedId.value === sessionId) {
      pendingAttachments.value = [...retained, ...pendingAttachments.value]
      if (retained.length > 0) options.error.value = 'Could not discard every pending attachment.'
    }
    return retained.length === 0
  }

  async function prepareSelectionChange(): Promise<boolean> {
    const composer = ++composerGeneration
    uploading.value = true
    const discarded = await discardPendingAttachments()
    if (composer !== composerGeneration) return false
    uploading.value = false
    return discarded
  }

  async function send(): Promise<void> {
    const selection = options.captureSelection()
    const composer = composerGeneration
    const originalDraft = draft.value
    const text = originalDraft.trim()
    const attachments = pendingAttachments.value
    if (selection === undefined || sending.value || text === '' && attachments.length === 0) return
    const generation = ++sendGeneration
    pendingAttachments.value = []
    sending.value = true
    const result = await runBackend(sendChatMessage({ sessionId: selection.sessionId, text, attachments }))
    if (generation === sendGeneration) sending.value = false
    const current = composer === composerGeneration && options.isCurrentSelection(selection)
    if (result._tag === 'Failure') {
      if (current) {
        const pendingIds = new Set(pendingAttachments.value.map(({ attachmentId }) => attachmentId))
        pendingAttachments.value = [...attachments.filter(({ attachmentId }) => !pendingIds.has(attachmentId)), ...pendingAttachments.value]
        options.error.value = result.error.message
      } else {
        await Promise.all(attachments.map(({ attachmentId }) => runBackend(discardChatAttachment(attachmentId))))
      }
      return
    }
    if (drafts.get(selection.sessionId) === originalDraft) drafts.delete(selection.sessionId)
    if (current) options.mergeMessage({
      sessionId: selection.sessionId,
      messageId: result.value,
      sender: 'user',
      text,
      imageUrls: [],
      attachments,
      replyToMessageId: null,
      parentMessageId: null,
    })
  }

  function forgetSession(sessionId: string): void {
    drafts.delete(sessionId)
  }

  onScopeDispose(() => {
    disposed = true
    composerGeneration += 1
    sendGeneration += 1
    void discardPendingAttachments()
  })

  return {
    draft, pendingAttachments, sending, uploading, attachFiles, discardAttachment, send,
    prepareSelectionChange, selectionChanged, selectionInvalidated, forgetSession,
  }
}
