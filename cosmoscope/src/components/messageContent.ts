export interface MessageContentAttachment {
  readonly key: string
  readonly name: string
  readonly detail: string
  readonly mimeType: string
  readonly url?: string
  readonly mediaId?: string
}
