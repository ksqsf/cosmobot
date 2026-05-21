export type ConnectionStatus = 'disconnected' | 'connecting' | 'connected' | 'error';

export type MessageRole = 'user' | 'assistant' | 'system' | 'tool';

export type AttachmentKind = 'image' | 'audio' | 'file';

export type AttachmentStatus = 'queued' | 'uploading' | 'uploaded' | 'failed';

export interface RpcConfig {
  url: string;
  token: string;
}

export interface SessionSummary {
  id: string;
  title: string;
  updatedAt?: string;
  messageCount?: number;
  branchOf?: string;
}

export interface ChatAttachment {
  id: string;
  name: string;
  mediaType: string;
  size: number;
  kind: AttachmentKind;
  url?: string;
}

export interface QueuedAttachment {
  localId: string;
  file: File;
  name: string;
  mediaType: string;
  size: number;
  kind: AttachmentKind;
  status: AttachmentStatus;
  error?: string;
  remote?: ChatAttachment;
}

export interface ChatMessage {
  sessionId: string;
  messageId: string;
  role: MessageRole;
  text: string;
  parentMessageId?: string;
  branchId?: string;
  attachments: ChatAttachment[];
  createdAt?: string;
  updatedAt?: string;
  streaming: boolean;
}

export interface AuditEvent {
  id: string;
  title: string;
  subtitle: string;
  payload: unknown;
}

export interface EventLogEntry {
  id: number;
  at: string;
  text: string;
}

export interface RpcErrorPayload {
  code?: number | string;
  message: string;
  data?: unknown;
}

export type RpcResponse<T = unknown> = {
  jsonrpc?: string;
  id: string | number | null;
} & (
  | { result: T; error?: never }
  | { error: RpcErrorPayload; result?: never }
);

export interface RpcNotification {
  jsonrpc?: string;
  method: string;
  params?: unknown;
}

export type IncomingRpcMessage = RpcResponse | RpcNotification;
