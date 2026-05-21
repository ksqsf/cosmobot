import { get } from 'svelte/store';
import { arrayOf, booleanValue, err, field, numberValue, ok, optionalField, stringValue, unknownRecord, type DecodeResult } from './decoders';
import { addAuditEvent, addLog, connection, currentSession, mergeMessage, sessions, setCurrentSession, setHistory } from './stores';
import type { AuditEvent, ChatAttachment, ChatMessage, IncomingRpcMessage, MessageRole, QueuedAttachment, RpcConfig, RpcNotification, RpcResponse, SessionSummary } from './types';

type PendingRequest = {
  method: string;
  resolve: (value: unknown) => void;
  reject: (reason: Error) => void;
  timeout: ReturnType<typeof setTimeout>;
};

const storageKey = 'cosmobot.web.rpc';
const pending = new Map<string, PendingRequest>();

let ws: WebSocket | null = null;
let nextId = 1;

export const defaultConfig = (): RpcConfig => ({
  url: defaultWsUrl(),
  token: ''
});

export const loadConfig = (): RpcConfig => {
  const fallback = defaultConfig();
  const raw = localStorage.getItem(storageKey);
  if (raw === null) {
    return fallback;
  }
  try {
    const parsed = unknownRecord(JSON.parse(raw), 'config');
    if (!parsed.ok) {
      return fallback;
    }
    return {
      url: typeof parsed.value['url'] === 'string' ? parsed.value['url'] : fallback.url,
      token: typeof parsed.value['token'] === 'string' ? parsed.value['token'] : ''
    };
  } catch {
    return fallback;
  }
};

export const saveConfig = (config: RpcConfig): void => {
  localStorage.setItem(storageKey, JSON.stringify(config));
};

export const connectRpc = (config: RpcConfig): void => {
  disconnectRpc();
  saveConfig(config);
  connection.set({ status: 'connecting', message: 'Connecting' });
  const socket = new WebSocket(buildUrl(config));
  ws = socket;

  socket.addEventListener('open', () => {
    connection.set({ status: 'connected', message: 'Connected' });
    addLog('Connected');
    void refreshSessions();
    void refreshAudit();
  });
  socket.addEventListener('message', (event: MessageEvent<string>) => {
    handleFrame(event.data);
  });
  socket.addEventListener('error', () => {
    connection.set({ status: 'error', message: 'Connection error' });
  });
  socket.addEventListener('close', () => {
    if (ws === socket) {
      ws = null;
    }
    for (const [id, request] of pending) {
      clearTimeout(request.timeout);
      request.reject(new Error('WebSocket closed'));
      pending.delete(id);
    }
    connection.set({ status: 'disconnected', message: 'Disconnected' });
    addLog('Disconnected');
  });
};

export const disconnectRpc = (): void => {
  if (ws !== null) {
    ws.close();
    ws = null;
  }
};

export const requestRpc = async <T>(method: string, params: Record<string, unknown> = {}): Promise<T> => {
  if (ws === null || ws.readyState !== WebSocket.OPEN) {
    throw new Error('WebSocket is not connected');
  }
  const id = String(nextId);
  nextId += 1;
  ws.send(JSON.stringify({ jsonrpc: '2.0', id, method, params }));
  addLog(`-> ${method} #${id}`);
  return new Promise<T>((resolve, reject) => {
    const timeout = setTimeout(() => {
      pending.delete(id);
      reject(new Error(`Request timed out: ${method}`));
    }, 30000);
    pending.set(id, { method, resolve: (value) => { resolve(value as T); }, reject, timeout });
  });
};

export const requestFirst = async <T>(methods: string[], params: Record<string, unknown>): Promise<T> => {
  let lastError: Error | undefined;
  for (const method of methods) {
    try {
      return await requestRpc<T>(method, params);
    } catch (error) {
      const normalized = toError(error);
      lastError = normalized;
      if (!/method_not_found|unknown rpc method/i.test(normalized.message)) {
        throw normalized;
      }
    }
  }
  throw lastError ?? new Error('No RPC method was available');
};

export const openSession = async (label: string): Promise<void> => {
  const result = await requestFirst<unknown>(['chat.open_session', 'chat.open'], label.length > 0 ? { label } : {});
  const sessionId = decodeSessionId(result);
  if (sessionId === '') {
    throw new Error('chat.open_session returned no session id');
  }
  sessions.update((rows) => [{ id: sessionId, title: label.length > 0 ? label : sessionId }, ...rows.filter((row) => row.id !== sessionId)]);
  setCurrentSession(sessionId);
  setHistory([]);
};

export const refreshSessions = async (): Promise<void> => {
  try {
    const result = await requestRpc<unknown>('chat.list_sessions', {});
    const decoded = decodeSessions(result);
    if (decoded.ok) {
      sessions.set(decoded.value);
    } else {
      addLog(`Session list decode failed: ${decoded.error}`);
    }
  } catch (error) {
    logNonMissing('Load sessions failed', error);
  }
};

export const loadHistory = async (sessionId: string): Promise<void> => {
  setCurrentSession(sessionId);
  if (sessionId === '') {
    setHistory([]);
    return;
  }
  try {
    const result = await requestFirst<unknown>(['chat.history', 'chat.get_session'], { sessionId, session_id: sessionId });
    const decoded = decodeHistory(result, sessionId);
    if (!decoded.ok) {
      throw new Error(decoded.error);
    }
    setHistory(decoded.value);
  } catch (error) {
    logNonMissing('Load history failed', error);
  }
};

export const sendChat = async (sessionId: string, text: string, attachments: QueuedAttachment[]): Promise<void> => {
  const uploaded = attachments.flatMap((item) => (item.remote === undefined ? [] : [item.remote]));
  await requestRpc('chat.send', {
    sessionId,
    session_id: sessionId,
    text,
    attachments: uploaded.map(attachmentParam),
    imageUrls: uploaded.filter((item) => item.kind === 'image' && item.url !== undefined).map((item) => item.url)
  });
};

export const forkFrom = async (sessionId: string, messageId: string): Promise<void> => {
  const result = await requestRpc<unknown>('chat.fork', { sessionId, session_id: sessionId, messageId, message_id: messageId });
  const forkedId = decodeSessionId(result);
  if (forkedId !== '') {
    await loadHistory(forkedId);
    await refreshSessions();
  }
};

export const deleteSession = async (sessionId: string): Promise<void> => {
  await requestRpc('chat.delete_session', { sessionId, session_id: sessionId });
  sessions.update((rows) => rows.filter((row) => row.id !== sessionId));
  if (get(currentSession) === sessionId) {
    setCurrentSession('');
    setHistory([]);
  }
};

export const renameSession = async (sessionId: string, title: string): Promise<void> => {
  await requestRpc('chat.rename_session', { sessionId, session_id: sessionId, title, label: title });
  sessions.update((rows) => rows.map((row) => (row.id === sessionId ? { ...row, title } : row)));
};

export const uploadAttachment = async (queued: QueuedAttachment): Promise<ChatAttachment> => {
  const result = await requestRpc<unknown>('chat.upload_attachment', {
    name: queued.name,
    mediaType: queued.mediaType,
    media_type: queued.mediaType,
    size: queued.size,
    kind: queued.kind,
    data: await fileToBase64(queued.file)
  });
  const decoded = decodeAttachment(result, 'attachment');
  if (!decoded.ok) {
    throw new Error(decoded.error);
  }
  return decoded.value;
};

export const deleteAttachment = async (attachmentId: string): Promise<void> => {
  await requestRpc('chat.delete_attachment', { attachmentId, attachment_id: attachmentId });
};

export const refreshAudit = async (): Promise<void> => {
  try {
    const result = await requestRpc<unknown>('audit.recent', { limit: 80 });
    const decoded = decodeAuditEvents(result);
    if (decoded.ok) {
      decoded.value.reverse().forEach((event) => {
        addAuditEvent(event);
      });
    }
  } catch (error) {
    logNonMissing('Load audit failed', error);
  }
};

export const subscribeAudit = async (): Promise<void> => {
  await requestRpc('audit.subscribe', {});
};

export const decodeIncomingRpcMessage = (value: unknown): DecodeResult<IncomingRpcMessage> => {
  const record = unknownRecord(value, 'rpc');
  if (!record.ok) {
    return record;
  }
  if ('method' in record.value) {
    const method = field(record.value, 'method', stringValue, 'rpc');
    if (!method.ok) {
      return method;
    }
    const notification: RpcNotification = { method: method.value };
    const jsonrpc = optionalText(record.value['jsonrpc']);
    if (jsonrpc !== undefined) {
      notification.jsonrpc = jsonrpc;
    }
    if ('params' in record.value) {
      notification.params = record.value['params'];
    }
    return ok(notification);
  }
  if ('id' in record.value) {
    const id = record.value['id'];
    if (id !== null && typeof id !== 'string' && typeof id !== 'number') {
      return err('rpc.id must be string, number, or null');
    }
    const response: RpcResponse = { id };
    const jsonrpc = optionalText(record.value['jsonrpc']);
    if (jsonrpc !== undefined) {
      response.jsonrpc = jsonrpc;
    }
    if ('result' in record.value) {
      response.result = record.value['result'];
    }
    if ('error' in record.value) {
      const error = decodeRpcError(record.value['error']);
      if (!error.ok) {
        return error;
      }
      response.error = error.value;
    }
    return ok(response);
  }
  return err('rpc message must be a response or notification');
};

export const decodeChatMessage = (value: unknown, fallbackSessionId = ''): DecodeResult<ChatMessage> => {
  const record = unknownRecord(value, 'message');
  if (!record.ok) {
    return record;
  }
  const messageId = firstString(record.value, ['messageId', 'message_id', 'id']) ?? `local-${String(Date.now())}`;
  const sessionId = firstString(record.value, ['sessionId', 'session_id']) ?? fallbackSessionId;
  const attachments = decodeAttachmentList(record.value['attachments'] ?? []);
  const images = arrayOf(stringValue)(record.value['imageUrls'] ?? record.value['image_urls'] ?? [], 'message.imageUrls');
  const imageAttachments: ChatAttachment[] = images.ok
    ? images.value.map((url, index) => ({ id: `${messageId}-image-${String(index)}`, name: url.split('/').at(-1) ?? 'image', mediaType: 'image/*', size: 0, kind: 'image', url }))
    : [];
  const message: ChatMessage = {
    sessionId,
    messageId,
    role: decodeRole(firstString(record.value, ['role', 'sender']) ?? 'assistant'),
    text: firstString(record.value, ['text', 'content', 'delta']) ?? '',
    attachments: [...(attachments.ok ? attachments.value : []), ...imageAttachments],
    streaming: firstBoolean(record.value, ['streaming', 'isStreaming']) ?? false
  };
  addOptional(message, 'parentMessageId', firstString(record.value, ['parentMessageId', 'parent_message_id', 'replyToMessageId', 'reply_to_message_id']));
  addOptional(message, 'branchId', firstString(record.value, ['branchId', 'branch_id']));
  addOptional(message, 'createdAt', firstString(record.value, ['createdAt', 'created_at']));
  addOptional(message, 'updatedAt', firstString(record.value, ['updatedAt', 'updated_at']));
  return ok(message);
};

export const attachmentKind = (mediaType: string): 'image' | 'audio' | 'file' => {
  if (mediaType.startsWith('image/')) {
    return 'image';
  }
  if (mediaType.startsWith('audio/')) {
    return 'audio';
  }
  return 'file';
};

export const toError = (error: unknown): Error => (error instanceof Error ? error : new Error(String(error)));

const handleFrame = (raw: string): void => {
  let parsed: unknown;
  try {
    parsed = JSON.parse(raw);
  } catch (error) {
    addLog(`Invalid JSON: ${toError(error).message}`);
    return;
  }
  const decoded = decodeIncomingRpcMessage(parsed);
  if (!decoded.ok) {
    addLog(`RPC decode failed: ${decoded.error}`);
    return;
  }
  if ('method' in decoded.value) {
    handleNotification(decoded.value);
  } else {
    handleResponse(decoded.value);
  }
};

const handleResponse = (response: RpcResponse): void => {
  const id = String(response.id);
  const request = pending.get(id);
  if (request === undefined) {
    addLog(`<- response #${id} without pending request`);
    return;
  }
  clearTimeout(request.timeout);
  pending.delete(id);
  if (response.error !== undefined) {
    const detail = [response.error.code, response.error.message].filter(Boolean).join(': ');
    addLog(`<- ${request.method} failed: ${detail}`);
    request.reject(new Error(detail));
    return;
  }
  addLog(`<- ${request.method} ok`);
  request.resolve(response.result);
};

const handleNotification = (notification: RpcNotification): void => {
  addLog(`<- ${notification.method}`);
  switch (notification.method) {
    case 'chat.message':
    case 'chat.message_update': {
      const decoded = decodeChatMessage(notification.params, get(currentSession));
      if (decoded.ok) {
        mergeMessage({ ...decoded.value, streaming: notification.method === 'chat.message_update' });
      } else {
        addLog(`Chat message decode failed: ${decoded.error}`);
      }
      break;
    }
    case 'audit.event':
    case 'review.event':
    case 'run.event':
      addAuditEvent(toAuditEvent(notification.params));
      break;
    default:
      break;
  }
};

const buildUrl = (config: RpcConfig): string => {
  const url = new URL(config.url);
  if (config.token.length > 0) {
    url.searchParams.set('access_token', config.token);
  }
  return url.toString();
};

const defaultWsUrl = (): string => {
  const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
  return `${protocol}//${window.location.host}/rpc`;
};

const decodeSessionId = (value: unknown): string => {
  if (typeof value === 'string') {
    return value;
  }
  const record = unknownRecord(value, 'session');
  if (!record.ok) {
    return '';
  }
  return firstString(record.value, ['sessionId', 'session_id', 'id', 'unRpcSessionId']) ?? '';
};

const decodeSession = (value: unknown, path = 'session'): DecodeResult<SessionSummary> => {
  const record = unknownRecord(value, path);
  if (!record.ok) {
    return record;
  }
  const id = firstString(record.value, ['id', 'sessionId', 'session_id']);
  if (id === undefined) {
    return err(`${path}.id must be present`);
  }
  const session: SessionSummary = { id, title: firstString(record.value, ['title', 'label', 'name']) ?? id };
  addOptional(session, 'updatedAt', firstString(record.value, ['updatedAt', 'updated_at']));
  addOptional(session, 'messageCount', firstNumber(record.value, ['messageCount', 'message_count']));
  addOptional(session, 'branchOf', firstString(record.value, ['branchOf', 'branch_of', 'parentSessionId', 'parent_session_id']));
  return ok(session);
};

const decodeSessions = (value: unknown): DecodeResult<SessionSummary[]> => {
  const rows = Array.isArray(value) ? value : maybeArrayField(value, ['sessions', 'records', 'items']);
  return arrayOf(decodeSession)(rows ?? [], 'sessions');
};

const decodeHistory = (value: unknown, fallbackSessionId: string): DecodeResult<ChatMessage[]> => {
  const rows = Array.isArray(value) ? value : maybeArrayField(value, ['messages', 'history', 'records']);
  return arrayOf((item) => decodeChatMessage(item, fallbackSessionId))(rows ?? [], 'history');
};

const decodeAttachmentList = (value: unknown): DecodeResult<ChatAttachment[]> => arrayOf(decodeAttachment)(value, 'attachments');

const decodeAttachment = (value: unknown, path = 'attachment'): DecodeResult<ChatAttachment> => {
  const record = unknownRecord(value, path);
  if (!record.ok) {
    return record;
  }
  const id = firstString(record.value, ['id', 'attachmentId', 'attachment_id']);
  if (id === undefined) {
    return err(`${path}.id must be present`);
  }
  const mediaType = firstString(record.value, ['mediaType', 'media_type', 'contentType', 'content_type']) ?? 'application/octet-stream';
  const attachment: ChatAttachment = {
    id,
    name: firstString(record.value, ['name', 'filename', 'fileName']) ?? id,
    mediaType,
    size: firstNumber(record.value, ['size', 'byteSize', 'byte_size']) ?? 0,
    kind: attachmentKind(mediaType)
  };
  addOptional(attachment, 'url', firstString(record.value, ['url', 'href']) ?? `/attachments/${encodeURIComponent(id)}`);
  return ok(attachment);
};

const decodeAuditEvents = (value: unknown): DecodeResult<AuditEvent[]> => {
  const rows = Array.isArray(value) ? value : maybeArrayField(value, ['events', 'records', 'items']);
  return arrayOf((item) => ok(toAuditEvent(item)))(rows ?? [], 'audit.events');
};

const decodeRpcError = (value: unknown): DecodeResult<{ code?: number | string; message: string; data?: unknown }> => {
  const record = unknownRecord(value, 'rpc.error');
  if (!record.ok) {
    return record;
  }
  const message = field(record.value, 'message', stringValue, 'rpc.error');
  if (!message.ok) {
    return message;
  }
  const error: { code?: number | string; message: string; data?: unknown } = { message: message.value };
  const code = record.value['code'];
  if (typeof code === 'string' || typeof code === 'number') {
    error.code = code;
  }
  if ('data' in record.value) {
    error.data = record.value['data'];
  }
  return ok(error);
};

const toAuditEvent = (payload: unknown): AuditEvent => {
  const record = unknownRecord(payload, 'audit');
  if (!record.ok) {
    return { id: crypto.randomUUID(), title: 'Event', subtitle: '', payload };
  }
  const id = firstString(record.value, ['id', 'auditId', 'audit_id', 'recordId', 'record_id']) ?? String(firstNumber(record.value, ['id', 'auditId', 'audit_id']) ?? crypto.randomUUID());
  const nested = unknownRecord(record.value['event'], 'audit.event');
  const eventRecord = nested.ok ? nested.value : record.value;
  return {
    id,
    title: [firstString(eventRecord, ['type', 'tag', 'kind']) ?? 'event', firstString(eventRecord, ['toolName', 'tool', 'name'])].filter(Boolean).join(' '),
    subtitle: firstString(record.value, ['occurredAt', 'createdAt', 'timestamp', 'status']) ?? '',
    payload
  };
};

const attachmentParam = (attachment: ChatAttachment): Record<string, unknown> => ({
  id: attachment.id,
  name: attachment.name,
  mediaType: attachment.mediaType,
  media_type: attachment.mediaType,
  size: attachment.size,
  kind: attachment.kind,
  url: attachment.url
});

const fileToBase64 = async (file: File): Promise<string> => {
  const buffer = await file.arrayBuffer();
  let binary = '';
  for (const byte of new Uint8Array(buffer)) {
    binary += String.fromCharCode(byte);
  }
  return btoa(binary);
};

const optionalText = (value: unknown): string | undefined => (typeof value === 'string' ? value : undefined);

const decodeRole = (value: string): MessageRole => {
  switch (value) {
    case 'user':
      return 'user';
    case 'assistant':
    case 'bot':
      return 'assistant';
    case 'system':
      return 'system';
    case 'tool':
      return 'tool';
    default:
      return 'assistant';
  }
};

const firstString = (record: Record<string, unknown>, names: string[]): string | undefined => {
  for (const name of names) {
    const decoded = optionalField(record, name, stringValue, 'record');
    if (decoded.ok && decoded.value !== undefined) {
      return decoded.value;
    }
  }
  return undefined;
};

const firstNumber = (record: Record<string, unknown>, names: string[]): number | undefined => {
  for (const name of names) {
    const decoded = optionalField(record, name, numberValue, 'record');
    if (decoded.ok && decoded.value !== undefined) {
      return decoded.value;
    }
  }
  return undefined;
};

const firstBoolean = (record: Record<string, unknown>, names: string[]): boolean | undefined => {
  for (const name of names) {
    const decoded = optionalField(record, name, booleanValue, 'record');
    if (decoded.ok && decoded.value !== undefined) {
      return decoded.value;
    }
  }
  return undefined;
};

const maybeArrayField = (value: unknown, names: string[]): unknown[] | undefined => {
  const record = unknownRecord(value, 'record');
  if (!record.ok) {
    return undefined;
  }
  for (const name of names) {
    const candidate = record.value[name];
    if (Array.isArray(candidate)) {
      const rows: unknown[] = Array.from(candidate as readonly unknown[]);
      return rows;
    }
  }
  return undefined;
};

const addOptional = <T extends object, K extends keyof T>(target: T, key: K, value: T[K] | undefined): void => {
  if (value !== undefined) {
    target[key] = value;
  }
};

const logNonMissing = (prefix: string, error: unknown): void => {
  const message = toError(error).message;
  if (!/method_not_found|unknown rpc method/i.test(message)) {
    addLog(`${prefix}: ${message}`);
  }
};
