import { arrayOf, booleanValue, err, field, numberValue, ok, optionalField, stringValue, unknownRecord, type DecodeResult } from './decoders';
import { addAuditEvent, addLog, appStore, connectionAtom, currentSessionAtom, mergeMessage, setCurrentSession, setHistory, setSessions, updateSessions } from './stores';
import type { AuditEvent, ChatAttachment, ChatMessage, IncomingRpcMessage, MessageRole, QueuedAttachment, RpcConfig, RpcNotification, RpcResponse, SessionSummary } from './types';

type PendingRequest = {
  method: string;
  resolve: (value: unknown) => void;
  reject: (reason: Error) => void;
  timeout: ReturnType<typeof setTimeout>;
};

const storageKey = 'cosmobot.web.rpc';
const legacySessionTokenStorageKey = 'cosmobot.web.rpc.token';
const pending = new Map<string, PendingRequest>();

let ws: WebSocket | null = null;
let nextId = 1;
let reconnectTimer: ReturnType<typeof setTimeout> | null = null;
let reconnectAttempts = 0;
let manualDisconnect = false;

const maxReconnectAttempts = 3;

export const maxAttachmentBytes = 25 * 1024 * 1024;

export const defaultConfig = (): RpcConfig => ({
  url: defaultWsUrl(),
  token: ''
});

export const loadConfig = (): RpcConfig => {
  const fallback = defaultConfig();
  const legacySessionToken = sessionStorage.getItem(legacySessionTokenStorageKey) ?? '';
  const raw = localStorage.getItem(storageKey);
  if (raw === null) {
    return { ...fallback, token: legacySessionToken };
  }
  try {
    const parsed = unknownRecord(JSON.parse(raw), 'config');
    if (!parsed.ok) {
      localStorage.removeItem(storageKey);
      return { ...fallback, token: legacySessionToken };
    }
    const token = typeof parsed.value['token'] === 'string' ? parsed.value['token'] : legacySessionToken;
    const config = {
      url: sanitizeRpcUrl(typeof parsed.value['url'] === 'string' ? parsed.value['url'] : fallback.url),
      token
    };
    if (parsed.value['url'] !== config.url || parsed.value['token'] !== config.token) {
      saveConfig(config);
    }
    return config;
  } catch {
    localStorage.removeItem(storageKey);
    return { ...fallback, token: legacySessionToken };
  }
};

export const saveConfig = (config: RpcConfig): void => {
  localStorage.setItem(storageKey, JSON.stringify({ url: sanitizeRpcUrl(config.url), token: config.token }));
  sessionStorage.removeItem(legacySessionTokenStorageKey);
};

export const connectRpc = (config: RpcConfig): void => {
  closeRpcSocket();
  clearReconnectTimer();
  manualDisconnect = false;
  saveConfig(config);
  openRpcSocket(config);
};

const openRpcSocket = (config: RpcConfig): void => {
  appStore.set(connectionAtom, { status: 'connecting', message: 'Connecting' });
  const socket = new WebSocket(buildUrl(config), websocketProtocols(config));
  ws = socket;

  socket.addEventListener('open', () => {
    reconnectAttempts = 0;
    appStore.set(connectionAtom, { status: 'connected', message: 'Connected' });
    addLog('Connected');
    void refreshSessions();
    void refreshAudit();
  });
  socket.addEventListener('message', (event: MessageEvent<string>) => {
    handleFrame(event.data);
  });
  socket.addEventListener('error', () => {
    appStore.set(connectionAtom, { status: 'error', message: 'Connection error' });
  });
  socket.addEventListener('close', (event) => {
    if (ws === socket) {
      ws = null;
    }
    for (const [id, request] of pending) {
      clearTimeout(request.timeout);
      request.reject(new Error('WebSocket closed'));
      pending.delete(id);
    }
    const reason = event.reason.length > 0 ? `: ${event.reason}` : '';
    addLog(`Disconnected (${String(event.code)}${reason})`);
    if (!manualDisconnect && reconnectAttempts < maxReconnectAttempts) {
      reconnectAttempts += 1;
      const delayMs = reconnectAttempts * 1000;
      appStore.set(connectionAtom, { status: 'connecting', message: `Reconnecting ${String(reconnectAttempts)}/${String(maxReconnectAttempts)}` });
      reconnectTimer = setTimeout(() => {
        reconnectTimer = null;
        openRpcSocket(loadConfig());
      }, delayMs);
    } else {
      appStore.set(connectionAtom, { status: 'disconnected', message: 'Disconnected' });
    }
  });
};

export const disconnectRpc = (): void => {
  manualDisconnect = true;
  clearReconnectTimer();
  closeRpcSocket();
  appStore.set(connectionAtom, { status: 'disconnected', message: 'Disconnected' });
};

const closeRpcSocket = (): void => {
  if (ws !== null) {
    ws.close();
    ws = null;
  }
};

const clearReconnectTimer = (): void => {
  if (reconnectTimer !== null) {
    clearTimeout(reconnectTimer);
    reconnectTimer = null;
  }
};

export const requestRpc = async (method: string, params: Record<string, unknown> = {}): Promise<unknown> => {
  if (ws === null || ws.readyState !== WebSocket.OPEN) {
    throw new Error('WebSocket is not connected');
  }
  const id = String(nextId);
  nextId += 1;
  ws.send(JSON.stringify({ jsonrpc: '2.0', id, method, params }));
  addLog(`-> ${method} #${id}`);
  return new Promise<unknown>((resolve, reject) => {
    const timeout = setTimeout(() => {
      pending.delete(id);
      reject(new Error(`Request timed out: ${method}`));
    }, 30000);
    pending.set(id, { method, resolve, reject, timeout });
  });
};

export const requestFirst = async (methods: string[], params: Record<string, unknown>): Promise<unknown> => {
  let lastError: Error | undefined;
  for (const method of methods) {
    try {
      return await requestRpc(method, params);
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

export const openSession = async (label: string): Promise<string> => {
  const result = await requestRpc('chat.open_session', label.length > 0 ? { label } : {});
  const sessionId = decodeSessionId(result);
  if (sessionId === '') {
    throw new Error('chat.open_session returned no session id');
  }
  updateSessions((rows) => [{ id: sessionId, title: label.length > 0 ? label : sessionId }, ...rows.filter((row) => row.id !== sessionId)]);
  setCurrentSession(sessionId);
  setHistory([]);
  return sessionId;
};

export const refreshSessions = async (): Promise<void> => {
  try {
    const result = await requestRpc('chat.list_sessions', {});
    const decoded = decodeSessions(result);
    if (decoded.ok) {
      setSessions(decoded.value);
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
    const result = await requestFirst(['chat.history', 'chat.get_session'], { sessionId, session_id: sessionId });
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
  const result = await requestRpc('chat.fork', { sessionId, session_id: sessionId, messageId, message_id: messageId });
  const forkedId = decodeSessionId(result);
  if (forkedId !== '') {
    await loadHistory(forkedId);
    await refreshSessions();
  }
};

export const deleteSession = async (sessionId: string): Promise<void> => {
  await requestRpc('chat.delete_session', { sessionId, session_id: sessionId });
  updateSessions((rows) => rows.filter((row) => row.id !== sessionId));
  if (appStore.get(currentSessionAtom) === sessionId) {
    setCurrentSession('');
    setHistory([]);
  }
};

export const renameSession = async (sessionId: string, title: string): Promise<void> => {
  await requestRpc('chat.rename_session', { sessionId, session_id: sessionId, title, label: title });
  updateSessions((rows) => rows.map((row) => (row.id === sessionId ? { ...row, title } : row)));
};

export const uploadAttachment = async (queued: QueuedAttachment): Promise<ChatAttachment> => {
  const sizeError = attachmentSizeError(queued.size);
  if (sizeError !== undefined) {
    throw new Error(sizeError);
  }
  const result = await requestRpc('chat.upload_attachment', {
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

export const openAttachment = async (attachment: ChatAttachment): Promise<void> => {
  const blob = await fetchAttachmentBlob(attachment);
  const objectUrl = URL.createObjectURL(blob);
  const link = document.createElement('a');
  link.href = objectUrl;
  link.download = attachment.name;
  link.rel = 'noopener noreferrer';
  document.body.append(link);
  link.click();
  link.remove();
  window.setTimeout(() => {
    URL.revokeObjectURL(objectUrl);
  }, 60000);
};

export const fetchAttachmentObjectUrl = async (attachment: ChatAttachment): Promise<string> => {
  const blob = await fetchAttachmentBlob(attachment);
  return URL.createObjectURL(blob);
};

export const canRenderInlineImage = (attachment: ChatAttachment): boolean =>
  attachment.kind === 'image' &&
  (attachment.mediaType.toLowerCase() === 'image/*' ||
    ['image/png', 'image/jpeg', 'image/jpg', 'image/gif', 'image/webp', 'image/avif'].includes(attachment.mediaType.toLowerCase()));

export const refreshAudit = async (): Promise<void> => {
  try {
    const result = await requestRpc('audit.recent', { limit: 80 });
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
    const hasResult = 'result' in record.value;
    const hasError = 'error' in record.value;
    if (hasResult === hasError) {
      return err('rpc response must include exactly one of result or error');
    }
    const jsonrpc = optionalText(record.value['jsonrpc']);
    const base = jsonrpc === undefined ? { id } : { id, jsonrpc };
    if (hasResult) {
      return ok({ ...base, result: record.value['result'] });
    }
    const error = decodeRpcError(record.value['error']);
    if (!error.ok) {
      return error;
    }
    return ok({ ...base, error: error.value });
  }
  return err('rpc message must be a response or notification');
};

export const attachmentSizeError = (size: number): string | undefined => {
  if (!Number.isFinite(size) || size < 0) {
    return 'Attachment size is invalid';
  }
  if (size > maxAttachmentBytes) {
    return `Attachment is too large: ${formatBytes(size)} exceeds ${formatBytes(maxAttachmentBytes)}`;
  }
  return undefined;
};

export const attachmentParam = (attachment: ChatAttachment): Record<string, unknown> => {
  const param: Record<string, unknown> = {
    attachmentId: attachment.id,
    kind: attachment.kind
  };
  if (attachment.name.length > 0) {
    param['name'] = attachment.name;
  }
  param['id'] = attachment.id;
  return param;
};

const formatBytes = (size: number): string => {
  if (size < 1024) {
    return `${String(size)} B`;
  }
  if (size < 1024 * 1024) {
    return `${(size / 1024).toFixed(1)} KiB`;
  }
  return `${(size / (1024 * 1024)).toFixed(1)} MiB`;
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
    ? images.value.flatMap((rawUrl, index) => {
        const url = safeImageUrl(rawUrl);
        return url === undefined
          ? []
          : [{ id: `${messageId}-image-${String(index)}`, name: imageName(url), mediaType: 'image/*', size: 0, kind: 'image', url }];
      })
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
      const decoded = decodeChatMessage(notification.params, appStore.get(currentSessionAtom));
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
  const url = new URL(sanitizeRpcUrl(config.url));
  return url.toString();
};

const websocketProtocols = (config: RpcConfig): string[] => {
  if (config.token.length === 0) {
    return [];
  }
  return ['cosmobot-rpc', `cosmobot-token.${base64Url(config.token)}`];
};

const base64Url = (value: string): string =>
  btoa(value).replaceAll('+', '-').replaceAll('/', '_').replace(/=+$/, '');

const sanitizeRpcUrl = (rawUrl: string): string => {
  const url = new URL(rawUrl, window.location.href);
  removeAccessTokenParams(url);
  return url.toString();
};

const attachmentFetchUrl = (config: RpcConfig, attachment: ChatAttachment): URL => {
  const rpcUrl = new URL(config.url);
  const protocol = rpcUrl.protocol === 'wss:' ? 'https:' : rpcUrl.protocol === 'ws:' ? 'http:' : window.location.protocol;
  const base = `${protocol}//${rpcUrl.host}`;
  const rawUrl = attachment.url ?? `/attachments/${encodeURIComponent(attachment.id)}`;
  const url = new URL(rawUrl, base);
  const rpcOrigin = new URL(base).origin;
  if (url.origin !== rpcOrigin || !url.pathname.startsWith('/attachments/')) {
    throw new Error('Attachment URL is outside the RPC attachment endpoint');
  }
  removeAccessTokenParams(url);
  return url;
};

const fetchAttachmentBlob = async (attachment: ChatAttachment): Promise<Blob> => {
  const config = loadConfig();
  const url = attachmentFetchUrl(config, attachment);
  const response = await fetch(url.toString(), {
    credentials: 'omit',
    headers: config.token.length > 0 ? { Authorization: `Bearer ${config.token}` } : {}
  });
  if (!response.ok) {
    throw new Error(`Attachment request failed: ${String(response.status)}`);
  }
  return response.blob();
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
  const id = firstString(record.value, ['attachmentId', 'attachment_id', 'id']);
  if (id === undefined) {
    return err(`${path}.attachmentId must be present`);
  }
  const mediaType = firstString(record.value, ['mediaType', 'media_type', 'contentType', 'content_type']) ?? 'application/octet-stream';
  const attachment: ChatAttachment = {
    id,
    name: firstString(record.value, ['name', 'filename', 'fileName']) ?? id,
    mediaType,
    size: firstNumber(record.value, ['size', 'byteSize', 'byte_size']) ?? 0,
    kind: attachmentKind(mediaType)
  };
  addOptional(attachment, 'url', safeAttachmentUrl(firstString(record.value, ['url', 'href']), id));
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

const fileToBase64 = async (file: File): Promise<string> => {
  const buffer = await file.arrayBuffer();
  let binary = '';
  for (const byte of new Uint8Array(buffer)) {
    binary += String.fromCharCode(byte);
  }
  return btoa(binary);
};

const safeAttachmentUrl = (rawUrl: string | undefined, attachmentId: string): string => {
  const url = new URL(rawUrl ?? `/attachments/${encodeURIComponent(attachmentId)}`, window.location.origin);
  removeAccessTokenParams(url);
  if (url.origin === window.location.origin && url.pathname.startsWith('/attachments/')) {
    return `${url.pathname}${url.search}${url.hash}`;
  }
  return `/attachments/${encodeURIComponent(attachmentId)}`;
};

const safeImageUrl = (rawUrl: string): string | undefined => {
  try {
    const url = new URL(rawUrl);
    if (url.protocol !== 'https:') {
      return undefined;
    }
    removeAccessTokenParams(url);
    return url.toString();
  } catch {
    return undefined;
  }
};

const imageName = (url: string): string => {
  const pathName = new URL(url).pathname;
  return pathName.split('/').filter(Boolean).at(-1) ?? 'image';
};

const removeAccessTokenParams = (url: URL): void => {
  for (const key of Array.from(url.searchParams.keys())) {
    if (key.toLowerCase() === 'access_token') {
      url.searchParams.delete(key);
    }
  }
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
