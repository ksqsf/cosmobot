import { describe, expect, it } from 'vitest';
import { attachmentKind, attachmentParam, attachmentSizeError, decodeChatMessage, decodeIncomingRpcMessage, maxAttachmentBytes, saveConfig } from './rpc';

describe('runtime RPC decoders', () => {
  it('decodes chat notifications', () => {
    const decoded = decodeIncomingRpcMessage({
      jsonrpc: '2.0',
      method: 'chat.message',
      params: { sessionId: 'browser-1', messageId: 'rpc-1', sender: 'user', text: 'hello' }
    });

    expect(decoded.ok).toBe(true);
    if (decoded.ok) {
      expect('method' in decoded.value && decoded.value.method).toBe('chat.message');
    }
  });

  it('normalizes current and planned chat message shapes', () => {
    saveConfig({ url: 'ws://localhost/rpc', token: 'secret-token' });
    const decoded = decodeChatMessage({
      session_id: 'stable-session',
      message_id: 'stable-message',
      role: 'assistant',
      content: 'stream text',
      image_urls: ['https://example.test/image.png'],
      attachments: [{ attachmentId: 'file-1', filename: 'notes.txt', content_type: 'text/plain', size: 12 }]
    });

    expect(decoded.ok).toBe(true);
    if (decoded.ok) {
      expect(decoded.value.sessionId).toBe('stable-session');
      expect(decoded.value.messageId).toBe('stable-message');
      expect(decoded.value.attachments).toHaveLength(2);
      expect(decoded.value.attachments[0]?.url).toBe('/attachments/file-1?access_token=secret-token');
    }
  });

  it('rejects JSON-RPC responses without exactly one result or error', () => {
    expect(decodeIncomingRpcMessage({ jsonrpc: '2.0', id: '1', result: null }).ok).toBe(true);
    expect(decodeIncomingRpcMessage({ jsonrpc: '2.0', id: '1', error: { message: 'failed' } }).ok).toBe(true);
    expect(decodeIncomingRpcMessage({ jsonrpc: '2.0', id: '1' }).ok).toBe(false);
    expect(decodeIncomingRpcMessage({ jsonrpc: '2.0', id: '1', result: null, error: { message: 'failed' } }).ok).toBe(false);
  });

  it('encodes attachment params with stable attachmentId and compatibility id', () => {
    expect(
      attachmentParam({
        id: 'att-1',
        name: 'notes.txt',
        mediaType: 'text/plain',
        size: 12,
        kind: 'file',
        url: '/attachments/att-1'
      })
    ).toEqual({ attachmentId: 'att-1', id: 'att-1', kind: 'file', name: 'notes.txt' });
  });

  it('guards large attachments before base64 conversion', () => {
    expect(attachmentSizeError(maxAttachmentBytes)).toBeUndefined();
    expect(attachmentSizeError(maxAttachmentBytes + 1)).toContain('too large');
    expect(attachmentSizeError(-1)).toBe('Attachment size is invalid');
  });

  it('classifies attachment media types', () => {
    expect(attachmentKind('image/png')).toBe('image');
    expect(attachmentKind('audio/mpeg')).toBe('audio');
    expect(attachmentKind('application/pdf')).toBe('file');
  });
});
