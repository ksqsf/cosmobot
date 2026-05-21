import { describe, expect, it } from 'vitest';
import { attachmentKind, decodeChatMessage, decodeIncomingRpcMessage } from './rpc';

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
    const decoded = decodeChatMessage({
      session_id: 'stable-session',
      message_id: 'stable-message',
      role: 'assistant',
      content: 'stream text',
      image_urls: ['https://example.test/image.png'],
      attachments: [{ id: 'file-1', filename: 'notes.txt', content_type: 'text/plain', size: 12 }]
    });

    expect(decoded.ok).toBe(true);
    if (decoded.ok) {
      expect(decoded.value.sessionId).toBe('stable-session');
      expect(decoded.value.messageId).toBe('stable-message');
      expect(decoded.value.attachments).toHaveLength(2);
    }
  });

  it('classifies attachment media types', () => {
    expect(attachmentKind('image/png')).toBe('image');
    expect(attachmentKind('audio/mpeg')).toBe('audio');
    expect(attachmentKind('application/pdf')).toBe('file');
  });
});
