import { describe, expect, it, beforeEach } from 'vitest';
import { addAuditEvent, addLog, appStore, auditEventsAtom, currentSessionAtom, eventLogAtom, mergeMessage, messagesAtom, resetClientState, setCurrentSession, setHistory } from './stores';

describe('client state transitions', () => {
  beforeEach(() => {
    resetClientState();
  });

  it('adopts the first incoming chat session and merges updates by message id', () => {
    mergeMessage({
      sessionId: 'session-a',
      messageId: 'rpc-1',
      role: 'assistant',
      text: 'hel',
      attachments: [],
      streaming: true
    });
    mergeMessage({
      sessionId: 'session-a',
      messageId: 'rpc-1',
      role: 'assistant',
      text: 'hello',
      attachments: [],
      streaming: false
    });

    expect(appStore.get(currentSessionAtom)).toBe('session-a');
    expect(appStore.get(messagesAtom)).toEqual([
      {
        sessionId: 'session-a',
        messageId: 'rpc-1',
        role: 'assistant',
        text: 'hello',
        attachments: [],
        streaming: false
      }
    ]);
  });

  it('ignores late messages for a different selected session', () => {
    setCurrentSession('session-a');
    setHistory([]);

    mergeMessage({
      sessionId: 'session-b',
      messageId: 'late',
      role: 'assistant',
      text: 'late update',
      attachments: [],
      streaming: false
    });

    expect(appStore.get(messagesAtom)).toHaveLength(0);
  });

  it('deduplicates audit events and keeps the newest payload', () => {
    addAuditEvent({ id: 'audit-1', title: 'tool old', subtitle: '', payload: { step: 1 } });
    addAuditEvent({ id: 'audit-1', title: 'tool new', subtitle: '', payload: { step: 2 } });

    expect(appStore.get(auditEventsAtom)).toEqual([{ id: 'audit-1', title: 'tool new', subtitle: '', payload: { step: 2 } }]);
  });

  it('keeps a bounded event log', () => {
    for (let index = 0; index < 130; index += 1) {
      addLog(`event ${String(index)}`);
    }

    expect(appStore.get(eventLogAtom)).toHaveLength(120);
    expect(appStore.get(eventLogAtom)[0]?.text).toBe('event 10');
  });
});
