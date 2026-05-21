import { get } from 'svelte/store';
import { beforeEach, describe, expect, it } from 'vitest';
import { auditEvents, currentSession, eventLog, messages, addAuditEvent, addLog, mergeMessage, setCurrentSession, setHistory } from './stores';

describe('client state transitions', () => {
  beforeEach(() => {
    currentSession.set('');
    messages.set([]);
    auditEvents.set([]);
    eventLog.set([]);
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

    expect(get(currentSession)).toBe('session-a');
    expect(get(messages)).toEqual([
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

    expect(get(messages)).toHaveLength(0);
  });

  it('deduplicates audit events and keeps the newest payload', () => {
    addAuditEvent({ id: 'audit-1', title: 'tool old', subtitle: '', payload: { step: 1 } });
    addAuditEvent({ id: 'audit-1', title: 'tool new', subtitle: '', payload: { step: 2 } });

    expect(get(auditEvents)).toEqual([{ id: 'audit-1', title: 'tool new', subtitle: '', payload: { step: 2 } }]);
  });

  it('keeps a bounded event log', () => {
    for (let index = 0; index < 130; index += 1) {
      addLog(`event ${String(index)}`);
    }

    expect(get(eventLog)).toHaveLength(120);
    expect(get(eventLog)[0]?.text).toBe('event 10');
  });
});
