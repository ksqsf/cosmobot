import { get, writable } from 'svelte/store';
import type { AuditEvent, ChatMessage, ConnectionStatus, EventLogEntry, SessionSummary } from './types';

export const connection = writable<{ status: ConnectionStatus; message: string }>({
  status: 'disconnected',
  message: 'Disconnected'
});

export const sessions = writable<SessionSummary[]>([]);
export const currentSession = writable<string>('');
export const messages = writable<ChatMessage[]>([]);
export const auditEvents = writable<AuditEvent[]>([]);
export const selectedAudit = writable<string | null>(null);
export const eventLog = writable<EventLogEntry[]>([]);

let nextLogId = 1;

export const addLog = (text: string): void => {
  const entry: EventLogEntry = {
    id: nextLogId,
    at: new Date().toLocaleTimeString(),
    text
  };
  nextLogId += 1;
  eventLog.update((rows) => [...rows, entry].slice(-120));
};

export const setCurrentSession = (sessionId: string): void => {
  currentSession.set(sessionId);
};

export const setHistory = (history: ChatMessage[]): void => {
  messages.set(history);
};

export const mergeMessage = (message: ChatMessage): void => {
  if (message.sessionId !== '' && get(currentSession) !== '' && message.sessionId !== get(currentSession)) {
    return;
  }
  if (get(currentSession) === '' && message.sessionId !== '') {
    currentSession.set(message.sessionId);
  }
  messages.update((rows) => {
    const index = rows.findIndex((row) => row.messageId === message.messageId);
    if (index === -1) {
      return [...rows, message];
    }
    const copy = [...rows];
    const existing = copy[index];
    if (existing !== undefined) {
      copy[index] = { ...existing, ...message };
    }
    return copy;
  });
};

export const addAuditEvent = (event: AuditEvent): void => {
  auditEvents.update((rows) => [event, ...rows.filter((row) => row.id !== event.id)].slice(0, 200));
};
