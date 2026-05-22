import { atom, createStore } from 'jotai/vanilla';
import type { AuditEvent, ChatMessage, ConnectionStatus, EventLogEntry, SessionSummary } from './types';

export const appStore = createStore();

export const connectionAtom = atom<{ status: ConnectionStatus; message: string }>({
  status: 'disconnected',
  message: 'Disconnected'
});

export const sessionsAtom = atom<SessionSummary[]>([]);
export const currentSessionAtom = atom<string>('');
export const messagesAtom = atom<ChatMessage[]>([]);
export const auditEventsAtom = atom<AuditEvent[]>([]);
export const selectedAuditAtom = atom<string | null>(null);
export const eventLogAtom = atom<EventLogEntry[]>([]);

let nextLogId = 1;

export const addLog = (text: string): void => {
  const entry: EventLogEntry = {
    id: nextLogId,
    at: new Date().toLocaleTimeString(),
    text
  };
  nextLogId += 1;
  appStore.set(eventLogAtom, [...appStore.get(eventLogAtom), entry].slice(-120));
};

export const setCurrentSession = (sessionId: string): void => {
  appStore.set(currentSessionAtom, sessionId);
};

export const setHistory = (history: ChatMessage[]): void => {
  appStore.set(messagesAtom, history);
};

export const setSessions = (rows: SessionSummary[]): void => {
  appStore.set(sessionsAtom, rows);
};

export const updateSessions = (update: (rows: SessionSummary[]) => SessionSummary[]): void => {
  appStore.set(sessionsAtom, update(appStore.get(sessionsAtom)));
};

export const mergeMessage = (message: ChatMessage): void => {
  const selectedSession = appStore.get(currentSessionAtom);
  if (message.sessionId !== '' && selectedSession !== '' && message.sessionId !== selectedSession) {
    return;
  }
  if (selectedSession === '' && message.sessionId !== '') {
    appStore.set(currentSessionAtom, message.sessionId);
  }
  appStore.set(
    messagesAtom,
    mergeMessageRows(appStore.get(messagesAtom), message)
  );
};

export const mergeMessageRows = (rows: ChatMessage[], message: ChatMessage): ChatMessage[] => {
  const index = rows.findIndex((row) => row.messageId === message.messageId);
  if (index === -1) {
    return [...rows, message];
  }
  return rows.map((row, rowIndex) => (rowIndex === index ? { ...row, ...message } : row));
};

export const addAuditEvent = (event: AuditEvent): void => {
  appStore.set(auditEventsAtom, [event, ...appStore.get(auditEventsAtom).filter((row) => row.id !== event.id)].slice(0, 200));
};

export const resetClientState = (): void => {
  appStore.set(currentSessionAtom, '');
  appStore.set(messagesAtom, []);
  appStore.set(sessionsAtom, []);
  appStore.set(auditEventsAtom, []);
  appStore.set(selectedAuditAtom, null);
  appStore.set(eventLogAtom, []);
  appStore.set(connectionAtom, { status: 'disconnected', message: 'Disconnected' });
  nextLogId = 1;
};
