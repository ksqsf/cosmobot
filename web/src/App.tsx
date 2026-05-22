import * as Tabs from '@radix-ui/react-tabs';
import { useAtom, useAtomValue } from 'jotai';
import {
  Archive,
  Bot,
  Download,
  GitFork,
  ImageIcon,
  Loader2,
  MessageSquare,
  Mic,
  Paperclip,
  Plus,
  RefreshCcw,
  Save,
  Send,
  Settings,
  Trash2,
  Unplug,
  X
} from 'lucide-react';
import React, { useEffect, useMemo, useRef, useState } from 'react';
import ReactMarkdown from 'react-markdown';
import type { Components } from 'react-markdown';
import rehypeSanitize from 'rehype-sanitize';
import remarkGfm from 'remark-gfm';
import {
  attachmentKind,
  attachmentSizeError,
  canRenderInlineImage,
  connectRpc,
  deleteSession,
  disconnectRpc,
  fetchAttachmentObjectUrl,
  forkFrom,
  loadConfig,
  loadHistory,
  openAttachment,
  openSession,
  refreshAudit,
  refreshSessions,
  renameSession,
  saveConfig,
  sendChat,
  subscribeAudit,
  toError,
  uploadAttachment
} from './lib/rpc';
import {
  auditEventsAtom,
  connectionAtom,
  currentSessionAtom,
  eventLogAtom,
  messagesAtom,
  selectedAuditAtom,
  sessionsAtom
} from './lib/stores';
import type { AttachmentKind, ChatAttachment, ChatMessage, QueuedAttachment, RpcConfig, SessionSummary } from './lib/types';

type View = 'dialog' | 'audit' | 'settings';

const defaultNewConversationLabel = 'chat';

const App = (): React.JSX.Element => {
  const [view, setView] = useState<View>('dialog');
  const connection = useAtomValue(connectionAtom);
  const sessions = useAtomValue(sessionsAtom);
  const currentSession = useAtomValue(currentSessionAtom);
  const messages = useAtomValue(messagesAtom);
  const [config, setConfig] = useState<RpcConfig>(() => loadConfig());
  const [draft, setDraft] = useState('');
  const [queue, setQueue] = useState<QueuedAttachment[]>([]);
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState('');
  const current = useMemo(() => sessions.find((session) => session.id === currentSession), [currentSession, sessions]);

  useEffect(() => {
    if (config.token.length > 0) {
      connectRpc(config);
    }
  }, []);

  const runConnected = (action: () => Promise<void>): void => {
    setError('');
    void action().catch((caught: unknown) => {
      setError(toError(caught).message);
    });
  };

  const connect = (): void => {
    try {
      connectRpc(config);
    } catch (caught) {
      setError(toError(caught).message);
    }
  };

  const createConversation = async (): Promise<void> => {
    setBusy(true);
    try {
      await openSession(defaultNewConversationLabel);
      setDraft('');
      setQueue([]);
    } finally {
      setBusy(false);
    }
  };

  const selectConversation = async (sessionId: string): Promise<void> => {
    await loadHistory(sessionId);
  };

  const submit = async (): Promise<void> => {
    const text = draft.trim();
    if (connection.status !== 'connected' || busy || (text.length === 0 && queue.length === 0)) {
      return;
    }
    setBusy(true);
    setError('');
    try {
      const uploaded = await uploadQueued(queue, setQueue);
      if (text.length === 0 && uploaded.length === 0) {
        setError('Attach at least one file successfully or enter a message.');
        return;
      }
      const sessionId = currentSession === '' ? await openSession(defaultNewConversationLabel) : currentSession;
      await sendChat(sessionId, text, uploaded);
      setDraft('');
      setQueue((items) => items.filter((item) => item.status === 'failed'));
      if (uploaded.length < queue.length) {
        setError('Message sent without one or more attachments.');
      }
    } catch (caught) {
      setError(toError(caught).message);
    } finally {
      setBusy(false);
    }
  };

  const addFiles = (files: FileList | File[]): void => {
    const additions: QueuedAttachment[] = [];
    for (const file of Array.from(files)) {
      const sizeError = attachmentSizeError(file.size);
      if (sizeError !== undefined) {
        setError(sizeError);
        continue;
      }
      const mediaType = file.type || 'application/octet-stream';
      additions.push({
        localId: crypto.randomUUID(),
        file,
        name: file.name,
        mediaType,
        size: file.size,
        kind: attachmentKind(mediaType),
        status: 'queued'
      });
    }
    setQueue((items) => [...items, ...additions]);
  };

  const removeQueued = (localId: string): void => {
    setQueue((items) => items.filter((item) => item.localId !== localId));
  };

  const forkMessage = async (messageId: string): Promise<void> => {
    if (currentSession === '') {
      return;
    }
    await forkFrom(currentSession, messageId);
  };

  const canSend = connection.status === 'connected' && !busy && (draft.trim().length > 0 || queue.length > 0);

  return (
    <Tabs.Root
      value={view}
      onValueChange={(next) => {
        if (next === 'dialog' || next === 'audit' || next === 'settings') {
          setView(next);
        }
      }}
      className="app-shell"
    >
      <header className="topbar">
        <div className="brand">
          <Bot aria-hidden="true" size={22} />
          <div>
            <h1>cosmobot</h1>
            <span className={connection.status}>{connection.message}</span>
          </div>
        </div>
        <Tabs.List className="view-switcher" aria-label="Function switcher">
          <Tabs.Trigger value="dialog">
            <MessageSquare aria-hidden="true" size={16} />
            Dialog
          </Tabs.Trigger>
          <Tabs.Trigger value="audit">
            <Archive aria-hidden="true" size={16} />
            Agent Audit
          </Tabs.Trigger>
          <Tabs.Trigger value="settings">
            <Settings aria-hidden="true" size={16} />
            Settings
          </Tabs.Trigger>
        </Tabs.List>
      </header>

      <Tabs.Content value="dialog" className="view dialog-view">
        <ConversationSidebar
          busy={busy}
          connectionStatus={connection.status}
          currentSession={currentSession}
          sessions={sessions}
          onCreate={() => {
            runConnected(createConversation);
          }}
          onDelete={() => {
            runConnected(async () => {
              if (currentSession !== '') {
                await deleteSession(currentSession);
              }
            });
          }}
          onRefresh={() => {
            runConnected(refreshSessions);
          }}
          onSelect={(sessionId) => {
            runConnected(async () => selectConversation(sessionId));
          }}
        />
        <main className="chat-column" aria-label="Dialog">
          <ChatHeader
            current={current}
            currentSession={currentSession}
            onRename={(title) => {
              runConnected(async () => {
                if (currentSession !== '') {
                  await renameSession(currentSession, title);
                }
              });
            }}
          />
          <Transcript
            messages={messages}
            onFork={(messageId) => {
              runConnected(async () => forkMessage(messageId));
            }}
            onError={setError}
          />
          <Composer
            busy={busy}
            canSend={canSend}
            draft={draft}
            error={error}
            queue={queue}
            onAddFiles={addFiles}
            onDraftChange={setDraft}
            onRemoveQueued={removeQueued}
            onSubmit={() => {
              void submit();
            }}
          />
        </main>
      </Tabs.Content>

      <Tabs.Content value="audit" className="view audit-view">
        <AgentAudit />
      </Tabs.Content>

      <Tabs.Content value="settings" className="view settings-view">
        <SettingsView config={config} onConfigChange={setConfig} onConnect={connect} />
      </Tabs.Content>
    </Tabs.Root>
  );
};

type SettingsViewProps = {
  config: RpcConfig;
  onConfigChange: (config: RpcConfig) => void;
  onConnect: () => void;
};

const SettingsView = ({ config, onConfigChange, onConnect }: SettingsViewProps): React.JSX.Element => {
  const connection = useAtomValue(connectionAtom);
  const [message, setMessage] = useState('');

  const save = (): void => {
    try {
      saveConfig(config);
      setMessage('Settings saved locally.');
    } catch (caught) {
      setMessage(toError(caught).message);
    }
  };

  return (
    <main className="settings-shell" aria-label="Settings">
      <section className="settings-panel">
        <header>
          <h2>Settings</h2>
          <span className={connection.status}>{connection.message}</span>
        </header>
        <div className="settings-form">
          <label>
            <span>RPC URL</span>
            <input
              value={config.url}
              autoComplete="off"
              spellCheck={false}
              onChange={(event) => {
                onConfigChange({ ...config, url: event.currentTarget.value });
                setMessage('');
              }}
            />
          </label>
          <label>
            <span>Access token</span>
            <input
              value={config.token}
              autoComplete="current-password"
              spellCheck={false}
              type="password"
              onChange={(event) => {
                onConfigChange({ ...config, token: event.currentTarget.value });
                setMessage('');
              }}
            />
          </label>
          <div className="settings-actions">
            <button
              className="primary"
              type="button"
              onClick={() => {
                save();
              }}
            >
              <Save aria-hidden="true" size={16} />
              Save
            </button>
            <button
              type="button"
              onClick={() => {
                onConnect();
              }}
              disabled={connection.status === 'connecting'}
            >
              {connection.status === 'connecting' ? <Loader2 aria-hidden="true" size={16} className="spin" /> : <Settings aria-hidden="true" size={16} />}
              Connect
            </button>
            <button type="button" onClick={disconnectRpc}>
              <Unplug aria-hidden="true" size={16} />
              Disconnect
            </button>
          </div>
          {message.length > 0 ? <p className="settings-message">{message}</p> : null}
        </div>
      </section>
    </main>
  );
};

type ConversationSidebarProps = {
  busy: boolean;
  connectionStatus: string;
  currentSession: string;
  sessions: SessionSummary[];
  onCreate: () => void;
  onDelete: () => void;
  onRefresh: () => void;
  onSelect: (sessionId: string) => void;
};

const ConversationSidebar = ({ busy, connectionStatus, currentSession, sessions, onCreate, onDelete, onRefresh, onSelect }: ConversationSidebarProps): React.JSX.Element => (
  <aside className="conversation-sidebar" aria-label="Conversations">
    <div className="sidebar-actions">
      <button className="primary action-button" type="button" onClick={onCreate} disabled={connectionStatus !== 'connected' || busy}>
        <Plus aria-hidden="true" size={16} />
        New conversation
      </button>
      <button className="icon-button" type="button" onClick={onRefresh} disabled={connectionStatus !== 'connected'} aria-label="Refresh conversations">
        <RefreshCcw aria-hidden="true" size={16} />
      </button>
      <button className="icon-button danger" type="button" onClick={onDelete} disabled={currentSession === ''} aria-label="Delete conversation">
        <Trash2 aria-hidden="true" size={16} />
      </button>
    </div>
    <nav className="conversation-list">
      {sessions.length === 0 ? (
        <p className="empty">No conversations loaded.</p>
      ) : (
        sessions.map((session) => (
          <button
            className={session.id === currentSession ? 'active' : ''}
            key={session.id}
            type="button"
            onClick={() => {
              onSelect(session.id);
            }}
          >
            <strong>{session.title}</strong>
            <span>{session.updatedAt ?? session.id}</span>
          </button>
        ))
      )}
    </nav>
  </aside>
);

type ChatHeaderProps = {
  current: SessionSummary | undefined;
  currentSession: string;
  onRename: (title: string) => void;
};

const ChatHeader = ({ current, currentSession, onRename }: ChatHeaderProps): React.JSX.Element => {
  const [title, setTitle] = useState(current?.title ?? '');
  useEffect(() => {
    setTitle(current?.title ?? '');
  }, [current?.title]);
  return (
    <header className="chat-header">
      <div>
        <h2>{current?.title ?? 'New dialog'}</h2>
        <p>{currentSession || 'Start typing to create a conversation.'}</p>
      </div>
      <form
        className="rename-form"
        onSubmit={(event) => {
          event.preventDefault();
          if (title.trim().length > 0) {
            onRename(title.trim());
          }
        }}
      >
        <input
          value={title}
          aria-label="Conversation title"
          disabled={currentSession === ''}
          onChange={(event) => {
            setTitle(event.currentTarget.value);
          }}
        />
        <button type="submit" disabled={currentSession === ''}>
          Rename
        </button>
      </form>
    </header>
  );
};

type TranscriptProps = {
  messages: ChatMessage[];
  onError: (message: string) => void;
  onFork: (messageId: string) => void;
};

const Transcript = ({ messages, onError, onFork }: TranscriptProps): React.JSX.Element => {
  const bottomRef = useRef<HTMLDivElement | null>(null);
  useEffect(() => {
    bottomRef.current?.scrollIntoView({ block: 'end' });
  }, [messages]);
  return (
    <section className="transcript" aria-label="Transcript">
      {messages.length === 0 ? (
        <div className="empty-state">
          <MessageSquare aria-hidden="true" size={34} />
          <p>Send a message to begin.</p>
        </div>
      ) : (
        messages.map((message) => <ChatMessageRow key={message.messageId} message={message} onError={onError} onFork={onFork} />)
      )}
      <div ref={bottomRef} />
    </section>
  );
};

type ChatMessageRowProps = {
  message: ChatMessage;
  onError: (message: string) => void;
  onFork: (messageId: string) => void;
};

const ChatMessageRow = ({ message, onError, onFork }: ChatMessageRowProps): React.JSX.Element => (
  <article className={`message ${message.role === 'user' ? 'from-user' : 'from-assistant'} ${message.streaming ? 'streaming' : ''}`}>
    <div className="message-body">
      <div className="message-author">{message.role === 'user' ? 'You' : message.role}</div>
      <div className="markdown">
        <ReactMarkdown components={markdownComponents} remarkPlugins={[remarkGfm]} rehypePlugins={[rehypeSanitize]}>
          {message.text}
        </ReactMarkdown>
      </div>
      {message.attachments.length > 0 ? <AttachmentGrid attachments={message.attachments} onError={onError} /> : null}
      <div className="message-actions">
        <button
          className="small-icon-button"
          type="button"
          onClick={() => {
            onFork(message.messageId);
          }}
          aria-label={`Fork from ${message.messageId}`}
        >
          <GitFork aria-hidden="true" size={14} />
        </button>
      </div>
    </div>
  </article>
);

type AttachmentGridProps = {
  attachments: ChatAttachment[];
  onError: (message: string) => void;
};

const AttachmentGrid = ({ attachments, onError }: AttachmentGridProps): React.JSX.Element => (
  <div className="attachment-grid">
    {attachments.map((attachment) =>
      canRenderInlineImage(attachment) ? (
        <InlineImageAttachment attachment={attachment} key={attachment.id} onError={onError} />
      ) : (
        <AttachmentChip attachment={attachment} key={attachment.id} onError={onError} />
      )
    )}
  </div>
);

type InlineImageAttachmentProps = {
  attachment: ChatAttachment;
  onError: (message: string) => void;
};

const InlineImageAttachment = ({ attachment, onError }: InlineImageAttachmentProps): React.JSX.Element => {
  const [src, setSrc] = useState<string | null>(null);
  useEffect(() => {
    const directUrl = directInlineImageUrl(attachment);
    if (directUrl !== null) {
      setSrc(directUrl);
      return () => {};
    }

    let live = true;
    let objectUrl: string | undefined;
    setSrc(null);
    void fetchAttachmentObjectUrl(attachment)
      .then((url) => {
        objectUrl = url;
        if (live) {
          setSrc(url);
        } else {
          URL.revokeObjectURL(url);
        }
      })
      .catch((caught: unknown) => {
        onError(toError(caught).message);
      });
    return () => {
      live = false;
      if (objectUrl !== undefined) {
        URL.revokeObjectURL(objectUrl);
      }
    };
  }, [attachment, onError]);
  return (
    <button
      className="image-attachment"
      type="button"
      onClick={() => {
        downloadAttachment(attachment, onError);
      }}
    >
      {src === null ? <ImageIcon aria-hidden="true" size={22} /> : <img src={src} alt={attachment.name} referrerPolicy="no-referrer" />}
      <span>{attachment.name}</span>
    </button>
  );
};

const directInlineImageUrl = (attachment: ChatAttachment): string | null => {
  if (attachment.url === undefined || !attachment.mediaType.toLowerCase().startsWith('image/')) {
    return null;
  }
  if (!/^https:\/\//iu.test(attachment.url)) {
    return null;
  }
  try {
    const url = new URL(attachment.url);
    if (url.protocol === 'https:') {
      for (const key of Array.from(url.searchParams.keys())) {
        if (key.toLowerCase() === 'access_token') {
          url.searchParams.delete(key);
        }
      }
      return url.toString();
    }
  } catch {
    return null;
  }
  return null;
};

type AttachmentChipProps = {
  attachment: ChatAttachment;
  onError: (message: string) => void;
};

const AttachmentChip = ({ attachment, onError }: AttachmentChipProps): React.JSX.Element => {
  const Icon = attachmentIcon(attachment.kind);
  return (
    <button
      className="attachment-chip"
      type="button"
      onClick={() => {
        downloadAttachment(attachment, onError);
      }}
    >
      <Icon aria-hidden="true" size={16} />
      <span>{attachment.name}</span>
      <Download aria-hidden="true" size={14} />
    </button>
  );
};

const downloadAttachment = (attachment: ChatAttachment, onError: (message: string) => void): void => {
  void openAttachment(attachment).catch((caught: unknown) => {
    onError(toError(caught).message);
  });
};

const attachmentIcon = (kind: AttachmentKind): typeof ImageIcon => {
  switch (kind) {
    case 'image':
      return ImageIcon;
    case 'audio':
      return Mic;
    case 'file':
      return Paperclip;
  }
};

type ComposerProps = {
  busy: boolean;
  canSend: boolean;
  draft: string;
  error: string;
  queue: QueuedAttachment[];
  onAddFiles: (files: FileList | File[]) => void;
  onDraftChange: (value: string) => void;
  onRemoveQueued: (localId: string) => void;
  onSubmit: () => void;
};

const Composer = ({ busy, canSend, draft, error, queue, onAddFiles, onDraftChange, onRemoveQueued, onSubmit }: ComposerProps): React.JSX.Element => {
  const [dragging, setDragging] = useState(false);
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  return (
    <form
      className={`composer ${dragging ? 'dragging' : ''}`}
      onSubmit={(event) => {
        event.preventDefault();
        onSubmit();
      }}
      onDragOver={(event) => {
        event.preventDefault();
        setDragging(true);
      }}
      onDragLeave={() => {
        setDragging(false);
      }}
      onDrop={(event) => {
        event.preventDefault();
        setDragging(false);
        onAddFiles(event.dataTransfer.files);
      }}
      onPaste={(event) => {
        if (event.clipboardData.files.length > 0) {
          onAddFiles(event.clipboardData.files);
        }
      }}
    >
      {queue.length > 0 ? (
        <div className="queue">
          {queue.map((item) => (
            <div className="queue-item" key={item.localId}>
              <span>{item.kind}</span>
              <strong>{item.name}</strong>
              <small>
                {formatSize(item.size)} · {item.status}
              </small>
              {item.error !== undefined ? <em>{item.error}</em> : null}
              <button
                className="small-icon-button"
                type="button"
                onClick={() => {
                  onRemoveQueued(item.localId);
                }}
                aria-label={`Remove ${item.name}`}
              >
                <X aria-hidden="true" size={14} />
              </button>
            </div>
          ))}
        </div>
      ) : null}
      <textarea
        value={draft}
        placeholder="Message cosmobot"
        rows={3}
        onChange={(event) => {
          onDraftChange(event.currentTarget.value);
        }}
      />
      <div className="composer-footer">
        <input
          ref={fileInputRef}
          className="hidden-input"
          type="file"
          multiple
          onChange={(event) => {
            if (event.currentTarget.files !== null) {
              onAddFiles(event.currentTarget.files);
              event.currentTarget.value = '';
            }
          }}
        />
        <button
          className="icon-button"
          type="button"
          onClick={() => {
            fileInputRef.current?.click();
          }}
          aria-label="Attach files"
        >
          <Paperclip aria-hidden="true" size={17} />
        </button>
        <span className="composer-error">{error}</span>
        <button className="primary send-button" type="submit" disabled={!canSend}>
          {busy ? <Loader2 aria-hidden="true" size={16} className="spin" /> : <Send aria-hidden="true" size={16} />}
          Send
        </button>
      </div>
    </form>
  );
};

const uploadQueued = async (queue: QueuedAttachment[], setQueue: React.Dispatch<React.SetStateAction<QueuedAttachment[]>>): Promise<QueuedAttachment[]> => {
  const uploaded: QueuedAttachment[] = [];
  for (const item of queue) {
    if (item.remote !== undefined) {
      uploaded.push(item);
      continue;
    }
    setQueue((items) => items.map((queued) => (queued.localId === item.localId ? { ...queued, status: 'uploading' } : queued)));
    try {
      const remote = await uploadAttachment(item);
      const next = { ...item, status: 'uploaded' as const, remote };
      setQueue((items) => items.map((queued) => (queued.localId === item.localId ? next : queued)));
      uploaded.push(next);
    } catch (caught) {
      const failed = { ...item, status: 'failed' as const, error: `Not attached: ${toError(caught).message}` };
      setQueue((items) => items.map((queued) => (queued.localId === item.localId ? failed : queued)));
    }
  }
  return uploaded;
};

const AgentAudit = (): React.JSX.Element => {
  const connection = useAtomValue(connectionAtom);
  const auditEvents = useAtomValue(auditEventsAtom);
  const eventLog = useAtomValue(eventLogAtom);
  const [selectedAudit, setSelectedAudit] = useAtom(selectedAuditAtom);
  const selected = auditEvents.find((event) => event.id === selectedAudit) ?? null;
  return (
    <main className="audit-shell" aria-label="Agent Audit">
      <section className="audit-list-panel">
        <header>
          <h2>Agent Audit</h2>
          <div className="button-row">
            <button
              type="button"
              onClick={() => {
                void refreshAudit();
              }}
              disabled={connection.status !== 'connected'}
            >
              <RefreshCcw aria-hidden="true" size={15} />
              Load
            </button>
            <button
              type="button"
              onClick={() => {
                void subscribeAudit();
              }}
              disabled={connection.status !== 'connected'}
            >
              Live
            </button>
          </div>
        </header>
        <nav className="audit-list">
          {auditEvents.length === 0 ? (
            <p className="empty">No audit or run events.</p>
          ) : (
            auditEvents.map((event) => (
              <button
                className={event.id === selectedAudit ? 'active' : ''}
                key={event.id}
                type="button"
                onClick={() => {
                  setSelectedAudit(event.id);
                }}
              >
                <strong>{event.title}</strong>
                <span>{event.subtitle || event.id}</span>
              </button>
            ))
          )}
        </nav>
      </section>
      <section className="audit-detail-panel">
        <pre>{selected === null ? 'Select an event.' : JSON.stringify(selected.payload, null, 2)}</pre>
        <div className="event-log">
          {eventLog.map((entry) => (
            <div key={entry.id}>
              {entry.at} {entry.text}
            </div>
          ))}
        </div>
      </section>
    </main>
  );
};

const formatSize = (size: number): string => {
  if (size < 1024) {
    return `${String(size)} B`;
  }
  if (size < 1024 * 1024) {
    return `${String(Math.round(size / 1024))} KB`;
  }
  return `${(size / 1024 / 1024).toFixed(1)} MB`;
};

const markdownComponents: Components = {
  img: () => null,
  a: ({ children, href }) => (
    <a href={href} rel="noreferrer" target="_blank">
      {children}
    </a>
  )
};

export default App;
