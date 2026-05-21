<script lang="ts">
  import { onMount } from 'svelte';
  import { auditEvents, connection, currentSession, eventLog, messages, selectedAudit, sessions } from './lib/stores';
  import { attachmentKind, attachmentSizeError, connectRpc, deleteSession, disconnectRpc, forkFrom, loadConfig, loadHistory, openSession, refreshAudit, refreshSessions, renameSession, sendChat, subscribeAudit, toError, uploadAttachment } from './lib/rpc';
  import type { QueuedAttachment, RpcConfig } from './lib/types';

  let config: RpcConfig = { url: '', token: '' };
  let newSessionLabel = 'browser';
  let draft = '';
  let renameDraft = '';
  let queue: QueuedAttachment[] = [];
  let busy = false;
  let composerError = '';
  let dragging = false;

  onMount(() => {
    config = loadConfig();
  });

  $: current = $sessions.find((session) => session.id === $currentSession);
  $: renameDraft = current?.title ?? '';
  $: selectedAuditEvent = $auditEvents.find((event) => event.id === $selectedAudit) ?? null;
  $: selectedAuditText = selectedAuditEvent === null ? 'Select an event.' : pretty(selectedAuditEvent.payload);
  $: canSend = $connection.status === 'connected' && $currentSession !== '' && draft.trim().length > 0 && !busy;

  const connect = (): void => {
    try {
      connectRpc(config);
    } catch (error) {
      composerError = toError(error).message;
    }
  };

  const createSession = (): void => {
    busy = true;
    composerError = '';
    void openSession(newSessionLabel.trim())
      .catch((error: unknown) => {
        composerError = toError(error).message;
      })
      .finally(() => {
        busy = false;
      });
  };

  const selectSession = (sessionId: string): void => {
    void loadHistory(sessionId);
  };

  const submit = (): void => {
    const text = draft.trim();
    if (!canSend || text.length === 0) {
      return;
    }
    busy = true;
    composerError = '';
    void uploadQueued()
      .then((uploaded) => sendChat($currentSession, text, uploaded))
      .then(() => {
        draft = '';
        queue = queue.filter((item) => item.status === 'failed');
      })
      .catch((error: unknown) => {
        composerError = toError(error).message;
      })
      .finally(() => {
        busy = false;
      });
  };

  const uploadQueued = async (): Promise<QueuedAttachment[]> => {
    const uploaded: QueuedAttachment[] = [];
    for (const item of queue) {
      if (item.remote !== undefined) {
        uploaded.push(item);
        continue;
      }
      updateQueue(item.localId, { status: 'uploading' });
      try {
        const remote = await uploadAttachment(item);
        const next = { ...item, status: 'uploaded' as const, remote };
        updateQueue(item.localId, next);
        uploaded.push(next);
      } catch (error) {
        updateQueue(item.localId, {
          status: 'failed',
          error: `Not attached: ${toError(error).message}`
        });
      }
    }
    if (queue.length > uploaded.length) {
      composerError = 'Message sent without one or more attachments.';
    }
    return uploaded;
  };

  const updateQueue = (localId: string, patch: Partial<QueuedAttachment>): void => {
    queue = queue.map((item) => (item.localId === localId ? { ...item, ...patch } : item));
  };

  const addFiles = (files: FileList | File[]): void => {
    const additions: QueuedAttachment[] = [];
    for (const file of Array.from(files)) {
      const sizeError = attachmentSizeError(file.size);
      if (sizeError !== undefined) {
        composerError = sizeError;
        continue;
      }
      const mediaType = file.type || 'application/octet-stream';
      additions.push({
        localId: globalThis.crypto.randomUUID(),
        file,
        name: file.name,
        mediaType,
        size: file.size,
        kind: attachmentKind(mediaType),
        status: 'queued'
      });
    }
    queue = [...queue, ...additions];
  };

  const removeQueued = (localId: string): void => {
    queue = queue.filter((item) => item.localId !== localId);
  };

  const onFileInput = (event: Event): void => {
    const input = event.currentTarget as HTMLInputElement;
    if (input.files !== null) {
      addFiles(input.files);
      input.value = '';
    }
  };

  const onDrop = (event: DragEvent): void => {
    event.preventDefault();
    dragging = false;
    if (event.dataTransfer?.files !== undefined) {
      addFiles(event.dataTransfer.files);
    }
  };

  const onPaste = (event: ClipboardEvent): void => {
    if (event.clipboardData?.files !== undefined && event.clipboardData.files.length > 0) {
      addFiles(event.clipboardData.files);
    }
  };

  const runFork = (messageId: string): void => {
    if ($currentSession === '') {
      return;
    }
    void forkFrom($currentSession, messageId).catch((error: unknown) => {
      composerError = toError(error).message;
    });
  };

  const runDeleteSession = (): void => {
    if ($currentSession === '') {
      return;
    }
    void deleteSession($currentSession).catch((error: unknown) => {
      composerError = toError(error).message;
    });
  };

  const runRename = (): void => {
    if ($currentSession === '' || renameDraft.trim().length === 0) {
      return;
    }
    void renameSession($currentSession, renameDraft.trim()).catch((error: unknown) => {
      composerError = toError(error).message;
    });
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

  const pretty = (value: unknown): string => JSON.stringify(value, null, 2);
</script>

<main class="app-shell">
  <aside class="sidebar" aria-label="Sessions">
    <div class="brand-row">
      <h1>cosmobot</h1>
      <span class:connected={$connection.status === 'connected'} class:error={$connection.status === 'error'}>{$connection.message}</span>
    </div>

    <div class="connection-panel">
      <label>
        RPC URL
        <input bind:value={config.url} autocomplete="off" spellcheck="false" />
      </label>
      <label>
        Token
        <input bind:value={config.token} autocomplete="off" spellcheck="false" type="password" />
      </label>
      <div class="button-row">
        <button class="primary" type="button" on:click={connect} disabled={$connection.status === 'connecting'}>Connect</button>
        <button type="button" on:click={disconnectRpc}>Disconnect</button>
      </div>
    </div>

    <div class="new-session">
      <input bind:value={newSessionLabel} aria-label="New session label" placeholder="New session label" />
      <button type="button" on:click={createSession} disabled={$connection.status !== 'connected' || busy}>New</button>
    </div>

    <div class="session-actions">
      <button type="button" on:click={() => { void refreshSessions(); }} disabled={$connection.status !== 'connected'}>Refresh</button>
      <button type="button" on:click={runDeleteSession} disabled={$currentSession === ''}>Delete</button>
    </div>

    <nav class="session-list">
      {#if $sessions.length === 0}
        <p class="empty">No durable sessions loaded.</p>
      {:else}
        {#each $sessions as session (session.id)}
          <button class:active={session.id === $currentSession} type="button" on:click={() => { selectSession(session.id); }}>
            <strong>{session.title}</strong>
            <span>{session.updatedAt ?? session.id}</span>
          </button>
        {/each}
      {/if}
    </nav>
  </aside>

  <section class="chat-column" aria-label="Chat transcript">
    <header class="chat-header">
      <div>
        <h2>{current?.title ?? 'No session selected'}</h2>
        <p>{$currentSession || 'Create or select a session to begin.'}</p>
      </div>
      <div class="rename-row">
        <input bind:value={renameDraft} aria-label="Session title" disabled={$currentSession === ''} />
        <button type="button" on:click={runRename} disabled={$currentSession === ''}>Rename</button>
      </div>
    </header>

    <div class="transcript">
      {#if $messages.length === 0}
        <div class="empty-state">No messages in this branch.</div>
      {:else}
        {#each $messages as message (message.messageId)}
          <article class:from-user={message.role === 'user'} class:streaming={message.streaming} class="message">
            <div class="message-meta">
              <span>{message.role}</span>
              <span>{message.messageId}</span>
            </div>
            <div class="message-text">{message.text}</div>
            {#if message.attachments.length > 0}
              <div class="attachment-grid">
                {#each message.attachments as attachment (attachment.id)}
                  <a class="attachment" href={attachment.url} target="_blank" rel="noreferrer">
                    <span>{attachment.kind}</span>
                    <strong>{attachment.name}</strong>
                  </a>
                {/each}
              </div>
            {/if}
            <div class="message-actions">
              <button type="button" on:click={() => { runFork(message.messageId); }}>Fork</button>
            </div>
          </article>
        {/each}
      {/if}
    </div>

    <form class:dragging class="composer" on:submit|preventDefault={submit} on:drop={onDrop} on:dragover|preventDefault={() => { dragging = true; }} on:dragleave={() => { dragging = false; }} on:paste={onPaste}>
      {#if queue.length > 0}
        <div class="queue">
          {#each queue as item (item.localId)}
            <div class="queue-item">
              <span>{item.kind}</span>
              <strong>{item.name}</strong>
              <small>{formatSize(item.size)} · {item.status}</small>
              {#if item.error !== undefined}
                <em>{item.error}</em>
              {/if}
              <button type="button" on:click={() => { removeQueued(item.localId); }}>Remove</button>
            </div>
          {/each}
        </div>
      {/if}
      <textarea bind:value={draft} placeholder="Message cosmobot" rows="4"></textarea>
      <div class="composer-footer">
        <label class="file-button">
          Attach
          <input type="file" multiple on:change={onFileInput} />
        </label>
        <span>{composerError}</span>
        <button class="primary" type="submit" disabled={!canSend}>Send</button>
      </div>
    </form>
  </section>

  <aside class="audit-panel" aria-label="Audit and events">
    <header>
      <h2>Audit</h2>
      <div class="button-row">
        <button type="button" on:click={() => { void refreshAudit(); }} disabled={$connection.status !== 'connected'}>Load</button>
        <button type="button" on:click={() => { void subscribeAudit(); }} disabled={$connection.status !== 'connected'}>Live</button>
      </div>
    </header>
    <div class="audit-list">
      {#if $auditEvents.length === 0}
        <p class="empty">No audit or run events.</p>
      {:else}
        {#each $auditEvents as event (event.id)}
          <button class:active={event.id === $selectedAudit} type="button" on:click={() => { selectedAudit.set(event.id); }}>
            <strong>{event.title}</strong>
            <span>{event.subtitle || event.id}</span>
          </button>
        {/each}
      {/if}
    </div>
    <pre class="audit-detail">{selectedAuditText}</pre>
    <section class="event-log" aria-label="RPC event log">
      {#each $eventLog as entry (entry.id)}
        <div>{entry.at} {entry.text}</div>
      {/each}
    </section>
  </aside>
</main>
