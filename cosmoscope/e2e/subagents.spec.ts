import { expect, test } from '@playwright/test'

const methods = ['thread.run_transcript', 'thread.list', 'thread.get', 'thread.resolve_run', 'thread.active', 'thread.halt', 'audit.run', 'audit.thread', 'audit.subscribe']

test('thread inspector preserves the parent while exploring nested subagent audit', async ({ page }) => {
  const key = { platform: 'PlatformRPC', chatId: null, messageId: 'root' }
  const summary = { threadId: 1, latestPreview: 'Parent reply', rootKey: key, latestKey: key, chatDisplayName: null, nodeCount: 1, leafCount: 1 }
  const record = (id: number, event: unknown): unknown => ({ id, occurredAt: '2026-01-01T00:00:00Z', event })
  const spawn = (id: number, runId: string, childRunId: string, subagentId: string): unknown => record(id, { tag: 'SubAgentRunStarted', runId, childRunId, subagentId })
  const calls: string[] = []
  await page.routeWebSocket('ws://127.0.0.1:39999/rpc', (socket) => {
    socket.onMessage((message) => {
      const request = JSON.parse(String(message)) as { id: number, method: string, params?: { runId?: string } }
      calls.push(request.method)
      const result = request.method === 'admin.authenticate' ? { authenticated: true }
        : request.method === 'admin.capabilities' ? { serverVersion: 'test', methods, topics: [], permissions: [], features: {} }
          : request.method === 'thread.run_transcript' ? request.params?.runId === 'child-b' ? null : { messages: [{ role: 'user', content: 'Research this topic' }, { role: 'assistant', content: '', tool_calls: [{ id: 'call', type: 'function', function: { name: 'search', arguments: '{}' } }] }, { role: 'tool', tool_call_id: 'call', content: 'Found evidence' }, { role: 'assistant', content: 'Research complete' }] }
          : request.method === 'thread.list' ? { threads: [summary], total: 1, nodes: 1, leaves: 1, platforms: 1 }
            : request.method === 'thread.active' ? { threads: [] }
              : request.method === 'thread.get' ? { summary, nodes: [{ messageKey: key, inputMessageKey: null, parentMessageKey: null, messages: [{ role: 'assistant', content: 'Parent reply' }] }] }
                : request.method === 'thread.resolve_run' ? { threadId: request.params?.runId === 'child-b' ? 2 : null, taskId: null }
                  : request.method === 'audit.thread' ? [spawn(1, 'parent', 'child-a', 'Researcher'), record(2, { tag: 'AgentThreadLinked', runId: 'parent', linkedMessageId: 'root', linkedMessageKey: key, parentMessageId: null })]
                    : request.method === 'audit.run' ? request.params?.runId === 'child-a' ? [
                      spawn(3, 'child-a', 'child-b', 'Reviewer'),
                      record(4, { tag: 'ToolCallStarted', runId: 'child-a', turn: 1, toolCall: { id: 'call', name: 'search', arguments: '{}' } }),
                      record(5, { tag: 'ToolCallFinished', runId: 'child-a', turn: 1, toolCallId: 'call', toolName: 'search', status: 'ok', result: 'Found evidence', resultLength: 14, messageIds: [] }),
                      record(6, { tag: 'AgentRunFinished', runId: 'child-a', status: 'success', finalLength: 0, turnsUsed: 1 }),
                    ] : [] : {}
      socket.send(JSON.stringify({ jsonrpc: '2.0', id: request.id, result }))
    })
  })
  await page.goto('/login')
  await page.getByLabel('RPC endpoint').fill('ws://127.0.0.1:39999/rpc')
  await page.getByLabel('RPC token').fill('token')
  await page.getByRole('button', { name: 'Connect' }).click()
  await expect(page).not.toHaveURL(/\/login/)
  await page.goto('/threads/1')
  const panel = page.getByRole('region', { name: 'Subagents', exact: true })
  await panel.getByRole('button', { name: 'Researcher', exact: true }).click()
  await expect(panel.getByText('success', { exact: true })).toBeVisible()
  await expect(panel.getByText('Research this topic', { exact: true })).toBeVisible()
  await expect(panel.getByText('Found evidence', { exact: true })).toBeVisible()
  await expect(panel.locator('.thread-transcript > li strong')).toHaveText(['User', 'Assistant', 'Tool', 'Assistant'])
  await expect(panel.getByText('Research complete', { exact: true })).toBeVisible()
  await expect(panel.getByRole('link', { name: 'View subagent audit' })).toHaveAttribute('href', /run=child-a/)
  await expect(panel.getByRole('link', { name: 'Open subagent thread' })).toHaveCount(0)
  await panel.getByRole('button', { name: 'Reviewer', exact: true }).click()
  await expect(panel.getByText('No audit records available for this subagent.')).toBeVisible()
  await expect(panel.getByText('Unknown', { exact: true })).toBeVisible()
  await expect(panel.getByText('Conversation unavailable. Older runs may not have a saved transcript.')).toBeVisible()
  await expect(panel.getByRole('link', { name: 'Open subagent thread' })).toHaveAttribute('href', '/threads?run=child-b')
  await panel.getByRole('button', { name: 'Researcher', exact: true }).click()
  await expect(panel.getByText('success', { exact: true })).toBeVisible()
  await panel.getByRole('button', { name: 'All subagents' }).click()
  await expect(panel.getByRole('button', { name: 'Researcher', exact: true })).toBeVisible()
  await expect(page).toHaveURL(/\/threads\/1$/)
  await expect(page.getByRole('heading', { name: 'Thread #1', exact: true })).toBeVisible()
  expect(calls).toContain('audit.unsubscribe')
})
