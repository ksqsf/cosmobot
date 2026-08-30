import { expect, test, type Page } from '@playwright/test'

interface RpcRequest { id: number, method: string, params: Record<string, unknown> }
interface TestOption {
  path: string[]
  label: string
  description: string
  owner: string
  type: { kind: string, values?: string[] }
  required: boolean
  default: unknown
  constraints: Record<string, never>
  activation: 'restart'
  source: { present: boolean, value: unknown }
  effective: unknown
}
interface ConfigRpc {
  requests: RpcRequest[]
  conflictNextUpdate: () => void
  delayNextValidation: () => void
  releaseValidation: () => void
  disconnect: () => void
  connectionCount: () => number
}

const methods = ['config.get', 'config.validate', 'config.update', 'config.rollback', 'admin.restart']
const group = (path: string[], label: string): { path: string[], label: string } => ({ path, label })
const textOption = (path: string[], label: string, value: string): TestOption => ({
  path, label, description: `${label} setting.`, owner: 'Test.Config', type: { kind: 'text' }, required: false,
  default: value, constraints: {}, activation: 'restart', source: { present: true, value }, effective: value,
})
const snapshot = {
  schemaVersion: 2, revision: 'revision-1', activeRevision: 'active-1', sourceState: 'valid', editable: true,
  diagnostics: [],
  configuration: {
    sections: [
      { path: ['rpc'], label: 'RPC', group: group(['interfaces'], 'Interfaces'), optional: false, present: true, repeatable: false, options: [
        textOption(['rpc', 'host'], 'Host', '127.0.0.1'),
        { ...textOption(['rpc', 'port'], 'Port', ''), type: { kind: 'integer' }, default: 38765, source: { present: true, value: 38765 }, effective: 38765 },
        { ...textOption(['rpc', 'token'], 'Token', ''), type: { kind: 'secret' }, default: 'unset', source: { present: true, value: 'configured' }, effective: 'configured' },
        { ...textOption(['rpc', 'allowed_browser_origins'], 'Allowed browser origins', ''), type: { kind: 'list', values: ['text'] }, default: [], source: { present: true, value: ['http://localhost:4173'] }, effective: ['http://localhost:4173'] },
      ] },
      { path: ['driver', 'qq'], label: 'QQ', group: group(['drivers'], 'Chat drivers'), optional: true, present: true, repeatable: false, options: [
        { ...textOption(['driver', 'qq', 'allowed_groups'], 'Allowed groups', ''), type: { kind: 'list', values: ['integer'] }, default: [], source: { present: true, value: [123] }, effective: [123] },
      ] },
      { path: ['driver', 'matrix'], label: 'Matrix', group: group(['drivers'], 'Chat drivers'), optional: true, present: false, repeatable: false, options: [
        { ...textOption(['driver', 'matrix', 'homeserver'], 'Homeserver', 'https://matrix.example.test'), source: { present: false, value: null } },
      ] },
      { path: ['llm'], label: 'General', group: group(['llm'], 'LLM'), optional: false, present: true, repeatable: false, options: [textOption(['llm', 'chat'], 'Chat provider', 'openrouter')] },
      { path: ['llm', 'chat_provider', 'openrouter'], label: 'openrouter', group: group(['llm'], 'LLM'), optional: false, present: true, repeatable: true, options: [
        textOption(['llm', 'chat_provider', 'openrouter', 'model'], 'Model', 'openai/gpt-5'),
      ] },
    ],
    repeatableSections: [
      { path: ['llm', 'chat_provider'], label: 'Chat providers', group: group(['llm'], 'LLM'), options: [
        { ...textOption(['llm', 'chat_provider', '*', 'model'], 'Model', 'default/model'), source: { present: false, value: null } },
        { ...textOption(['llm', 'chat_provider', '*', 'api_key'], 'API key', ''), type: { kind: 'secret' }, default: 'unset', source: { present: false, value: null }, effective: 'unset' },
      ] },
      { path: ['llm', 'image_provider'], label: 'Image providers', group: group(['llm'], 'LLM'), options: [] },
      { path: ['llm', 'audio_provider'], label: 'Audio providers', group: group(['llm'], 'LLM'), options: [] },
    ],
  },
  backup: { revision: 'backup-1' },
}

async function installConfigRpc(page: Page): Promise<ConfigRpc> {
  const requests: RpcRequest[] = []
  let conflict = false
  let delayValidation = false
  let providerAdded = false
  let connectionCount = 0
  let disconnectSocket = (): void => undefined
  let releaseValidation = (): void => undefined
  const addedProviderSection = {
    path: ['llm', 'chat_provider', 'new provider'], label: 'new provider', group: group(['llm'], 'LLM'), optional: false, present: true, repeatable: true, options: [
      textOption(['llm', 'chat_provider', 'new provider', 'model'], 'Model', 'custom/model'),
      { ...textOption(['llm', 'chat_provider', 'new provider', 'api_key'], 'API key', ''), type: { kind: 'secret' }, default: 'unset', source: { present: false, value: null }, effective: 'unset' },
    ],
  }
  await page.routeWebSocket('ws://127.0.0.1:39999/rpc', (socket) => {
    connectionCount += 1
    disconnectSocket = () => { void socket.close() }
    socket.onMessage((message) => {
      const request = JSON.parse(typeof message === 'string' ? message : message.toString()) as RpcRequest
      requests.push(request)
      const success = (result: unknown): void => socket.send(JSON.stringify({ jsonrpc: '2.0', id: request.id, result }))
      switch (request.method) {
        case 'admin.authenticate': success({ authenticated: true }); break
        case 'admin.capabilities': success({ serverVersion: 'test', methods, topics: [], permissions: ['config.read', 'config.manage', 'admin.restart'], features: {} }); break
        case 'config.get': success(providerAdded ? {
          ...snapshot,
          revision: 'revision-2',
          configuration: { ...snapshot.configuration, sections: [...snapshot.configuration.sections, addedProviderSection] },
        } : snapshot); break
        case 'config.validate': {
          const changes = request.params.changes as { path: string[], value?: unknown }[]
          const reply = (): void => success({ valid: true, revision: snapshot.revision, diagnostics: [], diff: changes.map((change) => ({ path: change.path, before: null, after: change.value ?? null, activation: 'restart' })), restartRequired: true })
          if (delayValidation) {
            delayValidation = false
            releaseValidation = reply
          } else reply()
          break
        }
        case 'config.update':
          if (conflict) {
            conflict = false
            socket.send(JSON.stringify({ jsonrpc: '2.0', id: request.id, error: { code: -32000, message: 'Configuration revision changed', data: { code: 'revision_conflict' } } }))
          } else {
            const changes = request.params.changes as { operation: string }[]
            providerAdded ||= changes.some(({ operation }) => operation === 'add_section')
            success({ updated: true, revision: 'revision-2', backupRevision: snapshot.revision, diff: [], restartRequired: true })
          }
          break
        case 'config.rollback': success({ rolledBack: true, revision: snapshot.revision, backupRevision: 'revision-2', restartRequired: true }); break
        case 'admin.restart': success({ acknowledged: true }); break
        default: socket.send(JSON.stringify({ jsonrpc: '2.0', id: request.id, error: { code: -32601, message: 'Method not found' } }))
      }
    })
  })
  return {
    requests,
    conflictNextUpdate: () => { conflict = true },
    delayNextValidation: () => { delayValidation = true },
    releaseValidation: () => { releaseValidation() },
    disconnect: () => { disconnectSocket() },
    connectionCount: () => connectionCount,
  }
}

async function connect(page: Page): Promise<void> {
  await page.goto('/login')
  await page.getByLabel('RPC endpoint').fill('ws://127.0.0.1:39999/rpc')
  await page.getByLabel('RPC token').fill('token')
  await page.getByRole('button', { name: 'Connect' }).click()
  await expect(page).toHaveURL(/\/overview/)
  await page.goto('/configuration')
  await expect(page.getByRole('heading', { name: 'Configuration' })).toBeVisible()
}

test('configuration editor groups schemas and uses typed controls', async ({ page, isMobile }) => {
  test.skip(isMobile, 'interaction coverage runs once in the desktop project')
  const rpc = await installConfigRpc(page)
  await connect(page)

  const navigation = page.locator('.config-nav')
  await expect(navigation.getByText('Interfaces', { exact: true })).toBeVisible()
  await expect(navigation.getByText('Chat drivers', { exact: true })).toBeVisible()
  await expect(navigation.getByText('LLM', { exact: true })).toBeVisible()
  await expect(navigation.getByText('Chat providers', { exact: true })).toBeVisible()
  await expect(navigation.getByText('Image providers', { exact: true })).toBeVisible()
  await expect(navigation.getByText('Audio providers', { exact: true })).toBeVisible()
  await expect(navigation.getByText('Llm', { exact: true })).toHaveCount(0)
  await expect(navigation.getByText('Acp', { exact: true })).toHaveCount(0)

  await navigation.getByRole('button', { name: 'RPC', exact: true }).click()
  await expect(page.getByRole('textbox', { name: 'Host' })).toHaveValue('127.0.0.1')
  await expect(page.getByRole('spinbutton', { name: 'Port' })).toHaveValue('38765')
  await expect(page.getByRole('textbox', { name: 'Token' })).toHaveValue('')
  await expect(page.getByRole('textbox', { name: 'Allowed browser origins item 1' })).toHaveValue('http://localhost:4173')
  await page.getByRole('button', { name: 'Add entry' }).click()
  expect(rpc.requests.filter(({ method }) => method === 'config.update')).toHaveLength(0)
  await page.getByRole('button', { name: 'Remove Allowed browser origins item 2' }).click()

  await navigation.getByRole('button', { name: 'QQ', exact: true }).click()
  await expect(page.getByRole('spinbutton', { name: 'Allowed groups item 1' })).toHaveValue('123')
  await page.getByRole('button', { name: 'Add entry' }).click()
  await page.getByRole('spinbutton', { name: 'Allowed groups item 2' }).fill('456')
  await page.getByRole('button', { name: 'Validate' }).click()
  await expect(page.getByText(/semantic change.*validated/)).toBeVisible()
  const validation = rpc.requests.filter(({ method }) => method === 'config.validate').at(-1)
  expect(validation?.params.changes).toEqual([{ operation: 'set', path: ['driver', 'qq', 'allowed_groups'], value: [123, 456] }])
  await page.getByRole('button', { name: 'Discard drafts' }).click()
  await navigation.getByRole('button', { name: 'Matrix', exact: true }).click()
  await expect(page.getByText('Add this optional section before editing its settings.')).toBeVisible()
  await page.getByRole('button', { name: 'Add Matrix' }).click()
  await page.getByRole('button', { name: 'Validate' }).click()
  expect(rpc.requests.filter(({ method }) => method === 'config.validate').at(-1)?.params.changes).toEqual([
    { operation: 'add_section', path: ['driver', 'matrix'] },
  ])
})

test('configuration drafts validate before apply and survive revision conflicts', async ({ page, isMobile }) => {
  test.skip(isMobile, 'interaction coverage runs once in the desktop project')
  const rpc = await installConfigRpc(page)
  await connect(page)
  const navigation = page.locator('.config-nav')

  await navigation.getByRole('button', { name: 'RPC', exact: true }).click()
  await page.getByRole('textbox', { name: 'Host' }).fill('127.0.0.2')
  await page.getByRole('button', { name: 'Refresh' }).click()
  await page.getByRole('alertdialog').getByRole('button', { name: 'Keep drafts' }).click()
  await expect(page.getByRole('textbox', { name: 'Host' })).toHaveValue('127.0.0.2')
  const configGetsBeforeReconnect = rpc.requests.filter(({ method }) => method === 'config.get').length
  const connectionsBeforeReconnect = rpc.connectionCount()
  rpc.delayNextValidation()
  await page.getByRole('button', { name: 'Validate' }).click()
  await expect.poll(() => rpc.requests.filter(({ method }) => method === 'config.validate').length).toBe(1)
  rpc.disconnect()
  await expect.poll(rpc.connectionCount).toBeGreaterThan(connectionsBeforeReconnect)
  await expect(page.getByText('Connected', { exact: true })).toBeVisible({ timeout: 5_000 })
  rpc.releaseValidation()
  await expect(page.getByRole('textbox', { name: 'Host' })).toHaveValue('127.0.0.2')
  expect(rpc.requests.filter(({ method }) => method === 'config.get')).toHaveLength(configGetsBeforeReconnect)
  await expect(page.getByRole('button', { name: 'Apply' })).toBeDisabled()
  await page.getByRole('button', { name: 'Validate' }).click()
  await expect(page.getByRole('button', { name: 'Apply' })).toBeEnabled()
  rpc.conflictNextUpdate()
  await page.getByRole('button', { name: 'Apply' }).click()
  await expect(page.getByText('Configuration revision changed')).toBeVisible()
  await expect(page.getByRole('textbox', { name: 'Host' })).toHaveValue('127.0.0.2')

  await page.getByRole('button', { name: 'Discard drafts' }).click()
  const chatProviders = navigation.locator('.config-nav-cluster').filter({ hasText: 'Chat providers' })
  await chatProviders.getByPlaceholder('Provider name').fill('new provider')
  await chatProviders.getByRole('button', { name: 'Add provider' }).click()
  await expect(page.getByRole('heading', { name: 'new provider' })).toBeVisible()
  await expect(page.getByRole('textbox', { name: 'Model' })).toHaveValue('default/model')
  await expect(page.getByRole('textbox', { name: 'API key' })).toHaveValue('')
  await page.getByRole('textbox', { name: 'Model' }).fill('custom/model')
  await page.getByRole('button', { name: 'Validate' }).click()
  expect(rpc.requests.filter(({ method }) => method === 'config.validate').at(-1)?.params.changes).toEqual([
    { operation: 'add_section', path: ['llm', 'chat_provider', 'new provider'] },
    { operation: 'set', path: ['llm', 'chat_provider', 'new provider', 'model'], value: 'custom/model' },
  ])
  await page.getByRole('button', { name: 'Apply' }).click()
  await expect(navigation.getByRole('button', { name: 'new provider', exact: true })).toBeVisible()

  await navigation.getByRole('button', { name: 'openrouter', exact: true }).click()
  await page.getByRole('button', { name: 'Remove provider' }).click()
  await page.getByRole('button', { name: 'Validate' }).click()
  expect((rpc.requests.filter(({ method }) => method === 'config.validate').at(-1)?.params.changes as unknown[])[0]).toEqual({ operation: 'remove_section', path: ['llm', 'chat_provider', 'openrouter'] })
  await chatProviders.getByPlaceholder('Provider name').fill('openrouter')
  await chatProviders.getByRole('button', { name: 'Add provider' }).click()
  await expect(navigation.getByRole('button', { name: 'openrouter', exact: true })).toBeVisible()
  await expect(page.getByRole('button', { name: 'Validate' })).toBeDisabled()
  await expect(page.getByRole('button', { name: 'Apply' })).toBeDisabled()
})

test('configuration secrets, rollback, and restart require explicit actions', async ({ page, isMobile }) => {
  test.skip(isMobile, 'interaction coverage runs once in the desktop project')
  const rpc = await installConfigRpc(page)
  await connect(page)
  await page.locator('.config-nav').getByRole('button', { name: 'RPC', exact: true }).click()

  await page.getByRole('textbox', { name: 'Token' }).fill('replacement')
  await page.getByRole('button', { name: 'Validate' }).click()
  expect((rpc.requests.filter(({ method }) => method === 'config.validate').at(-1)?.params.changes as unknown[])[0]).toEqual({ operation: 'replace_secret', path: ['rpc', 'token'], value: 'replacement' })
  await page.getByRole('button', { name: 'Discard drafts' }).click()
  await page.getByRole('button', { name: 'Clear secret' }).click()
  await page.getByRole('button', { name: 'Validate' }).click()
  expect((rpc.requests.filter(({ method }) => method === 'config.validate').at(-1)?.params.changes as unknown[])[0]).toEqual({ operation: 'clear_secret', path: ['rpc', 'token'] })

  await page.getByRole('button', { name: 'Discard drafts' }).click()
  await page.getByRole('button', { name: 'Roll back' }).click()
  await page.getByRole('alertdialog').getByRole('button', { name: 'Roll back' }).click()
  await expect.poll(() => rpc.requests.filter(({ method }) => method === 'config.rollback').length).toBe(1)
  await page.getByRole('button', { name: 'Restart' }).click()
  await page.getByRole('alertdialog').getByRole('button', { name: 'Restart' }).click()
  await expect.poll(() => rpc.requests.filter(({ method }) => method === 'admin.restart').length).toBe(1)
})
