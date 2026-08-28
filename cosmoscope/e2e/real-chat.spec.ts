import { expect, test } from '@playwright/test'

const { COSMOSCOPE_RPC_TOKEN: rpcToken, COSMOSCOPE_RPC_ENDPOINT: configuredEndpoint } = process.env
const rpcEndpoint = configuredEndpoint ?? 'ws://127.0.0.1:38765/rpc'

test('real chat sends attachments, renders streaming output, and cleans up its session', async ({ page, isMobile }) => {
  test.skip(isMobile || rpcToken === undefined, 'requires a desktop browser and COSMOSCOPE_RPC_TOKEN')
  const label = `Cosmoscope chat smoke ${String(Date.now())}`
  await page.goto('/login')
  await page.getByLabel('RPC endpoint').fill(rpcEndpoint)
  await page.getByLabel('RPC token').fill(rpcToken ?? '')
  await page.getByRole('button', { name: 'Connect' }).click()
  await page.locator('.desktop-nav').getByRole('link', { name: /Chat/ }).click()
  await page.getByRole('button', { name: 'New session' }).click()
  await expect(page).toHaveURL(/\/chat\/[^/]+$/)
  const createdSessionId = decodeURIComponent(new URL(page.url()).pathname.split('/').at(-1) ?? '')

  try {
    page.once('dialog', (dialog) => dialog.accept(label))
    await page.getByRole('button', { name: 'Rename session' }).click()
    await expect(page.getByRole('button', { name: new RegExp(label) })).toBeVisible()
    await page.locator('input[type="file"]').setInputFiles([
      { name: 'chat-smoke.txt', mimeType: 'text/plain', buffer: Buffer.from('chat smoke attachment') },
      { name: 'chat-smoke.png', mimeType: 'image/png', buffer: Buffer.from('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII=', 'base64') },
    ])
    await expect(page.getByText('chat-smoke.txt', { exact: true })).toBeVisible()
    await expect(page.getByText('chat-smoke.png', { exact: true })).toBeVisible()
    await page.getByLabel('Message cosmobot').fill('## Chat smoke\n\nRender `CHAT_SMOKE_RECEIVED` and $E = mc^2$. Reply exactly CHAT_SMOKE_RECEIVED and do not use tools.')
    await page.getByLabel('Message cosmobot').press('Control+Enter')
    const userMessage = page.locator('.message.user').filter({ hasText: 'CHAT_SMOKE_RECEIVED' }).last()
    await expect(userMessage).toBeVisible()
    await expect(userMessage.getByRole('link', { name: /chat-smoke.txt/ })).toHaveAttribute('download', '')
    await userMessage.getByRole('button', { name: 'Zoom image' }).click()
    await expect(page.getByRole('dialog', { name: 'Image preview' })).toBeVisible()
    await page.keyboard.press('Escape')
    const assistantMessage = page.locator('.message.bot').last()
    await expect(assistantMessage).toContainText('CHAT_SMOKE_RECEIVED', { timeout: 120_000 })
    await expect(assistantMessage.getByLabel('Streaming response')).toHaveCount(0, { timeout: 120_000 })
  } finally {
    const session = page.locator('.conversation').filter({ hasText: createdSessionId })
    if (await session.count() > 0) {
      await session.click()
      await page.getByRole('button', { name: 'Delete session' }).click()
      await page.getByRole('button', { name: 'Delete session', exact: true }).last().click()
      await expect(session).toHaveCount(0)
    }
  }
})
