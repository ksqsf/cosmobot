import { expect, test } from '@playwright/test'

const { COSMOSCOPE_RPC_TOKEN: rpcToken, COSMOSCOPE_RPC_ENDPOINT: configuredEndpoint } = process.env
const rpcEndpoint = configuredEndpoint ?? 'ws://127.0.0.1:38765/rpc'

test('live configuration can be inspected and validated without mutation', async ({ page, isMobile }) => {
  test.skip(isMobile || rpcToken === undefined, 'requires a desktop browser and COSMOSCOPE_RPC_TOKEN')
  await page.goto('/login')
  await page.getByLabel('RPC endpoint').fill(rpcEndpoint)
  await page.getByLabel('RPC token').fill(rpcToken ?? '')
  await page.getByRole('button', { name: 'Connect' }).click()
  await expect(page).toHaveURL(/\/overview/)

  await page.goto('/configuration')
  await expect(page.getByRole('heading', { name: 'Configuration' })).toBeVisible()
  const navigation = page.locator('.config-nav')
  await expect(navigation.getByRole('button', { name: 'RPC', exact: true })).toBeVisible()
  await expect(navigation.getByText('LLM', { exact: true })).toBeVisible()
  await expect(page.getByText('Demo', { exact: true })).toHaveCount(0)

  await navigation.getByRole('button', { name: 'RPC', exact: true }).click()
  const host = page.getByRole('textbox', { name: 'Host' })
  await host.fill(`${await host.inputValue()}.validation-smoke`)
  await page.getByRole('button', { name: 'Validate' }).click()
  await expect(page.getByText(/semantic change.*validated/)).toBeVisible()
})
