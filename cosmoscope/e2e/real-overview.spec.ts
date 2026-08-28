import { expect, test } from '@playwright/test'

const { COSMOSCOPE_RPC_TOKEN: rpcToken, COSMOSCOPE_RPC_ENDPOINT: configuredEndpoint } = process.env
const rpcEndpoint = configuredEndpoint ?? 'ws://127.0.0.1:38765/rpc'

test('authenticated overview combines live RPC snapshots with labelled fixtures', async ({ page, isMobile }) => {
  test.skip(isMobile || rpcToken === undefined, 'requires a desktop browser and COSMOSCOPE_RPC_TOKEN')
  await page.goto('/login')
  await page.getByLabel('RPC endpoint').fill(rpcEndpoint)
  await page.getByLabel('RPC token').fill(rpcToken ?? '')
  await page.getByRole('button', { name: 'Connect' }).click()
  await expect(page).toHaveURL(/\/overview/)
  await expect(page.getByText('Mixed data', { exact: true })).toBeVisible()
  for (const label of ['Active tasks', 'Chat sessions', 'Managed resources', 'Recent failures']) {
    await expect(page.locator('.metric').filter({ hasText: label }).getByText('Live', { exact: true })).toBeVisible()
  }
  await expect(page.locator('.metric').filter({ hasText: 'Plugins loaded' }).getByText('Demo', { exact: true })).toBeVisible()
  await expect(page.locator('.platform-panel').getByText('Demo', { exact: true })).toBeVisible()
  await expect(page.locator('.activity-list a').first()).toBeVisible()
})
