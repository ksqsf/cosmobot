import { expect, test } from '@playwright/test'

const { COSMOSCOPE_RPC_TOKEN: rpcToken, COSMOSCOPE_RPC_ENDPOINT: configuredEndpoint } = process.env
const rpcEndpoint = configuredEndpoint ?? 'ws://127.0.0.1:38765/rpc'

test('real task and resource snapshots match manager RPC contracts', async ({ page, isMobile }) => {
  test.skip(isMobile || rpcToken === undefined, 'requires a desktop browser and COSMOSCOPE_RPC_TOKEN')
  await page.goto('/login')
  await page.getByLabel('RPC endpoint').fill(rpcEndpoint)
  await page.getByLabel('RPC token').fill(rpcToken ?? '')
  await page.getByRole('button', { name: 'Connect' }).click()
  await expect(page).toHaveURL(/\/overview/)
  await page.goto('/tasks')
  await expect(page.getByText('Live data', { exact: true })).toBeVisible()
  await expect(page.getByRole('columnheader', { name: 'Owner' })).toHaveCount(0)
  await expect(page.getByRole('button', { name: 'Await completion' })).toHaveCount(0)
  const taskRows = page.locator('tbody tr')
  if (await taskRows.count() > 0) {
    await taskRows.first().click()
    await expect(page.getByRole('heading', { name: 'Resources created by this task' })).toBeVisible()
  }
  await page.goto('/resources')
  await expect(page.getByText('Live data', { exact: true })).toBeVisible()
  await expect(page.getByRole('columnheader', { name: 'Lifetime' })).toBeVisible()
})
