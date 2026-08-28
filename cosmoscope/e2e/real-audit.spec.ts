import { expect, test } from '@playwright/test'

const { COSMOSCOPE_RPC_TOKEN: rpcToken, COSMOSCOPE_RPC_ENDPOINT: configuredEndpoint } = process.env
const rpcEndpoint = configuredEndpoint ?? 'ws://127.0.0.1:38765/rpc'

test('real audit loads a selected detail and restores it after reload', async ({ page, isMobile }) => {
  test.skip(isMobile || rpcToken === undefined, 'requires a desktop browser and COSMOSCOPE_RPC_TOKEN')
  await page.goto('/login')
  await page.getByLabel('RPC endpoint').fill(rpcEndpoint)
  await page.getByLabel('RPC token').fill(rpcToken ?? '')
  await page.getByRole('button', { name: 'Connect' }).click()
  await page.locator('.desktop-nav').getByRole('link', { name: 'Audit' }).click()
  await expect(page.getByText('Live data', { exact: true })).toBeVisible()
  await expect(page.getByText('Receiving events', { exact: true })).toBeVisible()
  const search = page.getByLabel('Search audit events')
  await search.fill('thread:9007199254740991')
  await search.press('Enter')
  await expect(page.getByText('Thread #9007199254740991 was not found.')).toBeVisible()
  await expect(search).toHaveValue('thread:9007199254740991')
  await search.fill('')
  await search.press('Enter')
  await expect(page.getByText('Thread #9007199254740991 was not found.')).toHaveCount(0)
  await page.getByRole('option').first().click()
  await expect(page).toHaveURL(/\/audit\/\d+$/)
  await expect(page.getByText(/Audit event #\d+/)).toBeVisible()
  await page.getByRole('button', { name: 'Pause' }).click()
  await expect(page.getByRole('button', { name: /Resume \(0\)/ })).toBeVisible()
  await page.reload()
  await expect(page.getByText('Live data', { exact: true })).toBeVisible()
  await expect(page.getByText(/Audit event #\d+/)).toBeVisible()
})
