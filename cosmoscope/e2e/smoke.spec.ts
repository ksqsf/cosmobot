import { expect, test } from '@playwright/test'
import { AxeBuilder } from '@axe-core/playwright'

const axeRoutes = ['/login', '/overview', '/chat', '/threads', '/memory', '/skills', '/audit', '/tasks', '/resources', '/schedules', '/configuration']

for (const path of axeRoutes) {
  test(`${path} has no serious accessibility violations`, async ({ page }) => {
    await page.goto(path)
    await expect(page.locator('h1')).toBeVisible()
    const results = await new AxeBuilder({ page }).analyze()
    expect(results.violations.filter(({ impact }) => impact === 'critical' || impact === 'serious')).toEqual([])
  })
}

test('navigation, theme, sidebar, and command palette work', async ({ page, isMobile }) => {
  test.skip(isMobile, 'desktop project only')
  await page.goto('/overview')
  await page.locator('.desktop-nav').getByRole('link', { name: 'Tasks' }).click()
  await expect(page).toHaveURL(/\/tasks/)
  const initialTheme = await page.locator('html').getAttribute('data-theme')
  await page.getByLabel('Toggle color theme').click()
  await expect(page.locator('html')).toHaveAttribute('data-theme', initialTheme === 'dark' ? 'light' : 'dark')
  await page.goto('/overview')
  await page.getByRole('button', { name: 'Collapse sidebar' }).click()
  await expect(page.getByRole('button', { name: 'Expand sidebar' })).toBeVisible()
  await page.keyboard.press('Control+k')
  const commandDialog = page.getByRole('dialog', { name: 'Go to' })
  const search = page.getByRole('combobox', { name: 'Search pages' })
  await expect(commandDialog).toBeVisible()
  await expect(search).toBeFocused()
  const firstResult = await commandDialog.locator('.command-result').first().boundingBox()
  const firstIcon = await commandDialog.locator('.command-result-icon').first().boundingBox()
  expect(firstResult).not.toBeNull()
  expect(firstIcon).not.toBeNull()
  expect(firstIcon?.y).toBeGreaterThanOrEqual(firstResult?.y ?? 0)
  expect((firstIcon?.y ?? 0) + (firstIcon?.height ?? 0)).toBeLessThanOrEqual((firstResult?.y ?? 0) + (firstResult?.height ?? 0))
  await page.keyboard.type('a')
  await expect(search).toHaveValue('a')
  const initialTop = (await commandDialog.boundingBox())?.y
  await search.fill('no matching page')
  await expect(page.getByText('No matching pages')).toBeVisible()
  expect((await commandDialog.boundingBox())?.y).toBe(initialTop)
  await page.keyboard.press('Escape')
  await expect(page.getByRole('button', { name: /Search or jump/ })).toBeFocused()
  await page.keyboard.press('Control+k')
  await page.locator('.p-dialog-mask').click({ position: { x: 5, y: 5 } })
  await expect(commandDialog).toBeHidden()
})

test('tasks and resources are independent navigation destinations', async ({ page, isMobile }) => {
  await page.goto('/tasks')
  if (isMobile) await page.getByLabel('Open navigation').click()
  await page.getByRole('link', { name: 'Resources' }).click()
  await expect(page).toHaveURL(/\/resources/)
  await expect(page.getByRole('heading', { name: 'Resources' })).toBeVisible()
  await page.goBack()
  await expect(page).toHaveURL(/\/tasks/)
  await page.goForward()
  await expect(page).toHaveURL(/\/resources/)
  if (isMobile) await page.getByLabel('Open navigation').click()
  await page.getByRole('link', { name: 'Tasks' }).click()
  await expect(page).toHaveURL(/\/tasks/)
})

test('schedule navigation exposes the live-only manager', async ({ page, isMobile }) => {
  await page.goto('/resources')
  if (isMobile) await page.getByLabel('Open navigation').click()
  await page.getByRole('link', { name: 'Schedules' }).click()
  await expect(page).toHaveURL(/\/schedules/)
  await expect(page.getByRole('heading', { name: 'Schedules' })).toBeVisible()
  await expect(page.getByText('Connect to cosmobot to load schedules.')).toBeVisible()
})

test('manager pages expose real contracts without invented controls', async ({ page }) => {
  await page.goto('/tasks')
  await expect(page.getByText('Connect to cosmobot to load tasks.')).toBeVisible()
  await expect(page.getByRole('row', { name: /agent.run/ })).toHaveCount(0)
  await expect(page.getByText('Owner', { exact: true })).toHaveCount(0)
  await expect(page.getByRole('button', { name: 'Pause' })).toHaveCount(0)
  await expect(page.getByRole('button', { name: 'Await completion' })).toHaveCount(0)
  await page.goto('/resources')
  await expect(page.getByText('Connect to cosmobot to load resources.')).toBeVisible()
  await expect(page.getByRole('row', { name: /workspace-8f2c/ })).toHaveCount(0)
})

test('responsive navigation opens on mobile', async ({ page, isMobile }) => {
  test.skip(!isMobile, 'mobile project only')
  await page.goto('/overview')
  await page.getByLabel('Open navigation').click()
  const navigation = page.getByRole('dialog', { name: 'Navigate' })
  await expect(navigation).toBeVisible()
  await page.keyboard.press('Control+k')
  await expect(page.getByRole('dialog', { name: 'Go to' })).toBeVisible()
  await page.keyboard.press('Escape')
  await expect(page.getByRole('dialog', { name: 'Go to' })).toBeHidden()
  await expect(navigation).toBeVisible()
  await page.keyboard.press('Escape')
  await expect(navigation).toBeHidden()
})
