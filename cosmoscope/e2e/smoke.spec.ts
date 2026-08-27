import { expect, test } from '@playwright/test'
import { AxeBuilder } from '@axe-core/playwright'

const axeRoutes = ['/login', '/overview', '/tasks', '/configuration/drivers']

for (const path of axeRoutes) {
  test(`${path} has no serious accessibility violations`, async ({ page }) => {
    await page.goto(path)
    await expect(page.locator('h1')).toBeVisible()
    const results = await new AxeBuilder({ page }).analyze()
    expect(results.violations.filter(({ impact }) => impact === 'critical' || impact === 'serious')).toEqual([])
  })
}

test('navigation, theme, drawer focus, and command palette work', async ({ page, isMobile }) => {
  test.skip(isMobile, 'desktop project only')
  await page.goto('/overview')
  await page.getByRole('link', { name: 'Tasks' }).click()
  await expect(page).toHaveURL(/\/tasks/)
  const initialTheme = await page.locator('html').getAttribute('data-theme')
  await page.getByLabel('Toggle color theme').click()
  await expect(page.locator('html')).toHaveAttribute('data-theme', initialTheme === 'dark' ? 'light' : 'dark')
  await page.goto('/overview')
  await page.getByRole('row', { name: /agent.run/ }).click()
  await expect(page.getByRole('dialog', { name: 'Task detail' })).toBeVisible()
  await page.keyboard.press('Escape')
  await page.keyboard.press('Control+k')
  await expect(page.getByRole('dialog', { name: 'Command palette' })).toBeVisible()
  await expect(page.getByLabel('Search commands')).toBeFocused()
  await page.keyboard.press('Escape')
  await expect(page.getByRole('button', { name: /Search or jump/ })).toBeFocused()
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

test('responsive navigation opens on mobile', async ({ page, isMobile }) => {
  test.skip(!isMobile, 'mobile project only')
  await page.goto('/overview')
  await page.getByLabel('Open navigation').click()
  await expect(page.getByRole('dialog', { name: 'Navigate' })).toBeVisible()
})
