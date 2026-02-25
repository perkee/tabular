// @ts-check
const { test, expect } = require('@playwright/test');

test.beforeEach(async ({ page }) => {
  await page.goto('/');
});

// --- Initial state ---

test('shows a 3x3 grid with default headers', async ({ page }) => {
  // Three header cells pre-filled
  await expect(page.locator('#cell-0-0')).toHaveValue('Header 1');
  await expect(page.locator('#cell-0-1')).toHaveValue('Header 2');
  await expect(page.locator('#cell-0-2')).toHaveValue('Header 3');

  // Data cells exist but are empty
  await expect(page.locator('#cell-1-0')).toHaveValue('');
  await expect(page.locator('#cell-2-2')).toHaveValue('');
});

test('markdown output contains headers and separator', async ({ page }) => {
  const md = await page.locator('#md-output').inputValue();
  expect(md).toContain('Header 1');
  expect(md).toContain('|');
  expect(md).toContain('---');
});

test('box drawing output contains headers and box chars', async ({ page }) => {
  const box = await page.locator('#box-output').inputValue();
  expect(box).toContain('Header 1');
  // Box-drawing corners/lines
  expect(box).toMatch(/[┌┐└┘─│]/);
});

// --- Cell editing ---

test('filling a cell updates the input and markdown output', async ({ page }) => {
  await page.locator('#cell-1-0').fill('Hello');
  await expect(page.locator('#cell-1-0')).toHaveValue('Hello');

  const md = await page.locator('#md-output').inputValue();
  expect(md).toContain('Hello');
});

// --- Add / remove rows and columns ---

test('add row creates a new row', async ({ page }) => {
  // Initially cell-3-0 should not exist (3x3 grid: rows 0-2)
  await expect(page.locator('#cell-3-0')).toHaveCount(0);

  await page.locator('#add-row').click();
  await expect(page.locator('#cell-3-0')).toHaveCount(1);
});

test('add column creates a new column', async ({ page }) => {
  await expect(page.locator('#cell-0-3')).toHaveCount(0);

  await page.locator('#add-column').click();
  await expect(page.locator('#cell-0-3')).toHaveCount(1);
});

test('remove row removes that row', async ({ page }) => {
  await expect(page.locator('#cell-2-0')).toHaveCount(1);

  await page.locator('#del-row-2').click();
  // After removing row 2, cell-2-0 should be gone (grid is now 2x3)
  await expect(page.locator('#cell-2-0')).toHaveCount(0);
});

test('remove column removes that column', async ({ page }) => {
  await expect(page.locator('#cell-0-2')).toHaveCount(1);

  await page.locator('#del-col-2').click();
  await expect(page.locator('#cell-0-2')).toHaveCount(0);
});

// --- Format toggle ---

test('switching to compact format changes markdown output', async ({ page }) => {
  await page.locator('#format-compact').click();

  // Compact separator uses exactly "---" (auto-retries until Elm re-renders)
  await expect(page.locator('#md-output')).toHaveValue(/\| --- \|/);
});

// --- Alignment ---

test('setting center alignment updates markdown separator', async ({ page }) => {
  await page.locator('#align-0-c').click();

  // Expanded center alignment produces :-...-: pattern (auto-retries)
  await expect(page.locator('#md-output')).toHaveValue(/:-+:/);
});

// --- Import ---

test('import CSV populates the grid', async ({ page }) => {
  await page.locator('#toggle-import').click();

  await page.locator('#import-textarea').fill('A,B\n1,2');
  await page.locator('#import-btn').click();

  // Grid should now be 2x2 with imported data
  await expect(page.locator('#cell-0-0')).toHaveValue('A');
  await expect(page.locator('#cell-0-1')).toHaveValue('B');
  await expect(page.locator('#cell-1-0')).toHaveValue('1');
  await expect(page.locator('#cell-1-1')).toHaveValue('2');

  // Import panel should be hidden
  await expect(page.locator('#import-textarea')).toHaveCount(0);
});

// --- Copy buttons ---

const isRemote = !!process.env.BASE_URL;

test('copy markdown button writes textarea value to clipboard', async ({ page, context }) => {
  test.skip(isRemote, 'copy-button web component not available on preview deployments');
  await context.grantPermissions(['clipboard-read', 'clipboard-write']);

  const copyBtn = page.locator('copy-button[target="md-output"] button');
  await copyBtn.click();

  const clipboardText = await page.evaluate(() => navigator.clipboard.readText());
  const mdValue = await page.locator('#md-output').inputValue();
  expect(clipboardText).toBe(mdValue);
});

test('copy box-drawing button writes textarea value to clipboard', async ({ page, context }) => {
  test.skip(isRemote, 'copy-button web component not available on preview deployments');
  await context.grantPermissions(['clipboard-read', 'clipboard-write']);

  const copyBtn = page.locator('copy-button[target="box-output"] button');
  await copyBtn.click();

  const clipboardText = await page.evaluate(() => navigator.clipboard.readText());
  const boxValue = await page.locator('#box-output').inputValue();
  expect(clipboardText).toBe(boxValue);
});

test('copy HTML button writes textarea value to clipboard', async ({ page, context }) => {
  test.skip(isRemote, 'copy-button web component not available on preview deployments');
  await context.grantPermissions(['clipboard-read', 'clipboard-write']);

  const copyBtn = page.locator('copy-button[target="html-output"] button');
  await copyBtn.click();

  const clipboardText = await page.evaluate(() => navigator.clipboard.readText());
  const htmlValue = await page.locator('#html-output').inputValue();
  expect(clipboardText).toBe(htmlValue);
});

test('copy button shows "Copied!" feedback then resets', async ({ page, context }) => {
  test.skip(isRemote, 'copy-button web component not available on preview deployments');
  await context.grantPermissions(['clipboard-read', 'clipboard-write']);

  const copyBtn = page.locator('copy-button[target="md-output"] button');
  await expect(copyBtn).toHaveText('Copy');

  await copyBtn.click();
  await expect(copyBtn).toHaveText('Copied!');

  // Resets back to "Copy" after ~1.5s
  await expect(copyBtn).toHaveText('Copy', { timeout: 3000 });
});

test('import cancel hides the import panel', async ({ page }) => {
  await page.locator('#toggle-import').click();
  await expect(page.locator('#import-textarea')).toHaveCount(1);

  await page.locator('#import-cancel').click();
  await expect(page.locator('#import-textarea')).toHaveCount(0);
});

// --- Collapsible sections ---

test('clicking output section header collapses and expands its content', async ({ page }) => {
  // Box Drawing section should be visible initially
  await expect(page.locator('#box-output')).toHaveCount(1);

  // Click the Box Drawing header to collapse
  await page.locator('.output-section .output-header .output-title', { hasText: 'Box Drawing' }).click();
  await expect(page.locator('#box-output')).toHaveCount(0);

  // Click again to expand
  await page.locator('.output-section .output-header .output-title', { hasText: 'Box Drawing' }).click();
  await expect(page.locator('#box-output')).toHaveCount(1);
});

test('collapsing one section does not affect others', async ({ page }) => {
  // Collapse Markdown
  await page.locator('.output-section .output-header .output-title', { hasText: 'Markdown' }).click();
  await expect(page.locator('#md-output')).toHaveCount(0);

  // Box Drawing and HTML should still be visible
  await expect(page.locator('#box-output')).toHaveCount(1);
  await expect(page.locator('#html-output')).toHaveCount(1);
});
