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

// --- Insert rows and columns ---

test('insert row before row 1 shifts content down', async ({ page }) => {
  // Fill row 1 with known content
  await page.locator('#cell-1-0').fill('R1C0');

  // Click insert-row-1 to insert before row 1
  await page.locator('#insert-row-1').click();

  // Grid should now be 4 rows
  await expect(page.locator('#cell-3-0')).toHaveCount(1);

  // Original row 1 content should now be at row 2
  await expect(page.locator('#cell-2-0')).toHaveValue('R1C0');

  // New row 1 should be empty
  await expect(page.locator('#cell-1-0')).toHaveValue('');
});

test('insert column before column 1 shifts content right', async ({ page }) => {
  // Fill column 1 with known content
  await page.locator('#cell-0-1').fill('R0C1');

  // Click insert-col-1 to insert before column 1
  await page.locator('#insert-col-1').click();

  // Grid should now be 4 columns
  await expect(page.locator('#cell-0-3')).toHaveCount(1);

  // Original column 1 content should now be at column 2
  await expect(page.locator('#cell-0-2')).toHaveValue('R0C1');

  // New column 1 should be empty
  await expect(page.locator('#cell-0-1')).toHaveValue('');
});

// --- Format toggle ---

test('switching to compact format changes markdown output', async ({ page }) => {
  await page.locator('#format-compact').click();

  // Compact separator uses exactly "---" (auto-retries until Elm re-renders)
  await expect(page.locator('#md-output')).toHaveValue(/\| --- \|/);
});

// --- Alignment ---

test('setting body center alignment updates markdown separator', async ({ page }) => {
  await page.locator('#balign-0-c').click();

  // Expanded center alignment produces :-...-: pattern in separator (auto-retries)
  await expect(page.locator('#md-output')).toHaveValue(/:-+:/);
});

test('header and body alignment are independent', async ({ page }) => {
  // Set body to right for column 0 (header stays at default center)
  await page.locator('#balign-0-r').click();

  // Separator should use body alignment (right: ---...:)
  await expect(page.locator('#md-output')).toHaveValue(/---+:/);
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

test('copy markdown button writes textarea value to clipboard', async ({ page, context }) => {
  await context.grantPermissions(['clipboard-read', 'clipboard-write']);

  const copyBtn = page.locator('copy-button[target="md-output"] button');
  await copyBtn.click();

  const clipboardText = await page.evaluate(() => navigator.clipboard.readText());
  const mdValue = await page.locator('#md-output').inputValue();
  expect(clipboardText).toBe(mdValue);
});

test('copy box-drawing button writes textarea value to clipboard', async ({ page, context }) => {
  await context.grantPermissions(['clipboard-read', 'clipboard-write']);

  const copyBtn = page.locator('copy-button[target="box-output"] button');
  await copyBtn.click();

  const clipboardText = await page.evaluate(() => navigator.clipboard.readText());
  const boxValue = await page.locator('#box-output').inputValue();
  expect(clipboardText).toBe(boxValue);
});

test('copy HTML button writes textarea value to clipboard', async ({ page, context }) => {
  await context.grantPermissions(['clipboard-read', 'clipboard-write']);

  const copyBtn = page.locator('copy-button[target="html-output"] button');
  await copyBtn.click();

  const clipboardText = await page.evaluate(() => navigator.clipboard.readText());
  const htmlValue = await page.locator('#html-output').inputValue();
  expect(clipboardText).toBe(htmlValue);
});

test('copy button shows "Copied!" feedback then resets', async ({ page, context }) => {
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

// --- Undo ---

test('undo button is disabled when there is no history', async ({ page }) => {
  await expect(page.locator('#undo-btn')).toBeDisabled();
});

test('undo reverts add row', async ({ page }) => {
  // Initially 3x3 grid — cell-3-0 should not exist
  await expect(page.locator('#cell-3-0')).toHaveCount(0);

  await page.locator('#add-row').click();
  await expect(page.locator('#cell-3-0')).toHaveCount(1);
  await expect(page.locator('#undo-btn')).toBeEnabled();

  await page.locator('#undo-btn').click();
  await expect(page.locator('#cell-3-0')).toHaveCount(0);
  await expect(page.locator('#undo-btn')).toBeDisabled();
});

test('undo reverts add column', async ({ page }) => {
  await expect(page.locator('#cell-0-3')).toHaveCount(0);

  await page.locator('#add-column').click();
  await expect(page.locator('#cell-0-3')).toHaveCount(1);

  await page.locator('#undo-btn').click();
  await expect(page.locator('#cell-0-3')).toHaveCount(0);
});

test('undo reverts remove row and restores cell content', async ({ page }) => {
  // Fill a cell in row 2
  await page.locator('#cell-2-0').fill('RowData');

  await page.locator('#del-row-2').click();
  await expect(page.locator('#cell-2-0')).toHaveCount(0);

  await page.locator('#undo-btn').click();
  await expect(page.locator('#cell-2-0')).toHaveValue('RowData');
});

test('undo reverts import data', async ({ page }) => {
  // Remember original header
  await expect(page.locator('#cell-0-0')).toHaveValue('Header 1');

  await page.locator('#toggle-import').click();
  await page.locator('#import-textarea').fill('A,B\n1,2');
  await page.locator('#import-btn').click();

  await expect(page.locator('#cell-0-0')).toHaveValue('A');

  await page.locator('#undo-btn').click();
  await expect(page.locator('#cell-0-0')).toHaveValue('Header 1');
  // Grid should be back to 3x3
  await expect(page.locator('#cell-2-2')).toHaveCount(1);
});

test('multiple undos work in sequence', async ({ page }) => {
  await page.locator('#add-row').click();
  await page.locator('#add-column').click();

  // Now 4x4 grid
  await expect(page.locator('#cell-3-3')).toHaveCount(1);

  // Undo add column → 4x3
  await page.locator('#undo-btn').click();
  await expect(page.locator('#cell-3-3')).toHaveCount(0);
  await expect(page.locator('#cell-3-2')).toHaveCount(1);

  // Undo add row → 3x3
  await page.locator('#undo-btn').click();
  await expect(page.locator('#cell-3-2')).toHaveCount(0);
  await expect(page.locator('#cell-2-2')).toHaveCount(1);
});

test('collapsing one section does not affect others', async ({ page }) => {
  // Collapse Markdown
  await page.locator('.output-section .output-header .output-title', { hasText: 'Markdown' }).click();
  await expect(page.locator('#md-output')).toHaveCount(0);

  // Box Drawing and HTML should still be visible
  await expect(page.locator('#box-output')).toHaveCount(1);
  await expect(page.locator('#html-output')).toHaveCount(1);
});

// --- Sorting ---

test('sort dropdown lists column headers', async ({ page }) => {
  const sortSelect = page.locator('#sort-column');
  await expect(sortSelect).toHaveCount(1);

  // Should have "Sort…" option plus one per column
  const options = sortSelect.locator('option');
  await expect(options).toHaveCount(4); // Sort… + Header 1 + Header 2 + Header 3
  await expect(options.nth(1)).toHaveText('Header 1');
  await expect(options.nth(2)).toHaveText('Header 2');
});

test('selecting a sort column shows direction and method radios', async ({ page }) => {
  // Initially no radios visible
  await expect(page.locator('#sort-asc')).toHaveCount(0);

  // Select column 0
  await page.locator('#sort-column').selectOption('0');

  // Radio buttons should appear
  await expect(page.locator('#sort-asc')).toHaveCount(1);
  await expect(page.locator('#sort-desc')).toHaveCount(1);
  await expect(page.locator('#sort-lex')).toHaveCount(1);
  await expect(page.locator('#sort-num')).toHaveCount(1);
});

test('selecting None hides radios and restores original order', async ({ page }) => {
  // Fill some body cells
  await page.locator('#cell-1-0').fill('B');
  await page.locator('#cell-2-0').fill('A');

  // Sort by column 0
  await page.locator('#sort-column').selectOption('0');
  await expect(page.locator('#sort-asc')).toHaveCount(1);

  // Select None
  await page.locator('#sort-column').selectOption('');
  await expect(page.locator('#sort-asc')).toHaveCount(0);

  // Markdown should have B before A (original order)
  const md = await page.locator('#md-output').inputValue();
  expect(md.indexOf('B')).toBeLessThan(md.indexOf('A'));
});

test('sorting by column reorders rows in markdown output', async ({ page }) => {
  await page.locator('#cell-1-0').fill('Cherry');
  await page.locator('#cell-2-0').fill('Apple');

  // Sort ascending by column 0
  await page.locator('#sort-column').selectOption('0');

  const md = await page.locator('#md-output').inputValue();
  // Apple should come before Cherry in sorted output
  expect(md.indexOf('Apple')).toBeLessThan(md.indexOf('Cherry'));
});

test('numeric sort orders numbers correctly', async ({ page }) => {
  await page.locator('#cell-1-0').fill('10');
  await page.locator('#cell-2-0').fill('2');

  // Sort by column 0, then switch to numeric
  await page.locator('#sort-column').selectOption('0');
  await page.locator('label:has(#sort-num)').click();

  const md = await page.locator('#md-output').inputValue();
  // Numeric: 2 before 10
  expect(md.indexOf('2')).toBeLessThan(md.indexOf('10'));
});

test('sorting does not reorder editor inputs by default', async ({ page }) => {
  await page.locator('#cell-1-0').fill('Cherry');
  await page.locator('#cell-2-0').fill('Apple');

  // Sort ascending by column 0
  await page.locator('#sort-column').selectOption('0');

  // Output should be sorted (Apple before Cherry)
  const md = await page.locator('#md-output').inputValue();
  expect(md.indexOf('Apple')).toBeLessThan(md.indexOf('Cherry'));

  // But editor inputs should stay in original order (Cherry in row 1, Apple in row 2)
  await expect(page.locator('#cell-1-0')).toHaveValue('Cherry');
  await expect(page.locator('#cell-2-0')).toHaveValue('Apple');
});

test('apply sort to inputs physically reorders cell data', async ({ page }) => {
  await page.locator('#cell-1-0').fill('Cherry');
  await page.locator('#cell-2-0').fill('Apple');

  // Sort ascending by column 0
  await page.locator('#sort-column').selectOption('0');

  // Click "Sort inputs to match outputs"
  await page.locator('#apply-sort-to-inputs').click();

  // Cell data should now be physically reordered: Apple in row 1, Cherry in row 2
  await expect(page.locator('#cell-1-0')).toHaveValue('Apple');
  await expect(page.locator('#cell-2-0')).toHaveValue('Cherry');

  // Sort state should be preserved
  await expect(page.locator('#sort-column')).toHaveValue('0');

  // Button should be disabled since inputs now match outputs
  await expect(page.locator('#apply-sort-to-inputs')).toBeDisabled();
});

test('apply sort to inputs is undoable', async ({ page }) => {
  await page.locator('#cell-1-0').fill('Cherry');
  await page.locator('#cell-2-0').fill('Apple');

  await page.locator('#sort-column').selectOption('0');
  await page.locator('#apply-sort-to-inputs').click();

  await expect(page.locator('#cell-1-0')).toHaveValue('Apple');

  await page.locator('#undo-btn').click();
  await expect(page.locator('#cell-1-0')).toHaveValue('Cherry');
  await expect(page.locator('#cell-2-0')).toHaveValue('Apple');
});

test('apply sort button is disabled when inputs already match sort order', async ({ page }) => {
  await page.locator('#cell-1-0').fill('Apple');
  await page.locator('#cell-2-0').fill('Cherry');

  // Sort ascending by column 0 — inputs are already in ascending order
  await page.locator('#sort-column').selectOption('0');
  await expect(page.locator('#apply-sort-to-inputs')).toBeDisabled();

  // Switch to descending — now inputs differ from outputs
  await page.locator('label', { hasText: 'Desc' }).click();
  await expect(page.locator('#apply-sort-to-inputs')).toBeEnabled();
});

test('apply sort button only visible when sorting is active', async ({ page }) => {
  await expect(page.locator('#apply-sort-to-inputs')).toHaveCount(0);

  await page.locator('#sort-column').selectOption('0');
  await expect(page.locator('#apply-sort-to-inputs')).toHaveCount(1);

  await page.locator('#sort-column').selectOption('');
  await expect(page.locator('#apply-sort-to-inputs')).toHaveCount(0);
});

// --- Summary rows ---

test('summary max toggle button is visible', async ({ page }) => {
  await expect(page.locator('#summary-max')).toHaveCount(1);
});

test('clicking max toggle shows summary row in editor', async ({ page }) => {
  // Fill some numeric data
  await page.locator('#cell-1-0').fill('Name');
  await page.locator('#cell-1-1').fill('10');
  await page.locator('#cell-2-0').fill('Other');
  await page.locator('#cell-2-1').fill('20');

  await page.locator('#summary-max').click();

  // Summary row should appear in the editor table
  const summaryRow = page.locator('.editor-table .summary-row');
  await expect(summaryRow).toHaveCount(1);
});

test('summary max shows correct computed values in markdown', async ({ page }) => {
  await page.locator('#cell-1-1').fill('10');
  await page.locator('#cell-2-1').fill('20');

  await page.locator('#summary-max').click();

  await expect(page.locator('#md-output')).toHaveValue(/\*\*MAX\*\*/);
  await expect(page.locator('#md-output')).toHaveValue(/\*\*20\*\*/);
});

test('toggling summary off removes summary row', async ({ page }) => {
  await page.locator('#cell-1-1').fill('10');

  // Toggle on
  await page.locator('#summary-max').click();
  await expect(page.locator('.editor-table .summary-row')).toHaveCount(1);

  // Toggle off
  await page.locator('#summary-max').click();
  await expect(page.locator('.editor-table .summary-row')).toHaveCount(0);

  // Markdown should not contain MAX
  const md = await page.locator('#md-output').inputValue();
  expect(md).not.toContain('**MAX**');
});
