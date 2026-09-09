import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {chromium} from 'playwright';

const url = process.env.RILL_BROWSER_URL || 'http://127.0.0.1:3876';
assert.ok(['127.0.0.1', 'localhost'].includes(new URL(url).hostname));
const browser = await chromium.launch({channel: 'chrome', headless: true});

try {
  const page = await browser.newPage({
    viewport: {width: 1440, height: 1000},
    reducedMotion: 'reduce',
  });
  await page.goto(`${url}?tools=fixture`);
  await page.waitForFunction(() => window.rillUiAudit?.().appBusy === 'false');
  await page.getByRole('button', {name: 'Read the anchor source', exact: true}).click();
  await page.getByRole('button', {name: 'Focus Ask Rill', exact: true}).first().click();
  await page.waitForFunction(() =>
    document.getElementById('reader_agent_sidebar').getBoundingClientRect().width >= innerWidth - 1);
  await page.evaluate(() => Shiny.setInputValue('audit_document_result', 1, {priority: 'event'}));
  await page.getByText('Source Document', {exact: true}).click();
  const result = page.locator('.rill-document-result');
  await result.waitFor();
  assert.ok((await result.innerText()).includes('Shiny as a personal information surface'));
  await page.getByText('Original tool result (JSON)', {exact: true}).click();
  await fs.mkdir('../../artifacts/ui-followups-responsive', {recursive: true});
  await page.screenshot({path: '../../artifacts/ui-followups-responsive/tool-source-details.png'});
  const original = await result.locator('pre').textContent();
  const pinnedDocument = JSON.parse(original);
  assert.equal(pinnedDocument.title, 'Shiny as a personal information surface');
  const sourceLink = result.getByRole('link', {name: 'Original Source', exact: true});
  assert.equal(await sourceLink.getAttribute('href'), pinnedDocument.source_url);
  assert.equal(await sourceLink.isVisible(), true);
  await sourceLink.focus();
  assert.equal(await sourceLink.evaluate(element => document.activeElement === element), true);

  await page.context().grantPermissions(['clipboard-read', 'clipboard-write']);
  await page.getByRole('button', {name: 'Copy original JSON', exact: true}).click();
  await page.getByRole('status').filter({hasText: 'Copied.'}).waitFor();
  assert.equal(await page.evaluate(() => navigator.clipboard.readText()), original);
  await page.setViewportSize({width: 320, height: 700});
  await page.waitForFunction(() => !document.querySelector('.transitioning'));
  assert.equal(await page.evaluate(() => window.rillUiAudit().horizontalOverflow), false);
  console.log('Pinned source link, details and exact JSON copying passed');
} finally {
  await browser.close();
}
