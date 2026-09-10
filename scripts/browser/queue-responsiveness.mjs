import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {chromium} from 'playwright';
const url = process.env.RILL_BROWSER_URL || 'http://127.0.0.1:3890';
assert.ok(['127.0.0.1', 'localhost'].includes(new URL(url).hostname));
const output = new URL('../../artifacts/queue-responsiveness/', import.meta.url);
await fs.mkdir(output, {recursive: true});
const browser = await chromium.launch({channel: 'chrome', headless: true});
const page = await browser.newPage({viewport: {width: 1440, height: 1000}});
page.setDefaultTimeout(15000);
const errors = [];
page.on('pageerror', error => errors.push(error.message));
try {
  await page.goto(`${url}/?stress=1`);
  await page.waitForFunction(() => document.querySelectorAll('.story-row').length === 30);
  await page.evaluate(() => {
    window.queueTimings = [];
    window.addEventListener('rill:queue-visible', event => window.queueTimings.push(event.detail));
  });
  for (const [label, view] of [['Today','today'], ['All stories','all'], ['Today','today']]) {
    await page.getByRole('radio', {name: label, exact: true}).check();
    await page.waitForFunction(view => document.querySelector('.queue-batch')?.dataset.queueView === view &&
      document.querySelector('#story_list').getAttribute('aria-busy') === 'false', view);
    assert.equal(await page.locator('.story-row').count(), 30);
    assert.equal(await page.locator('.reader-pane').isVisible(), false);
    assert.ok((await page.locator('.story-pane').boundingBox()).width > 900);
  }
  await page.locator('.rill-skip-link').focus();
  await page.keyboard.press('Enter');
  await page.waitForFunction(() => document.activeElement?.classList.contains('story-card'));
  console.log('view transitions and skip link passed');
  await page.screenshot({path: new URL('1440-queue-primary.png', output).pathname});
  await page.evaluate(() => { window.retainedRow = document.querySelector('.story-row'); });
  await page.getByRole('button', {name: 'Show 30 more stories'}).click();
  await page.waitForFunction(() => document.querySelectorAll('.story-row').length === 60);
  assert.equal(await page.evaluate(() => window.retainedRow === document.querySelector('.story-row')), true);
  console.log('load more passed');
  await page.locator('.story-save').first().click();
  await page.getByText('Saved for later', {exact:true}).waitFor();
  await page.getByRole('radio', {name: 'Unread', exact: true}).check();
  await page.waitForFunction(() => document.querySelector('.queue-batch')?.dataset.queueView === 'unread' &&
    document.querySelector('#story_list').getAttribute('aria-busy') === 'false');
  await page.evaluate(() => { window.retainedRow = document.querySelectorAll('.story-row')[1]; });
  await page.locator('.story-read').first().click();
  await page.getByText('Marked read', {exact:true}).waitFor();
  assert.equal(await page.evaluate(() => window.retainedRow === document.querySelector('.story-row')), true);
  assert.equal(await page.evaluate(() => window.retainedRow.dataset.queueIndex), '1');
  await page.locator('#queue-undo').click();
  await page.getByText('Marked unread', {exact:true}).waitFor();
  await page.getByRole('radio', {name: 'All stories', exact: true}).check();
  await page.waitForFunction(() => document.querySelectorAll('.story-row').length === 30);
  await page.waitForFunction(() => document.querySelector('#story_list').getAttribute('aria-busy') === 'false');
  await page.locator('.story-card').first().click();
  await page.locator('#reader-document').waitFor();
  assert.equal(await page.locator('.reader-pane').isVisible(), true);
  const timings = await page.evaluate(() => window.queueTimings);
  assert.equal(errors.length, 0, errors.join('\n'));
  await fs.writeFile(new URL('verification.json', output), JSON.stringify({timings, errors}, null, 2));
  console.log(JSON.stringify({timings, errors}));
} finally { await browser.close(); }
