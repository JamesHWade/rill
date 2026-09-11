import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {chromium} from 'playwright';

const url = process.env.RILL_BROWSER_URL || 'http://127.0.0.1:3894';
assert.ok(['localhost', '127.0.0.1'].includes(new URL(url).hostname));
const browser = await chromium.launch({channel: 'chrome', headless: true});
const output = new URL('../../artifacts/orientation-entry/', import.meta.url);
await fs.mkdir(output, {recursive: true});
const results = [];
let page;
try {
  for (const width of [1440, 390]) {
    page = await browser.newPage({viewport: {width, height: 900}, reducedMotion: 'reduce'});
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    page.setDefaultTimeout(15000);
    async function audit() {
      await page.addScriptTag({path: new URL('./node_modules/axe-core/axe.min.js', import.meta.url).pathname});
      const result = await page.evaluate(() => axe.run(document, {runOnly: ['wcag2a', 'wcag2aa', 'wcag21aa']}));
      assert.deepEqual(result.violations.map(v => ({id: v.id, impact: v.impact})), []);
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth), false);
    }
    await page.goto(`${url}?feedback=fixture&resume=1`);
    await page.waitForFunction(() => window.rillUiAudit?.().appBusy === 'false');
    assert.equal(await page.locator('#reader-document').count(), 0,
      'A fresh visit does not reopen the article from an old completed answer');
    await page.locator('#rill-orientation').waitFor();
    await page.locator('.orientation-browse').click();
    const queueOrientation = page.locator('.story-pane').getByRole('button', {name: 'Open Orientation', exact: true});
    await queueOrientation.waitFor();
    assert.equal((await queueOrientation.innerText()).trim(), 'Orientation');
    assert.ok((await queueOrientation.boundingBox()).height >= 44);
    await audit();
    await page.screenshot({path: new URL(`${width}-queue.png`, output).pathname});
    await queueOrientation.click();
    await page.locator('#rill-orientation').waitFor();
    await page.locator('.orientation-browse').click();
    await page.locator('.story-card').first().click();
    await page.locator('#reader-document').waitFor();
    const articleOrientation = page.locator('.article-header').getByRole('button', {name: 'Open Orientation', exact: true});
    await articleOrientation.waitFor();
    await audit();
    await page.screenshot({path: new URL(`${width}-article.png`, output).pathname});
    await articleOrientation.click();
    await page.locator('#rill-orientation').waitFor();
    assert.equal(await page.locator('#reader-document').count(), 0);
    await page.locator('.orientation-browse').click();
    await page.locator('.story-card').first().click();
    await page.locator('#reader-document').waitFor();
    await page.evaluate(() => {
      const original = window.Shiny.setInputValue;
      window.Shiny.setInputValue = function (name, ...args) {
        if (name === 'show_orientation') {
          window.releaseOrientation = () => original.call(this, name, ...args);
          window.Shiny.setInputValue = original;
        } else return original.call(this, name, ...args);
      };
    });
    await page.locator('.article-header').getByRole('button', {name: 'Open Orientation', exact: true}).click();
    if (width < 768) await page.locator('.article-header .mobile-back').click();
    else await page.getByRole('radio', {name: 'All stories', exact: true}).check();
    await page.evaluate(() => window.releaseOrientation());
    await page.locator('#reader-document').waitFor({state: 'detached'});
    await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
    assert.equal(await page.locator('.story-pane').isVisible(), true, 'Returning to the queue wins over a late Orientation response');
    assert.equal(await page.locator('.reader-pane').isVisible(), false);
    if (width < 768) await page.locator('.compact-library-trigger').click();
    await page.getByRole('button', {name: 'Reopen last answer', exact: true}).click();
    await page.locator('#reader-document').waitFor();
    await page.getByRole('button', {name: 'Close Ask Rill', exact: true}).waitFor();
    await page.locator('.reader-agent-sidebar').getByRole('heading', {name: 'Source boundary', exact: true}).waitFor();
    await page.getByRole('button', {name: 'Close Ask Rill', exact: true}).click();
    await page.locator('.article-header').getByRole('button', {name: 'Open Orientation', exact: true}).click();
    await page.locator('#rill-orientation').waitFor();
    await page.reload();
    await page.waitForFunction(() => window.rillUiAudit?.().appBusy === 'false');
    assert.equal(await page.locator('#reader-document').count(), 0, 'Reload keeps the old answer opt-in');
    await audit();
    assert.deepEqual(errors, []);
    results.push({width, noAutomaticArticle: true, queueAndArticleOrientation: true, lastAnswerAvailable: true, violations: 0, errors});
    await page.close();
  }
  await fs.writeFile(new URL('results.json', output), JSON.stringify(results, null, 2) + '\n');
  console.log(JSON.stringify(results));
} catch (error) {
  console.log(await page.evaluate(() => ({ui: window.rillUiAudit(),
    header: document.getElementById('reader_header')?.innerText.slice(0, 180),
    orientation: Array.from(document.querySelectorAll('#rill-orientation')).map(el => ({parent: el.parentElement.id,
      visible: !!el.getClientRects().length, tag: el.tagName})),
    notifications: Array.from(document.querySelectorAll('.shiny-notification')).map(el => el.innerText)})));
  await page.screenshot({path: new URL('failure.png', output).pathname});
  throw error;
} finally {
  await browser.close();
}
