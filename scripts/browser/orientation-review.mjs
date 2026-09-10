import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {chromium} from 'playwright';
const url = process.env.RILL_BROWSER_URL || 'http://127.0.0.1:3892';
assert.ok(['localhost', '127.0.0.1'].includes(new URL(url).hostname));
const output = new URL('../../artifacts/orientation-review/', import.meta.url);
await fs.mkdir(output, {recursive: true});
const browser = await chromium.launch({channel: 'chrome', headless: true});
const results = [];
try {
  for (const width of [1440, 390]) {
    const page = await browser.newPage({viewport: {width, height: 1000}});
    page.setDefaultTimeout(15000);
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.goto(url);
    await page.locator('.orientation-theme').first().waitFor();
    assert.equal(await page.locator('.orientation-step').count(), 3);
    assert.match(await page.locator('.orientation-totals').innerText(), /6 unread in total/);
    const excerpts = await page.locator('.orientation-step').evaluateAll(cards => cards.map(card => ({
      lead: card.querySelector('.orientation-evidence-lead').textContent,
      full: card.querySelector('.orientation-evidence blockquote').textContent
    })));
    assert.ok(excerpts.every(({lead, full}) => full.includes(lead)));
    await page.addScriptTag({path: new URL('./node_modules/axe-core/axe.min.js', import.meta.url).pathname});
    const audit = await page.evaluate(() => axe.run(document, {runOnly: ['wcag2a', 'wcag2aa', 'wcag21aa']}));
    if (audit.violations.length) console.log(JSON.stringify(audit.violations.map(v => ({id: v.id, nodes: v.nodes.map(n => ({target: n.target, summary: n.failureSummary}))}))));
    assert.deepEqual(audit.violations.map(v => ({id: v.id, impact: v.impact})), []);
    assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth), false);
    await page.screenshot({path: new URL(`${width}-orientation.png`, output).pathname, fullPage: true});
    await page.getByRole('button', {name: 'Digests and releases (2 unread stories)', exact: true}).click();
    await page.waitForFunction(() => document.querySelectorAll('.story-card').length === 2);
    if (width < 1000) await page.locator('.compact-library-trigger').click();
    await page.locator('#feed_nav button').filter({hasText: /^\s*R\s*4\s*$/}).click();
    await page.waitForFunction(() => document.querySelectorAll('.story-card').length === 4);
    assert.deepEqual(errors, []);
    results.push({width, picks: 3, unreadTotal: 6, themeStories: 2, groupStories: 4, violations: 0, errors});
    await page.close();
  }
  await fs.writeFile(new URL('results.json', output), JSON.stringify(results, null, 2) + '\n');
  console.log(JSON.stringify(results));
} finally {
  await browser.close();
}
