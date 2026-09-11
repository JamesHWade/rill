import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {chromium} from 'playwright';

const url = process.env.RILL_BROWSER_URL || 'http://127.0.0.1:3894';
assert.ok(['127.0.0.1', 'localhost'].includes(new URL(url).hostname));
const browser = await chromium.launch({channel: 'chrome', headless: true});
const page = await browser.newPage({viewport: {width: 1280, height: 900}});
try {
  await page.goto(`${url}?stress=1`);
  await page.locator('.story-card').first().waitFor();
  await page.evaluate(() => {
    window.readerBenchmark = {opening: [], marking: []};
    let pending = null;
    document.addEventListener('click', event => {
      const card = event.target.closest('.story-card');
      const read = event.target.closest('.story-read');
      const next = event.target.closest('.reader-next');
      if (!card && !read && !next) return;
      const cards = Array.from(document.querySelectorAll('.story-card'));
      const target = next ? cards[cards.findIndex(item => item.classList.contains('is-selected')) + 1] : card || read;
      pending = {started: performance.now(), id: target.dataset.entryId,
        kind: read ? 'marking' : 'opening', scheduled: false};
    }, true);
    new MutationObserver(() => {
      const current = pending;
      if (!current || current.scheduled) return;
      const ready = current.kind === 'opening'
        ? document.querySelector('#reader-document')?.dataset.entryId === current.id
        : !document.querySelector('#queue-notice')?.hidden &&
          (!document.querySelector(`.story-row[data-entry-id="${current.id}"]`) ||
            document.querySelector(`.story-row[data-entry-id="${current.id}"]`).classList.contains('is-read'));
      if (!ready) return;
      current.scheduled = true;
      requestAnimationFrame(() => requestAnimationFrame(() => {
        window.readerBenchmark[current.kind].push(performance.now() - current.started);
        if (pending === current) pending = null;
      }));
    }).observe(document.getElementById('rill-app'), {subtree: true, childList: true, attributes: true});
  });
  for (let index = 0; index < 7; index++) {
    if (process.env.RILL_BENCHMARK_NEXT === 'true' && index > 0) await page.locator('.reader-next').click();
    else await page.locator('.story-card').nth(index).click();
    await page.waitForFunction(n => window.readerBenchmark.opening.length === n, index + 1);
    await page.waitForTimeout(400);
  }
  for (let index = 7; index < 14; index++) {
    await page.locator('.story-read').nth(index).click();
    await page.waitForFunction(n => window.readerBenchmark.marking.length === n, index - 6);
    await page.locator('#queue-notice-dismiss').click();
    await page.waitForTimeout(400);
  }
  const result = await page.evaluate(() => window.readerBenchmark);
  const output = `artifacts/reader-swipes/${process.env.RILL_BENCHMARK_LABEL || 'current'}-latency.json`;
  await fs.mkdir('artifacts/reader-swipes', {recursive: true});
  await fs.writeFile(output, JSON.stringify(result, null, 2) + '\n');
  console.log(JSON.stringify(result));
} finally {
  await browser.close();
}
