import assert from 'node:assert/strict';
import {chromium} from 'playwright';
const url = process.env.RILL_BROWSER_URL || 'http://127.0.0.1:3890';
assert.ok(['127.0.0.1', 'localhost'].includes(new URL(url).hostname));
const browser = await chromium.launch({channel: 'chrome', headless: true});
const page = await browser.newPage({viewport: {width: 1440, height: 1000}, hasTouch: true});
page.setDefaultTimeout(15000);
const errors = [];
page.on('pageerror', error => errors.push(error.message));
const count = n => page.waitForFunction(n => document.querySelectorAll('.story-card').length === n, n);
const selected = n => page.waitForFunction(n => document.querySelectorAll('.story-card')[n]?.classList.contains('is-selected'), n);
async function allStories() {
  await page.goto(`${url}/?stress=1`);
  await count(30);
  await page.getByRole('radio', {name: 'All stories', exact: true}).check();
  await page.waitForFunction(() => document.querySelector('.queue-batch')?.dataset.queueView === 'all' &&
    document.querySelector('#story_list').getAttribute('aria-busy') === 'false');
}
try {
  await allStories();
  await page.locator('.story-card').nth(29).click();
  await selected(29);
  await page.locator('#reader-document').waitFor();
  await page.locator(".reader-next").click();
  await count(60);
  await selected(30);
  assert.equal(await page.locator('.reader-pane').isVisible(), true);
  await page.locator(".reader-previous").click();
  await selected(29);
  await page.locator('.story-card').nth(59).click();
  await selected(59);
  await page.locator('#reader-document').waitFor();
  await page.locator('#reader-document').focus();
  await page.keyboard.press('j');
  await count(90);
  await selected(60);
  console.log('Reader buttons and keyboard navigation cross batch boundaries.');

  await allStories();
  await page.locator('.story-card').nth(29).click();
  await selected(29);
  await page.evaluate(() => {
    const original = window.Shiny.setInputValue;
    window.Shiny.setInputValue = function (name, ...args) {
      if (name === 'queue_more') window.releaseBatch = () => original.call(this, name, ...args);
      else return original.call(this, name, ...args);
    };
  });
  await page.locator('.reader-next').click();
  await page.locator('.reader-previous').click();
  await selected(28);
  await page.evaluate(() => window.releaseBatch());
  await count(60);
  await selected(28);
  console.log('Changing direction cancels pending batch navigation.');

  await allStories();
  for (const n of [60, 90, 120, 150]) {
    await page.locator('#queue_more').focus();
    await page.keyboard.press('Enter');
    await count(n);
    await page.waitForFunction(n => n < 150 ? document.activeElement?.id === 'queue_more' :
      document.activeElement === document.querySelectorAll('.story-card')[120], n);
  }
  console.log('Keyboard focus survives every batch, including the final batch.');
  await page.locator('.story-card').last().click();
  await selected(149);
  assert.equal(await page.evaluate(() => window.rillMoveStory(1)), false);
  assert.equal(await page.locator('.reader-next').isDisabled(), true);

  await allStories();
  await page.evaluate(() => {
    Object.defineProperty(document, 'activeElement', {configurable: true, get: () => null});
    window.Shiny.setInputValue('queue_more', Math.random(), {priority: 'event'});
  });
  await count(60);
  await page.evaluate(() => { delete document.activeElement; });
  console.log('Reconciliation tolerates a null active element.');

  await allStories();
  await page.getByRole('radio', {name: 'Unread', exact: true}).check();
  await page.waitForFunction(() => document.querySelector('.queue-batch')?.dataset.queueView === 'unread' &&
    !document.querySelector('#story_list').classList.contains('queue-changing'));
  await page.locator('.story-read').first().click();
  await page.getByText('Marked read', {exact: true}).waitFor();
  const boundaryId = await page.locator('.story-card').nth(29).getAttribute('data-entry-id');
  await page.locator('.story-card').nth(29).click();
  await selected(29);
  await page.locator('#queue-undo').click();
  await page.getByText('Marked unread', {exact: true}).waitFor();
  await count(31);
  await selected(30);
  assert.equal(await page.locator('.story-card.is-selected').getAttribute('data-entry-id'), boundaryId);
  assert.equal(await page.locator('.reader-previous').isEnabled(), true);
  assert.equal(await page.locator('.reader-next').isEnabled(), true);
  await page.locator('.reader-next').click();
  await count(61);
  await selected(31);
  console.log('Undo before a boundary selection preserves the reader and both navigation buttons.');

  await allStories();
  await page.setViewportSize({width: 390, height: 844});
  await page.locator('.story-card').nth(29).click();
  await selected(29);
  await page.locator('#reader-document').waitFor();
  const cdp = await page.context().newCDPSession(page);
  await page.waitForFunction(() => !document.querySelector('.transitioning') &&
    !document.documentElement.classList.contains('shiny-busy'));
  await page.locator('#reader-document p').first().evaluate(el =>
    el.scrollIntoView({block: 'center', behavior: 'instant'}));
  await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
  const point = await page.locator('#reader-document p').first().evaluate(el => {
    const box = el.getBoundingClientRect();
    return {x: Math.min(310, box.right - 30),
      y: Math.max(1, Math.min(innerHeight - 1, box.top + Math.min(30, box.height / 2)))};
  });
  assert.equal(await page.locator('#reader-document p').first().evaluate(
    (el, point) => el.contains(document.elementFromPoint(point.x, point.y)), point), true);
  await cdp.send('Input.dispatchTouchEvent', {type: 'touchStart', touchPoints: [point]});
  await cdp.send('Input.dispatchTouchEvent', {type: 'touchMove', touchPoints: [{x: point.x - 110, y: point.y}]});
  await cdp.send('Input.dispatchTouchEvent', {type: 'touchEnd', touchPoints: []});
  await count(60);
  await selected(30);
  await cdp.detach();
  await page.setViewportSize({width: 1440, height: 1000});
  console.log('Native Chromium reader swipe crosses the batch boundary.');

  for (const mode of ['absent', 'throws']) {
    await allStories();
    await page.evaluate(mode => {
      Object.defineProperty(window, 'crypto', {configurable: true, value: mode === 'absent' ? undefined : {
        getRandomValues() { throw new Error('crypto unavailable'); }
      }});
    }, mode);
    await page.getByRole('radio', {name: 'Today', exact: true}).check();
    await page.waitForFunction(() => document.querySelector('.queue-batch')?.dataset.queueView === 'today' &&
      document.querySelector('#story_list').getAttribute('aria-busy') === 'false');
    assert.match(await page.locator('.queue-batch').getAttribute('data-queue-request'), /^[a-f0-9]{32}$/);
  }
  console.log('Queue transitions work with absent and throwing crypto.');

  await allStories();
  await page.evaluate(() => {
    const original = window.Shiny.setInputValue;
    window.Shiny.setInputValue = function (name, ...args) {
      if (name !== 'queue_view_request') return original.call(this, name, ...args);
    };
  });
  await page.getByRole('radio', {name: 'Today', exact: true}).check();
  await page.waitForFunction(() => document.querySelector('#story_list').getAttribute('aria-busy') === 'true');
  await page.waitForFunction(() => !document.querySelector('#story_list').classList.contains('queue-changing') &&
    document.querySelector('#story_list').getAttribute('aria-busy') === 'false', null, {timeout: 20000});
  assert.equal(await page.locator('#story_list').evaluate(el => el.classList.contains('queue-changing')), false);
  await page.locator('.story-card').first().click();
  await page.locator('#reader-document').waitFor();
  console.log('Missing acknowledgements release the queue after the timeout.');
  assert.deepEqual(errors, []);
} finally {
  await browser.close();
}
