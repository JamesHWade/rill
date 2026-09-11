import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {chromium} from 'playwright';

const url = process.env.RILL_BROWSER_URL || 'http://127.0.0.1:3894';
assert.ok(['127.0.0.1', 'localhost'].includes(new URL(url).hostname));
const output = new URL('../../artifacts/reader-swipes/', import.meta.url);
await fs.mkdir(output, {recursive: true});
const browser = await chromium.launch({channel: 'chrome', headless: true});
const page = await browser.newPage({viewport: {width: 390, height: 844}, hasTouch: true});
const touch = await page.context().newCDPSession(page);
const errors = [];
page.on('pageerror', error => errors.push(error.message));
page.setDefaultTimeout(15000);
await page.addInitScript(() => {
  window.swipeDebug = [];
  for (const type of ['pointerdown', 'pointermove', 'pointerup', 'pointercancel', 'lostpointercapture']) {
    document.addEventListener(type, event => window.swipeDebug.push({type, target: event.target.tagName,
      id: event.target.id, x: event.clientX, y: event.clientY, pointer: event.pointerId,
      distance: document.querySelector('.reader-scroll')?.style.getPropertyValue('--reader-swipe-distance'),
      selected: document.querySelector('#reader-document')?.dataset.entryId,
      selection: window.getSelection()?.toString().length}), {capture: true});
  }
});
const frame = () => page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
async function drag(selector, dx, release = true, dy = 0) {
  const element = page.locator(selector).first();
  await element.evaluate(el => el.scrollIntoView({block: 'center', behavior: 'instant'}));
  await page.evaluate(selector => new Promise((resolve, reject) => {
    const deadline = performance.now() + 3000;
    let previous = '', previousElement = null, stable = 0;
    function check() {
      const el = document.querySelector(selector);
      const box = el?.getBoundingClientRect();
      if (!box || !box.width || !box.height) {
        stable = 0;
        if (performance.now() > deadline) reject(new Error('Touch target disappeared'));
        else requestAnimationFrame(check);
        return;
      }
      const current = `${box.x},${box.y},${box.width},${box.height}`;
      stable = el === previousElement && current === previous &&
        !document.documentElement.classList.contains('shiny-busy') ? stable + 1 : 0;
      previous = current;
      previousElement = el;
      if (stable >= 18) resolve();
      else if (performance.now() > deadline) reject(new Error('Touch target kept moving'));
      else requestAnimationFrame(check);
    }
    requestAnimationFrame(check);
  }), selector);
  const box = await element.boundingBox();
  const x = dx < 0 ? Math.min(310, box.x + box.width - 25) : Math.max(70, box.x + 25);
  const y = Math.max(1, Math.min(page.viewportSize().height - 1, box.y + Math.min(30, box.height / 2)));
  assert.ok(await element.evaluate((el, {x, y}) => el.contains(document.elementFromPoint(x, y)), {x, y}),
    'Native touch starts inside the requested element');
  await page.evaluate(() => {window.swipeDebug = [];});
  await touch.send('Input.dispatchTouchEvent', {type: 'touchStart', touchPoints: [{x, y}]});
  for (let step = 1; step <= 5; step++) {
    await touch.send('Input.dispatchTouchEvent', {type: 'touchMove', touchPoints: [{x: x + dx * step / 5, y: y + dy * step / 5}]});
    await frame();
  }
  if (release) await touch.send('Input.dispatchTouchEvent', {type: 'touchEnd', touchPoints: []});
}
try {
  await page.goto(`${url}?stress=1`);
  await page.locator('.story-card').first().waitFor();
  const first = await page.locator('.story-card').first().getAttribute('data-entry-id');
  await drag('.story-card', -30);
  await page.waitForFunction(() => !document.querySelector('.story-row').classList.contains('is-swiping') &&
    document.querySelector('.story-row').style.getPropertyValue('--swipe-distance') === '0px');
  assert.equal(await page.locator('#queue-notice').isVisible(), false);
  await drag('.story-card', -120, false);
  await touch.send('Input.dispatchTouchEvent', {type: 'touchCancel', touchPoints: []});
  assert.equal(await page.locator('.story-row.is-pending').count(), 0);
  assert.equal(await page.locator('#queue-notice').isVisible(), false);
  await drag('.story-card', -180, false);
  const tracking = await page.locator('.story-front').first().evaluate(el => ({
    transform: getComputedStyle(el).transform,
    transition: getComputedStyle(el).transition,
    rowClass: el.parentElement.className,
    x: new DOMMatrixReadOnly(getComputedStyle(el).transform).m41
  }));
  await page.screenshot({path: new URL('queue-drag.png', output).pathname});
  await touch.send('Input.dispatchTouchEvent', {type: 'touchEnd', touchPoints: []});
  await page.getByText('Marked read', {exact: true}).waitFor();
  await page.locator('#queue-undo').click();
  await page.getByText('Marked unread', {exact: true}).waitFor();
  assert.equal(await page.locator(`[data-entry-id="${first}"].story-card`).count(), 1);
  console.log(JSON.stringify({queueTracking: tracking}));
  await page.locator('#queue-notice-dismiss').click();
  await page.locator('.story-card').first().click();
  await page.locator('#reader-document').waitFor();
  await drag('#reader-document p', 130);
  await frame();
  console.log(JSON.stringify({rightSwipeShowsQueue: await page.locator('.story-pane').isVisible()}));
  assert.ok(tracking.x <= -160, 'The card follows a 180px drag instead of hitting a short hard stop');
  assert.equal(await page.locator('.story-pane').isVisible(), true, 'Right swipe returns to the queue');
  assert.equal(await page.locator('.reader-pane').isVisible(), false, 'Right swipe closes the reading pane');

  await page.locator('.story-card').first().click();
  await page.locator('#reader-document').waitFor();
  const initialArticle = await page.locator('#reader-document').getAttribute('data-entry-id');
  await drag('#reader-document p', -30);
  assert.equal(await page.locator('#reader-document').getAttribute('data-entry-id'), initialArticle);
  await drag('#reader-document p', -120, false);
  await touch.send('Input.dispatchTouchEvent', {type: 'touchCancel', touchPoints: []});
  assert.equal(await page.locator('#reader-document').getAttribute('data-entry-id'), initialArticle);
  await drag('#reader-document p', -130, false);
  const readerTracking = await page.locator('#reader-document').evaluate(el => ({
    transform: getComputedStyle(el).transform,
    x: new DOMMatrixReadOnly(getComputedStyle(el).transform).m41,
    distance: getComputedStyle(el).getPropertyValue('--reader-swipe-distance')
  }));
  console.log(JSON.stringify({readerTracking}));
  if (readerTracking.x >= -40) console.log(await page.evaluate(() => window.swipeDebug));
  assert.ok(readerTracking.x < -40, 'The reading copy moves with a horizontal drag');
  await page.screenshot({path: new URL('reader-next-swipe.png', output).pathname});
  await touch.send('Input.dispatchTouchEvent', {type: 'touchEnd', touchPoints: []});
  await page.waitForFunction(id => document.querySelector('#reader-document')?.dataset.entryId !== id, initialArticle);
  const nextArticle = await page.locator('#reader-document').getAttribute('data-entry-id');
  assert.equal(await page.locator('.reader-pane').isVisible(), true);
  await drag('#reader-document p', 3, true, -130);
  assert.equal(await page.locator('#reader-document').getAttribute('data-entry-id'), nextArticle);
  await drag('#reader-document pre', -150);
  assert.equal(await page.locator('#reader-document').getAttribute('data-entry-id'), nextArticle);
  await page.waitForFunction(() => document.querySelector('#reader-document pre').scrollLeft > 0,
    null, {timeout: 2000});

  await page.evaluate(() => {
    const original = window.Shiny.setInputValue;
    window.Shiny.setInputValue = function (name, ...args) {
      if (name === 'select_entry') window.releaseSelection = () => original.call(this, name, ...args);
      else return original.call(this, name, ...args);
    };
  });
  await drag('#reader-document p', -130);
  await drag('#reader-document p', 130);
  assert.equal(await page.locator('.story-pane').isVisible(), true);
  await page.evaluate(() => window.releaseSelection());
  await page.waitForFunction(id => document.querySelector('#reader-document')?.dataset.entryId !== id, nextArticle);
  assert.equal(await page.locator('.story-pane').isVisible(), true, 'Returning to the queue wins over a late article response');
  assert.equal(await page.locator('.reader-pane').isVisible(), false);
  console.log('Article swipes advance, return, cancel, and preserve vertical and code scrolling.');

  await page.goto(`${url}?feedback=fixture&resume=unsubscribed`);
  await page.locator('.compact-library-trigger').click();
  await page.getByRole('button', {name: 'Reopen last answer', exact: true}).click();
  await page.locator('#reader-document').waitFor();
  await page.getByRole('button', {name: 'Close Ask Rill', exact: true}).click();
  await page.waitForFunction(() => {
    const layout = document.getElementById('reader_agent_sidebar').parentElement;
    return layout.classList.contains('sidebar-collapsed') && !layout.classList.contains('transitioning');
  });
  const recoveredArticle = await page.locator('#reader-document').getAttribute('data-entry-id');
  assert.equal(await page.locator('.reader-next').isDisabled(), true);
  assert.equal(await page.locator('.story-card.is-selected').count(), 0);
  assert.ok(await page.locator('.story-card').count() > 0);
  await page.evaluate(() => {
    window.unavailableSelectionAttempts = [];
    const original = window.Shiny.setInputValue;
    window.Shiny.setInputValue = function (name, value, ...args) {
      if (name === 'select_entry') window.unavailableSelectionAttempts.push(value);
      return original.call(this, name, value, ...args);
    };
  });
  await drag('#reader-document p', -130, false);
  assert.equal(await page.locator('.reader-scroll').getAttribute('data-swipe-label'), 'End of queue');
  assert.equal(await page.locator('.is-reader-swipe-armed').count(), 0);
  await touch.send('Input.dispatchTouchEvent', {type: 'touchEnd', touchPoints: []});
  await frame();
  assert.deepEqual(await page.evaluate(() => window.unavailableSelectionAttempts), [],
    'An unavailable Next swipe does not request another story');
  assert.equal(await page.locator('#reader-document').getAttribute('data-entry-id'), recoveredArticle);
  await drag('#reader-document p', 130);
  assert.equal(await page.locator('.story-pane').isVisible(), true,
    'A recovered answer outside the queue can still swipe back to the queue');
  console.log('Unavailable Next swipes retain the recovered answer; right swipes still return to the queue.');

  await page.goto(`${url}?stress=1`);
  for (let cycle = 0; cycle < 8; cycle++) {
    const card = page.locator('.story-card').first();
    await card.waitFor();
    const entryId = await card.getAttribute('data-entry-id');
    await card.click();
    await page.waitForFunction(id => document.querySelector('#reader-document')?.dataset.entryId === id, entryId);
    await drag('#reader-document p', -130);
    await page.waitForFunction(id => document.querySelector('#reader-document')?.dataset.entryId !== id, entryId);
    await drag('#reader-document p', 130);
    assert.equal(await page.locator('.story-pane').isVisible(), true, `Next/back cycle ${cycle + 1}`);
  }
  console.log('Eight consecutive native next/back cycles passed.');

  await page.goto(`${url}?stress=1`);
  await page.locator('.story-read').first().click();
  await page.getByText('Marked read', {exact: true}).waitFor();
  await page.locator('#queue-undo').focus();
  await page.waitForTimeout(8500);
  assert.equal(await page.locator('#queue-notice').isVisible(), true, 'Focused Undo pauses expiry');
  await page.locator('.story-card').first().focus();
  await page.mouse.move(1, 1);
  await page.waitForFunction(() => document.getElementById('queue-notice').hidden, null, {timeout: 10000});
  console.log('Success notices expire after eight seconds and pause for keyboard Undo.');

  await page.goto(`${url}?stress=1`);
  await page.locator('.story-read').first().waitFor();
  await page.evaluate(() => {
    const original = window.Shiny.setInputValue;
    window.Shiny.setInputValue = function (name, value, ...args) {
      if (name === 'queue_action') value = {...value, entry_id: 'unavailable-fixture-entry'};
      return original.call(this, name, value, ...args);
    };
  });
  await page.locator('.story-read').first().click();
  await page.getByText("Couldn't update this story. Please try again.", {exact: true}).waitFor();
  await page.waitForTimeout(8500);
  assert.equal(await page.locator('#queue-notice').isVisible(), true, 'Errors remain available for recovery');
  assert.equal(await page.locator('.story-row.is-pending').count(), 0);
  assert.equal(await page.locator('.story-row.is-read').count(), 0);

  await page.goto(`${url}?stress=1`);
  await page.locator('.story-card').first().waitFor();
  await page.evaluate(() => {
    const original = window.Shiny.setInputValue;
    window.Shiny.setInputValue = function (name, ...args) {
      if (name !== 'queue_action') return original.call(this, name, ...args);
    };
  });
  await drag('.story-card', -180);
  await page.locator('.story-row.is-pending').waitFor();
  await page.evaluate(() => window.Shiny.setInputValue('audit_disconnect', Math.random(), {priority: 'event'}));
  await page.getByText("Connection lost. Check the story's status after reconnecting.", {exact: true}).waitFor();
  assert.equal(await page.locator('.story-row.is-pending').count(), 0);
  assert.equal(await page.locator('.story-row').first().evaluate(el => el.style.getPropertyValue('--swipe-distance')), '0px');
  console.log('Rejected and disconnected actions restore the row and retain recovery feedback.');
  assert.deepEqual(errors, []);
  await fs.writeFile(new URL('results.json', output), JSON.stringify({queueTracking: tracking, readerTracking,
    unavailableNextRetainsAnswer: true, errors}, null, 2) + '\n');
} catch (error) {
  await page.screenshot({path: new URL('failure.png', output).pathname});
  console.log(await page.evaluate(() => ({events: window.swipeDebug,
    surface: document.querySelector('.app-shell')?.dataset.compactSurface,
    selected: document.querySelector('#reader-document')?.dataset.entryId,
    nextDisabled: document.querySelector('.reader-next')?.disabled,
    queueSelection: document.querySelector('.story-card.is-selected')?.dataset.entryId})));
  throw error;
} finally {
  await browser.close();
}
