import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import { createRequire } from 'node:module';
import { chromium } from 'playwright';
const require = createRequire(import.meta.url);
const url = process.env.RILL_BROWSER_URL || 'http://127.0.0.1:3876';
assert.ok(['127.0.0.1', 'localhost'].includes(new URL(url).hostname), 'Use a local demo server');
const output = path.resolve(process.env.RILL_BROWSER_OUTPUT || '../../artifacts/responsive-audit');
await fs.mkdir(output, { recursive: true });
const browser = await chromium.launch({ channel: 'chrome', headless: true });
const context = await browser.newContext({ viewport: { width: 390, height: 844 }, hasTouch: true });
const page = await context.newPage();
page.setDefaultTimeout(15000);
const results = [];
const errors = [];
page.on('pageerror', error => errors.push(error.message));
async function ready() {
  await page.waitForFunction(() => window.rillUiAudit?.().appBusy === 'false');
}
async function audit(state, width) {
  console.log(`Checking ${state} at ${width}`);
  await page.evaluate(async () => {
    await Promise.all(document.getAnimations().filter(a => Number.isFinite(a.effect.getComputedTiming().iterations)).map(a => a.finished.catch(() => {})));
  });
  await page.addScriptTag({ path: require.resolve('axe-core/axe.min.js') });
  const result = await page.evaluate(async () => ({
    ui: window.rillUiAudit(),
    accessibility: (await axe.run(document, { runOnly: { type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa'] } })).violations.map(v => ({ id: v.id, impact: v.impact, help: v.help, nodes: v.nodes.map(n => ({ target: n.target, summary: n.failureSummary })) }))
  }));
  results.push({ state, width, ...result });
  assert.equal(result.ui.horizontalOverflow, false, `${state} overflow at ${width}`);
  await page.screenshot({ path: path.join(output, `${width}-${state}.png`) });
}
const touchSession = await context.newCDPSession(page);
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
  await touchSession.send('Input.dispatchTouchEvent', {type: 'touchStart', touchPoints: [{x, y}]});
  for (let step = 1; step <= 5; step++) {
    await touchSession.send('Input.dispatchTouchEvent', {type: 'touchMove', touchPoints: [{x: x + dx * step / 5, y: y + dy * step / 5}]});
    await frame();
  }
  if (release) await touchSession.send('Input.dispatchTouchEvent', {type: 'touchEnd', touchPoints: []});
}
try {
  await page.goto(url);
  await ready();
  await audit("orientation", 390);
  await page.evaluate(() => window.rillOpenLibrary());
  await page.getByRole('radio', { name: 'All stories', exact: true }).check();
  await page.getByRole('heading', { name: 'All stories', exact: true }).waitFor();
  await page.waitForFunction(() => !document.querySelector('.shiny-busy'));
  await page.waitForTimeout(200);
  await page.evaluate(() => window.rillOpenQueue());
  await drag('.story-card', -140, false);
  assert.equal(await page.locator('.story-swipe-tray').first().getAttribute('aria-hidden'), 'false');
  assert.equal(await page.locator('#reader-document').count(), 0, 'Swipe does not open article');
  await touchSession.send('Input.dispatchTouchEvent', {type: 'touchCancel', touchPoints: []});
  assert.equal(await page.locator('.story-swipe-tray').first().getAttribute('aria-hidden'), 'true');
  await page.locator('.story-save').first().focus();
  await page.keyboard.press('Enter');
  await page.locator('.story-save[aria-pressed="true"]').first().waitFor();
  await page.waitForFunction(() => document.activeElement.textContent.trim() === 'Saved');
  await page.locator('.story-read').first().click();
  await page.getByText('Marked read', {exact: true}).waitFor();
  assert.equal(await page.locator('#reader-document').count(), 0, 'Mark read does not open article');
  await page.locator('#queue-undo').click();
  await page.getByText('Marked unread', {exact: true}).waitFor();
  await page.locator('#queue-notice-dismiss').click();
  await page.locator('.story-card').first().click();
  await page.waitForSelector('#reader-document');
  const first = await page.locator('#reader-document').getAttribute('data-entry-id');
  await drag('#reader-document p', -130);
  await page.waitForFunction(id => document.querySelector('#reader-document')?.dataset.entryId !== id, first);
  assert.notEqual(await page.locator('#reader-document').getAttribute('data-entry-id'), first, 'Swipe navigates to next story');
  for (const width of [320, 390, 430, 768, 1024, 1440]) {
    await page.setViewportSize({ width, height: 900 });
    await page.evaluate(() => window.rillOpenQueue());
    await audit('queue', width);
    await page.locator('.story-card').first().click();
    await page.waitForSelector('#reader-document');
    await audit('reading', width);
    if (width < 768) {
      await page.evaluate(() => window.rillOpenLibrary());
      await audit('library', width);
    }
  }
  await page.setViewportSize({ width: 390, height: 844 });
  await page.evaluate(() => window.rillOpenLibrary());
  await page.getByRole('button', { name: 'Manage feeds', exact: true }).click();
  await page.getByRole('dialog').waitFor();
  await audit('manage-feeds', 390);
  await page.waitForFunction(() => document.querySelector('.modal.show')?.contains(document.activeElement));
  await page.keyboard.press('Escape');
  await page.getByRole('dialog').waitFor({state: 'hidden'});
  await page.waitForFunction(() => document.activeElement.id === 'manage_feeds');
  await page.keyboard.press('Enter');
  await page.getByRole('dialog').waitFor();
  await page.getByRole('button', {name: 'Done', exact: true}).click();
  await page.getByRole('dialog').waitFor({state: 'hidden'});
  await page.waitForFunction(() => document.activeElement.id === 'manage_feeds');
  await page.getByRole('radio', { name: 'Starred', exact: true }).check();
  await page.getByRole('heading', { name: 'No starred stories yet', exact: true }).waitFor();
  await audit('empty', 390);
  await page.evaluate(() => window.rillOpenLibrary());
  await page.getByRole('radio', { name: 'All stories', exact: true }).check();
  await page.getByRole('heading', { name: 'All stories', exact: true }).waitFor();
  await page.locator('.story-card').first().click();
  await page.waitForSelector('#reader-document');
  await page.getByRole('button', { name: 'Ask Rill', exact: true }).click();
  await audit('ask-rill', 390);
  await page.keyboard.press('Escape');
  await page.emulateMedia({ colorScheme: 'dark', reducedMotion: 'reduce' });
  await audit('dark-reading-reduced-motion', 390);
  await page.evaluate(() => window.rillOpenQueue());
  await audit('dark-queue-reduced-motion', 390);
  await page.evaluate(() => window.rillOpenLibrary());
  await audit('dark-library-reduced-motion', 390);
  await page.getByRole('button', {name: 'Manage feeds', exact: true}).click();
  await page.getByRole('dialog').waitFor();
  await page.waitForFunction(() => document.querySelector('.modal.show')?.contains(document.activeElement));
  await audit('dark-manage-feeds', 390);
  await page.locator('#managed_feed-selectized').click();
  await page.waitForFunction(() => document.getElementById('managed_feed-selectized')?.getAttribute('aria-expanded') === 'true');
  assert.equal(await page.locator('#managed_feed-selectized').evaluate(input =>
    document.getElementById(input.getAttribute('aria-controls'))?.getAttribute('role')),
    'listbox', 'Expanded feed picker identifies its controlled listbox');
  await audit('dark-feed-picker-expanded', 390);
  for (let step = 0; step < 40; step++) {
    if (await page.evaluate(() => document.activeElement?.id === 'add_feed_groups')) break;
    await page.keyboard.press('Tab');
  }
  assert.equal(await page.locator('#add_feed_groups').evaluate(el => el === document.activeElement && el.matches(':focus-visible')), true, 'Group action receives keyboard-visible focus');
  await audit('dark-group-action-focused', 390);
  await page.getByRole('button', {name: 'Done', exact: true}).click();
  await page.getByRole('dialog').waitFor({state: 'hidden'});
  await page.setViewportSize({ width: 320, height: 225 });
  await page.evaluate(() => window.rillOpenQueue());
  await audit('400-percent-reflow-equivalent', 320);
  await page.locator('.story-card').first().click();
  await page.waitForSelector('#reader-document');
  await page.getByRole('button', {name: 'Queue', exact: true}).click();
  await page.locator('.compact-library-trigger').click();
  await page.getByRole('button', {name: 'Manage feeds', exact: true}).click();
  await page.getByRole('dialog').waitFor();
  await page.getByRole('button', {name: 'Done', exact: true}).click();
  await page.setViewportSize({ width: 320, height: 900 });
  await page.goto(`${url}?stress=1`);
  await ready();
  await page.evaluate(() => window.rillOpenQueue());
  await audit('long-queue', 320);
  await page.locator('.story-card').first().click();
  await page.waitForSelector('#reader-document');
  await audit('long-reading', 320);
  await page.setViewportSize({ width: 390, height: 844 });
  await page.evaluate(() => Shiny.setInputValue('audit_error', Date.now()));
  await page.locator('.shiny-notification-error').waitFor();
  await audit('native-error', 390);
  await page.evaluate(() => Shiny.setInputValue('audit_disconnect', Date.now()));
  await page.waitForFunction(() => window.rillUiAudit().connectionState === 'disconnected');
  await context.setOffline(true);
  await page.waitForFunction(() => ['offline', 'disconnected'].includes(window.rillUiAudit().connectionState));
  await audit('disconnected', 390);
  await context.setOffline(false);
  await page.reload();
  await ready();
  await audit('recovered', 390);
  await page.emulateMedia({ colorScheme: 'light', reducedMotion: 'reduce' });
  for (const width of [390, 1440]) {
    await page.setViewportSize({ width, height: 900 });
    await page.goto(`${url}?delay=3`);
    await page.locator('#rill-system-status:not([hidden])').waitFor();
    await audit('loading', width);
    await ready();
    await page.goto(`${url}?agent=fixture`);
    await ready();
    await page.evaluate(() => window.rillOpenQueue());
    await page.locator('.story-card').first().click();
    await page.waitForSelector('#reader-document');
    await page.getByRole('button', { name: 'Ask Rill', exact: true }).click();
    await page.evaluate(() => Shiny.setInputValue('audit_agent_status', 'running'));
    await page.locator('.reader-agent-run-status').waitFor();
    await audit('agent-running', width);
    await page.evaluate(() => Shiny.setInputValue('audit_agent_stream', Date.now()));
    await page.getByText('This is a synthetic response for browser validation.', {exact: false}).waitFor();
    await audit('agent-completed', width);
    await page.evaluate(() => Shiny.setInputValue('audit_agent_status', 'interrupted'));
    await page.getByRole('button', {name: 'Retry', exact: true}).waitFor();
    await audit('agent-interrupted', width);
    for (const access of ['pending', 'denied']) {
      await page.goto(`${url}?access=${access}`);
      await page.getByRole('dialog').waitFor();
      await page.locator('#rill-system-status').waitFor({state: 'hidden'});
      await audit(`access-${access}`, width);
    }
  }
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto(`${url}?delay=17`);
  await page.getByText('Still opening Rill', {exact: true}).waitFor({timeout: 20000});
  await audit('slow-startup', 390);
  await ready();
  await audit('slow-startup-recovered', 390);
  let interruptStartup = true;
  await page.routeWebSocket('**/*', socket => {
    if (interruptStartup) socket.close({code: 1011, reason: 'Synthetic startup interruption'});
    else socket.connectToServer();
  });
  await page.goto(url);
  await page.getByRole('button', {name: 'Reload Rill', exact: true}).waitFor({timeout: 20000});
  await audit('startup-interrupted', 390);
  interruptStartup = false;
  await page.getByRole('button', {name: 'Reload Rill', exact: true}).click();
  await ready();
  await audit('startup-recovered', 390);
  assert.deepEqual(errors, [], 'No browser errors');
  assert.deepEqual(results.flatMap(r => r.accessibility), [], 'No WCAG A/AA automated violations');
} catch (error) {
  console.error(error);
  console.log(await page.evaluate(() => ({audit: window.rillUiAudit(), shell: document.getElementById("rill-app").className, layers: document.elementsFromPoint(48,40).map(e=>({tag:e.tagName,cl:e.className,z:getComputedStyle(e).zIndex})), popovers: document.querySelectorAll(":popover-open").length})).catch(() => null));
  await page.screenshot({path: path.join(output, "failure.png")});
  throw error;
} finally {
  await fs.writeFile(path.join(output, 'results.json'), JSON.stringify({ results, errors }, null, 2));
  await browser.close();
}
console.log(JSON.stringify(results.map(r => ({ state: r.state, width: r.width, violations: r.accessibility.map(v => v.id) })), null, 2));
