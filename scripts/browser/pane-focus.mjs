import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {chromium} from 'playwright';

const url = process.env.RILL_BROWSER_URL || 'http://127.0.0.1:3876';
assert.ok(['127.0.0.1', 'localhost'].includes(new URL(url).hostname));
const browser = await chromium.launch({channel: 'chrome', headless: true});

try {
  const page = await browser.newPage({
    viewport: {width: 1680, height: 1000},
    reducedMotion: 'reduce',
  });
  const errors = [];
  page.on('pageerror', error => errors.push(error.message));
  if (process.env.RILL_LEGACY_MEDIA_QUERIES === 'true') {
    await page.addInitScript(() => {
      const matchMedia = window.matchMedia.bind(window);
      window.rillLegacyMediaQueries = 0;
      window.matchMedia = query => {
        const media = matchMedia(query);
        // Exercise Rill's fallback without changing third-party component APIs.
        if (document.currentScript?.textContent.includes('const desktopReaderMode')) {
          window.rillLegacyMediaQueries += 1;
          media.addEventListener = undefined;
          media.removeEventListener = undefined;
        }
        return media;
      };
    });
  }
  const state = () => page.evaluate(() => [
    'navigation_sidebar', 'story_sidebar', 'reader_agent_sidebar',
  ].map(id => {
    const layout = document.getElementById(id).parentElement;
    return {
      id,
      expanded: !layout.classList.contains('sidebar-collapsed'),
      width: layout.style.getPropertyValue('--_sidebar-width'),
    };
  }));
  const waitForFocusedAsk = () => page.waitForFunction(() => {
    const sidebar = document.getElementById('reader_agent_sidebar');
    return document.getElementById('rill-app').dataset.paneFocus === 'ask' &&
      !sidebar.parentElement.classList.contains('sidebar-collapsed') &&
      !document.querySelector('.transitioning') &&
      sidebar.getBoundingClientRect().width >= innerWidth - 1;
  });

  await page.goto(url);
  await page.waitForFunction(() => window.rillUiAudit?.().appBusy === 'false');
  await page.getByRole('button', {name: 'Read the anchor source', exact: true}).click();
  await page.waitForSelector('#reader-document');
  for (const id of ['navigation_sidebar', 'story_sidebar']) {
    const resize = page.locator(
      `.bslib-sidebar-layout:has(> #${id}) > .bslib-sidebar-resize-handle`,
    );
    await resize.press('Shift+ArrowRight');
  }
  const before = await state();
  const readingScroll = await page.locator('.reader-scroll').evaluate(element => {
    element.scrollTop = 120;
    return element.scrollTop;
  });
  const focusAsk = page.getByRole('button', {name: 'Focus Ask Rill', exact: true}).first();
  await focusAsk.click();
  await waitForFocusedAsk();
  const composer = page.locator('.tiptap');
  const draft = 'A draft that survives pane focus.';
  await composer.fill(draft);
  await fs.mkdir('../../artifacts/ui-followups-responsive', {recursive: true});
  await page.screenshot({path: '../../artifacts/ui-followups-responsive/focused-chat.png'});

  for (const width of [1024, 768, 1440]) {
    await page.setViewportSize({width, height: 1000});
    await waitForFocusedAsk();
    assert.equal(await composer.innerText(), draft);
    assert.equal(await page.evaluate(() => window.rillUiAudit().horizontalOverflow), false);
    assert.equal(await composer.evaluate(
      element => document.activeElement === element,
    ), true, `Ask Rill composer keeps keyboard focus at ${width}px`);
  }

  await page.getByRole('button', {name: 'Focus Reading', exact: true}).last().click();
  await page.getByRole('button', {name: 'Restore layout', exact: true}).first().click();
  await page.waitForFunction(() => !document.querySelector('.transitioning'));
  assert.deepEqual(await state(), before);
  await page.waitForFunction(expected =>
    document.querySelector('.reader-scroll').scrollTop === expected, readingScroll);
  assert.equal(await focusAsk.evaluate(element => document.activeElement === element), true);

  await focusAsk.click();
  await waitForFocusedAsk();
  assert.equal(await composer.innerText(), draft);
  await page.keyboard.press('Escape');
  await page.waitForFunction(() =>
    !document.getElementById('rill-app').hasAttribute('data-pane-focus'));
  assert.deepEqual(await state(), before);
  await page.evaluate(() => document.documentElement.style.fontSize = '200%');
  await page.waitForFunction(() =>
    getComputedStyle(document.querySelector('shiny-chat-container')).fontSize === '32px');
  await page.setViewportSize({width: 320, height: 700});
  await page.waitForFunction(() => !document.querySelector('.transitioning'));
  assert.equal(await page.evaluate(() => window.rillUiAudit().horizontalOverflow), false);
  assert.deepEqual(errors, []);
  if (process.env.RILL_LEGACY_MEDIA_QUERIES === 'true') {
    assert.ok(await page.evaluate(() => window.rillLegacyMediaQueries >= 4));
  }
  console.log('Pane widths, breakpoint focus, draft, scalable text and narrow reflow passed');
} finally {
  await browser.close();
}
