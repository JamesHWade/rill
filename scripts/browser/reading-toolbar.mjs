import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import {chromium} from 'playwright';

const url = process.env.RILL_BROWSER_URL || 'http://127.0.0.1:3876';
assert.ok(['127.0.0.1', 'localhost'].includes(new URL(url).hostname));
const output = new URL('../../artifacts/reading-toolbar/', import.meta.url);
await fs.mkdir(output, {recursive: true});
const browser = await chromium.launch({channel: 'chrome', headless: true});
const results = [];
function luminance(rgb) {
  const channels = rgb.match(/[\d.]+/g).slice(0, 3).map(Number).map(n => {
    const c = n / 255;
    return c <= 0.04045 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4;
  });
  return channels.reduce((sum, c, i) => sum + c * [0.2126, 0.7152, 0.0722][i], 0);
}
try {
  for (const width of [1440, 1024, 390, 320]) {
    const page = await browser.newPage({viewport: {width, height: 900}, reducedMotion: 'reduce'});
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.goto(`${url}?stress=1`);
    await page.waitForFunction(() => window.rillUiAudit?.().appBusy === 'false');
    if (width === 1024) {
      await page.getByRole('button', {name: 'Library', exact: true}).click();
      await page.getByRole('button', {name: 'Manage feeds', exact: true}).waitFor();
    }
    await page.locator('.story-card').first().click();
    await page.locator('#reader-document').waitFor();
    assert.equal(await page.locator('#reader_previous').isDisabled(), true);
    assert.equal(await page.locator('#reader_next').isEnabled(), true);
    for (const theme of ['light', 'dark']) {
      await page.emulateMedia({colorScheme: theme});
      await page.waitForFunction(theme => document.documentElement.dataset.bsTheme === theme, theme);
      await page.keyboard.press('Tab');
      await page.locator('#reader_next').focus();
      const colors = await page.locator('#reader_next').evaluate(el => {
        const style = getComputedStyle(el);
        return {outline: style.outlineColor, background: style.backgroundColor,
          width: style.outlineWidth, focused: el.matches(':focus-visible')};
      });
      assert.equal(colors.focused, true);
      assert.equal(colors.width, '3px');
      const a = luminance(colors.outline), b = luminance(colors.background);
      const contrast = (Math.max(a, b) + 0.05) / (Math.min(a, b) + 0.05);
      assert.ok(contrast >= 3, `Focus ring contrast ${contrast} at ${width}px in ${theme}`);
      await page.locator('#reader_more').click();
      await page.locator('.article-menu.show').waitFor();
      const menu = await page.locator('.article-menu').boundingBox();
      assert.ok(menu.x >= 0 && menu.x + menu.width <= width);
      await page.keyboard.press('Escape');
      await page.locator('.article-menu.show').waitFor({state: 'hidden'});
      await page.locator('.article-copy-link').click();
      await page.locator('.reading-copy-popover.show').waitFor();
      await page.keyboard.press('Escape');
      await page.locator('.reading-copy-popover.show').waitFor({state: 'hidden'});
      await page.locator('.reader-scroll').evaluate(el => { el.scrollTop = 600; });
      const sticky = await page.locator('.article-actions').evaluate(el =>
        Math.abs(el.getBoundingClientRect().top - el.closest('.reader-scroll').getBoundingClientRect().top));
      assert.ok(sticky <= 1, `Toolbar stays visible while scrolling: ${sticky}`);
      await page.locator('.reader-scroll').evaluate(el => { el.scrollTop = 0; });
      if (width >= 768) {
        await page.locator('#reader_focus').click();
        await page.waitForFunction(() => document.querySelector('#reader_focus').getAttribute('aria-pressed') === 'true');
        await page.locator('#reader_focus').click();
        await page.waitForFunction(() => document.querySelector('#reader_focus').getAttribute('aria-pressed') === 'false');
      }
      await page.locator('#reader_ask').click();
      await page.locator('.tiptap').waitFor();
      await page.keyboard.press('Escape');
      await page.waitForFunction(() => document.getElementById('reader_agent_sidebar').parentElement.classList.contains('sidebar-collapsed'));
      assert.equal(await page.evaluate(() => window.rillUiAudit().horizontalOverflow), false);
      await page.addScriptTag({path: new URL('./node_modules/axe-core/axe.min.js', import.meta.url).pathname});
      const violations = await page.evaluate(async () => (await axe.run(document,
        {runOnly: ['wcag2a', 'wcag2aa', 'wcag21aa']})).violations.map(v => v.id));
      assert.deepEqual(violations, []);
      await page.screenshot({path: new URL(`${width}-${theme}.png`, output).pathname});
      results.push({width, theme, focusContrast: contrast, stickyOffset: sticky, violations});
    }
    assert.deepEqual(errors, []);
    await page.close();
  }
  await fs.writeFile(new URL('verification.json', output), JSON.stringify(results, null, 2));
  console.log('Toolbar navigation, focus contrast, menus, provenance, Ask Rill, sticky position, and reflow passed.');
} finally {
  await browser.close();
}
