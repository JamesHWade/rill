import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createRequire} from 'node:module';
import {chromium} from 'playwright';

const require = createRequire(import.meta.url);
const url = process.env.RILL_BROWSER_URL || 'http://127.0.0.1:3876';
const label = process.env.RILL_CAPTURE_LABEL;
assert.ok(['before', 'after'].includes(label), 'Set RILL_CAPTURE_LABEL to before or after');
assert.ok(['127.0.0.1', 'localhost'].includes(new URL(url).hostname), 'Use a local fixture');
const output = path.resolve('../../artifacts/responsive-comparison', label);
await fs.mkdir(output, {recursive: true});
const browser = await chromium.launch({channel: 'chrome', headless: true});
const page = await browser.newPage({viewport: {width: 390, height: 900}, colorScheme: 'light', reducedMotion: 'reduce'});
page.setDefaultTimeout(15000);
const results = [];
async function ready() {
  await page.locator('#list_title h1').waitFor({state: 'attached', timeout: 25000});
}
async function capture(state, width) {
  await page.evaluate(async () => {
    await Promise.all(document.getAnimations().filter(a => Number.isFinite(a.effect.getComputedTiming().iterations)).map(a => a.finished.catch(() => {})));
  });
  await page.addScriptTag({path: require.resolve('axe-core/axe.min.js')});
  const result = await page.evaluate(async () => ({
    ui: window.rillUiAudit(),
    violations: (await axe.run(document, {runOnly: {type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa']}})).violations.map(v => ({id: v.id, impact: v.impact, targets: v.nodes.map(n => n.target)}))
  }));
  results.push({state, width, ...result});
  await page.screenshot({path: path.join(output, `${width}-${state}.png`)});
  console.log(`${label}: ${width} ${state}, ${result.violations.length} violations`);
}
try {
  for (const width of [390, 1440]) {
    await page.setViewportSize({width, height: 900});
    await page.goto(`${url}?delay=17`);
    await page.locator('#rill-system-status:not([hidden])').waitFor();
    await page.waitForFunction(() => !document.querySelector('#list_title')?.textContent.trim());
    await capture('loading', width);
    await ready();
    await capture('orientation', width);
    await page.evaluate(() => window.rillOpenLibrary());
    await capture('library', width);
    await page.evaluate(() => window.rillOpenQueue());
    await capture('queue', width);
    await page.locator('.story-card').first().click();
    await page.waitForSelector('#reader-document');
    await capture('reading', width);
    await page.getByRole('button', {name: 'Ask Rill', exact: true}).click();
    await capture('ask-rill', width);
    await page.keyboard.press('Escape');
    await page.evaluate(() => window.rillOpenLibrary());
    await page.getByRole('button', {name: 'Manage feeds', exact: true}).click();
    await page.getByRole('dialog').waitFor();
    await page.waitForFunction(() => document.querySelector('.modal.show')?.contains(document.activeElement));
    await capture('manage-feeds', width);
    await page.getByRole('button', {name: 'Done', exact: true}).click();
    await page.getByRole('dialog').waitFor({state: 'hidden'});
    await page.getByRole('radio', {name: 'Starred', exact: true}).check();
    await page.getByRole('heading', {name: 'No starred stories yet', exact: true}).waitFor();
    await capture('empty', width);
    await page.evaluate(() => Shiny.setInputValue('audit_error', Date.now()));
    await page.locator('.shiny-notification-error').waitFor();
    await capture('native-error', width);
    await page.evaluate(() => Shiny.setInputValue('audit_disconnect', Date.now()));
    await page.getByRole('button', {name: 'Reload Rill', exact: true}).waitFor();
    await capture('disconnected', width);
    for (const access of ['pending', 'denied']) {
      await page.goto(`${url}?access=${access}`);
      await page.getByRole('dialog').waitFor();
      await capture(`access-${access}`, width);
    }
    await page.goto(`${url}?agent=fixture`);
    await ready();
    await page.evaluate(() => window.rillOpenQueue());
    await page.locator('.story-card').first().click();
    await page.waitForSelector('#reader-document');
    await page.getByRole('button', {name: 'Ask Rill', exact: true}).click();
    await page.evaluate(() => Shiny.setInputValue('audit_agent_status', 'running'));
    if (label === 'after') await page.locator('.reader-agent-run-status').waitFor();
    else await page.waitForTimeout(500);
    await capture('agent-running', width);
    await page.evaluate(() => Shiny.setInputValue('audit_agent_stream', Date.now()));
    await page.getByText('This is a synthetic response for browser validation.', {exact: false}).waitFor();
    await capture('agent-completed', width);
    await page.evaluate(() => Shiny.setInputValue('audit_agent_status', 'interrupted'));
    if (label === 'after') await page.getByRole('button', {name: 'Retry', exact: true}).waitFor();
    else await page.waitForTimeout(500);
    await capture('agent-interrupted', width);
  }
  if (label === 'after') {
    assert.deepEqual(results.flatMap(r => r.violations), [], 'No WCAG violations in final captures');
    assert.ok(results.every(r => !r.ui.horizontalOverflow), 'No viewport overflow');
  }
} finally {
  await fs.writeFile(path.join(output, 'results.json'), JSON.stringify(results, null, 2));
  await browser.close();
}
