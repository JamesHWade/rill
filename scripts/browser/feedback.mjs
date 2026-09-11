import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createRequire} from 'node:module';
import {chromium} from 'playwright';

const require = createRequire(import.meta.url);
const url = process.env.RILL_BROWSER_URL || 'http://127.0.0.1:3876';
assert.ok(['127.0.0.1', 'localhost'].includes(new URL(url).hostname), 'Use a local fixture');
const output = path.resolve(process.env.RILL_BROWSER_OUTPUT || '../../artifacts/ui-followups-feedback');
await fs.mkdir(output, {recursive: true});
const browser = await chromium.launch({channel: 'chrome', headless: true});
const results = [];
try {
  for (const width of [320, 390, 1440]) {
    const page = await browser.newPage({viewport: {width, height: 900}, reducedMotion: 'reduce'});
    page.setDefaultTimeout(20000);
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    async function audit(state, dialog = true) {
      if (dialog) await page.getByRole('dialog').waitFor();
      await page.evaluate(async () => {
        await Promise.all(document.getAnimations().filter(a => Number.isFinite(a.effect.getComputedTiming().iterations)).map(a => a.finished.catch(() => {})));
      });
      await page.addScriptTag({path: require.resolve('axe-core/axe.min.js')});
      const result = await page.evaluate(async () => ({
        ui: window.rillUiAudit(),
        violations: (await axe.run(document, {runOnly: {type: 'tag', values: ['wcag2a', 'wcag2aa', 'wcag21aa']}})).violations.map(v => ({id: v.id, impact: v.impact, targets: v.nodes.map(n => n.target)}))
      }));
      results.push({width, state, ...result});
      await page.screenshot({path: path.join(output, `${width}-${state}.png`)});
      assert.equal(result.ui.horizontalOverflow, false, `${state}: horizontal overflow`);
      assert.deepEqual(result.violations, [], `${state}: accessibility violations`);
      assert.deepEqual(errors, [], `${state}: browser errors`);
      console.log(`${width}: ${state} passed`);
    }
    await page.goto(`${url}?feedback=fixture`);
    await page.waitForFunction(() => window.rillUiAudit?.().appBusy === 'false');
    await page.getByRole('button', {name: 'Rate this Orientation', exact: true}).waitFor();
    await audit('orientation-ready', false);
    await page.getByRole('button', {name: 'Rate this Orientation', exact: true}).click();
    await page.getByRole('dialog').waitFor();
    assert.equal(await page.getByRole('radio', {name: 'Helpful', exact: true}).isChecked(), false);
    await page.getByRole('button', {name: 'Save rating', exact: true}).click();
    await page.getByText('Choose Helpful or Not helpful before saving.', {exact: true}).waitFor();
    await page.getByRole('radio', {name: 'Helpful', exact: true}).check();
    await page.getByRole('dialog').getByText('Source Document', {exact: true}).first().waitFor();
    assert.equal(await page.getByRole('region', {name: 'Output being rated'}).evaluate(el => getComputedStyle(el).maxHeight), 'none');
    await page.getByText('Add reasons or a comment (optional)', {exact: true}).click();
    await page.getByRole('checkbox', {name: 'Clarity', exact: true}).check();
    await page.getByLabel('Optional comment (up to 2,000 characters)', {exact: true}).fill('The source boundary is clear.');
    await audit('orientation-rating');
    await page.getByRole('button', {name: 'Save rating', exact: true}).click();
    await page.getByRole('dialog').waitFor({state: 'hidden'});
    await page.getByRole('button', {name: 'Rate this Orientation', exact: true}).click();
    await page.getByRole('dialog').waitFor();
    assert.equal(await page.getByRole('radio', {name: 'Helpful', exact: true}).isChecked(), true);
    assert.equal(await page.getByLabel('Optional comment (up to 2,000 characters)', {exact: true}).inputValue(), 'The source boundary is clear.');
    await page.getByRole('radio', {name: 'Not helpful', exact: true}).check();
    await page.getByRole('button', {name: 'Save rating', exact: true}).click();
    await page.getByRole('dialog').waitFor({state: 'hidden'});
    await page.locator('.orientation-read').first().click();
    await page.getByRole('button', {name: 'Ask Rill', exact: true}).click();
    await page.getByRole('link', {name: 'Rate an earlier response', exact: true}).click();
    await page.getByRole('button', {name: 'Review this response', exact: true}).click();
    await page.locator('.feedback-answer').getByText('Interpretation: keep source material separate from generated explanation.', {exact: true}).waitFor();
    await page.getByRole('radio', {name: 'Helpful', exact: true}).check();
    assert.equal(await page.locator('.feedback-answer .action-button').count(), 0);
    assert.equal(await page.locator('.feedback-answer [id]').count(), 0);
    await page.getByText('Original answer text', {exact: true}).click();
    await page.context().grantPermissions(['clipboard-read', 'clipboard-write']);
    await page.getByRole('button', {name: 'Copy original answer', exact: true}).click();
    await page.getByRole('status').filter({hasText: 'Copied.'}).waitFor();
    const originalAnswer = await page.locator('.feedback-original').textContent();
    assert.ok(originalAnswer.startsWith('\n\n## Source boundary'));
    assert.equal(await page.evaluate(() => navigator.clipboard.readText()), originalAnswer);
    await page.getByText('Original answer text', {exact: true}).click();
    await audit('response-rating');
    if (width === 1440) {
      await page.evaluate(() => document.documentElement.style.fontSize = '200%');
      await audit('response-rating-enlarged-text');
      await page.evaluate(() => document.documentElement.style.fontSize = '');
    }
    await page.getByRole('button', {name: 'Save rating', exact: true}).click();
    await page.getByRole('dialog').waitFor({state: 'hidden'});
    await page.waitForFunction(() => document.activeElement?.id === 'choose_feedback_response');
    await page.getByRole('button', {name: 'My saved ratings', exact: true}).click();
    await audit('saved-ratings');
    await page.getByRole('radio', {name: /Orientation · Not helpful/}).check();
    await page.getByRole('button', {name: 'Review rating', exact: true}).click();
    await page.getByRole('button', {name: 'Cancel', exact: true}).click();
    await page.getByRole('dialog').waitFor({state: 'hidden'});
    await page.getByRole('button', {name: 'My saved ratings', exact: true}).click();
    assert.equal(await page.getByRole('radio', {name: /Orientation · Not helpful/}).isChecked(), true);

    const [download] = await Promise.all([
      page.waitForEvent('download'),
      page.getByRole('link', {name: /Download my ratings/}).click()
    ]);
    const downloaded = await fs.readFile(await download.path(), 'utf8');
    const records = Object.values(JSON.parse(downloaded));
    assert.equal(records.length, 2);
    assert.equal(records.find(r => r.snapshot.kind === 'orientation').rating, 'not_helpful');
    assert.equal(records.find(r => r.snapshot.kind === 'question').snapshot.provenance.model, 'fixture-model');
    assert.ok(!downloaded.includes('PRIVATE_OTHER_READER_OUTPUT'));
    await page.getByRole('radio', {name: /Explain the source boundary.*Ask Rill/}).check();
    await page.getByRole('button', {name: 'Review rating', exact: true}).click();
    await page.getByRole('button', {name: 'Cancel', exact: true}).click();
    await page.getByRole('dialog').waitFor({state: 'hidden'});
    await page.waitForFunction(() => document.activeElement?.id === 'review_feedback');
    await page.getByRole('button', {name: 'My saved ratings', exact: true}).click();
    await page.getByRole('radio', {name: /Explain the source boundary.*Ask Rill/}).check();
    await page.getByRole('button', {name: 'Review rating', exact: true}).click();
    await page.getByRole('button', {name: 'Withdraw rating', exact: true}).click();
    await page.getByRole('dialog').waitFor({state: 'hidden'});
    await page.getByRole('button', {name: 'My saved ratings', exact: true}).click();
    const [remainingDownload] = await Promise.all([
      page.waitForEvent('download'), page.getByRole('link', {name: /Download my ratings/}).click()
    ]);
    const remaining = Object.values(JSON.parse(await fs.readFile(await remainingDownload.path(), 'utf8')));
    assert.equal(remaining.length, 1);
    assert.equal(remaining[0].snapshot.kind, 'orientation');
    await page.goto(`${url}?feedback=fixture&resume=1`);
    await page.waitForFunction(() => window.rillUiAudit?.().appBusy === 'false');
    if (width < 768) {
      await page.locator('.orientation-browse').click();
      await page.locator('.compact-library-trigger').click();
    }
    await page.getByRole('button', {name: 'Reopen last answer', exact: true}).click();
    await page.getByRole('button', {name: 'Rate this response', exact: true}).click();
    await page.getByRole('dialog').waitFor();
    await page.getByRole('dialog').locator('.feedback-answer').getByText('Interpretation: keep source material separate from generated explanation.', {exact: true}).waitFor();
    await audit('current-response-rating');
    await page.getByRole('button', {name: 'Cancel', exact: true}).click();
    await page.getByRole('dialog').waitFor({state: 'hidden'});
    await page.waitForFunction(() => document.activeElement?.id === 'rate_response');
    await page.close();
  }
} finally {
  await fs.writeFile(path.join(output, 'results.json'), JSON.stringify(results, null, 2) + '\n');
  await browser.close();
}
