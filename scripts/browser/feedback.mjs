import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import {createRequire} from 'node:module';
import {chromium} from 'playwright';

const require = createRequire(import.meta.url);
const url = process.env.RILL_BROWSER_URL || 'http://127.0.0.1:3876';
assert.ok(['127.0.0.1', 'localhost'].includes(new URL(url).hostname), 'Use a local fixture');
const output = path.resolve('../../artifacts/reader-feedback-audit');
await fs.mkdir(output, {recursive: true});
const browser = await chromium.launch({channel: 'chrome', headless: true});
const results = [];
try {
  for (const width of [320, 390, 1440]) {
    const page = await browser.newPage({viewport: {width, height: 900}, reducedMotion: 'reduce'});
    page.setDefaultTimeout(20000);
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    async function audit(state) {
      await page.getByRole('dialog').waitFor();
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
    await page.getByRole('button', {name: 'Rate this Orientation', exact: true}).click();
    await page.getByRole('dialog').waitFor();
    assert.equal(await page.getByRole('radio', {name: 'Helpful', exact: true}).isChecked(), false);
    await page.getByRole('button', {name: 'Save rating', exact: true}).click();
    await page.getByText('Choose Helpful or Not helpful before saving.', {exact: true}).waitFor();
    await page.getByRole('radio', {name: 'Helpful', exact: true}).check();
    await page.getByText('Review the exact output being rated', {exact: true}).click();
    await page.getByRole('dialog').getByText('Source Document', {exact: true}).first().waitFor();
    await page.getByRole('region', {name: 'Output being rated', exact: true}).press('ArrowDown');
    await page.waitForFunction(() => document.querySelector('[aria-label="Output being rated"]').scrollTop > 0);
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
    await page.getByRole('button', {name: 'Read the anchor source', exact: true}).click();
    await page.getByRole('button', {name: 'Ask Rill', exact: true}).click();
    await page.getByRole('link', {name: 'Rate an earlier response', exact: true}).click();
    await page.getByRole('button', {name: 'Review this response', exact: true}).click();
    await page.getByText('Review the exact output being rated', {exact: true}).click();
    await page.getByText('Interpretation: keep source material separate from generated explanation.', {exact: true}).waitFor();
    await page.getByRole('radio', {name: 'Helpful', exact: true}).check();
    await audit('response-rating');
    await page.getByRole('button', {name: 'Save rating', exact: true}).click();
    await page.getByRole('dialog').waitFor({state: 'hidden'});
    await page.waitForFunction(() => document.activeElement?.id === 'choose_feedback_response');
    await page.getByRole('button', {name: 'My saved ratings', exact: true}).click();
    await audit('saved-ratings');
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
    await page.getByRole('combobox', {name: 'Choose an output to review', exact: true}).click();
    await page.locator('.selectize-dropdown-content .option').filter({hasText: 'Ask Rill'}).click();
    await page.getByRole('button', {name: 'Review rating', exact: true}).click();
    await page.getByRole('button', {name: 'Cancel', exact: true}).click();
    await page.getByRole('dialog').waitFor({state: 'hidden'});
    await page.waitForFunction(() => document.activeElement?.id === 'review_feedback');
    await page.getByRole('button', {name: 'My saved ratings', exact: true}).click();
    await page.getByRole('combobox', {name: 'Choose an output to review', exact: true}).click();
    await page.locator('.selectize-dropdown-content .option').filter({hasText: 'Ask Rill'}).click();
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
    await page.close();
  }
} finally {
  await fs.writeFile(path.join(output, 'results.json'), JSON.stringify(results, null, 2) + '\n');
  await browser.close();
}
