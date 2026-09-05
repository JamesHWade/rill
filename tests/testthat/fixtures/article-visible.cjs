const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");
const javascript = fs.readFileSync(process.argv[2], "utf8").replace(/\r\n/g, "\n");
const source = javascript.match(/  function reportArticleTextReady\(\) \{.*?\n  }\n/s);
assert.ok(source);
const frames = [];
const reports = [];
const pending = { id: "opening", started: 100 };
let article = {
  dataset: { openId: "opening" }, textContent: "Article text",
  isConnected: true, getClientRects: () => [1]
};
const context = vm.createContext({
  pendingArticleTiming: pending,
  document: { getElementById: () => article },
  performance: { now: () => 500 },
  window: {
    requestAnimationFrame: callback => frames.push(callback),
    Shiny: { setInputValue: (name, value) => reports.push({ name, value }) }
  }
});
vm.runInContext(source[0], context);
context.reportArticleTextReady();
context.reportArticleTextReady();
assert.equal(frames.length, 1, "Mutations must not schedule duplicate reports");
frames.shift()();
article.isConnected = false;
article = { ...article, isConnected: true };
frames.shift()();
assert.equal(reports.length, 0, "A replaced node must not count as visible");
frames.shift()();
frames.shift()();
assert.equal(reports.length, 1);
assert.equal(reports[0].name, "article_visible");
assert.equal(reports[0].value.elapsed_ms, 400);
assert.equal(reports[0].value.id, "opening");
context.reportArticleTextReady();
assert.equal(reports.length, 1);
