const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");
const vm = require("node:vm");
const sourcePath = process.argv[2] ?? path.resolve(__dirname, "../../../inst/app/www/app.js");
const javascript = fs.readFileSync(sourcePath, "utf8").replace(/\r\n/g, "\n");
const extract = pattern => {
  const match = javascript.match(pattern);
  assert.ok(match, `Missing ${pattern}`);
  return match[0];
};
const source = [
  extract(/  const choiceInputTypes = .*?;\n/s),
  extract(/  function isEditableTarget\(target\) \{.*?\n  }\n/s),
  extract(/  function handleShortcut\(event\) \{.*?\n  }\n/s)
].join("\n");

const moves = [];
const actions = [];
let dialogOpen = false;
const context = vm.createContext({
  dialogOwnedEscapeEvents: new Set(),
  askRillReadingTelemetryPaused: false,
  compactReaderMode: { matches: false },
  compactSurface: "queue",
  activeEntryId: null,
  visibleDialogOwnsEscape: () => dialogOpen,
  moveStory: direction => moves.push(direction) || true,
  openOriginal: () => actions.push("original") || true,
  toggleReaderAction: id => actions.push(id) || true,
  document: { querySelector: () => null },
  window: {}
});
vm.runInContext(source, context);

const field = (tagName, type) => ({
  tagName,
  type,
  isContentEditable: false,
  closest: () => ({ tagName, type })
});
const press = (key, target = { closest: () => null }) => {
  let prevented = false;
  context.handleShortcut({
    key,
    target,
    preventDefault: () => {
      prevented = true;
    }
  });
  return prevented;
};

assert.equal(context.isEditableTarget(field("INPUT", "text")), true);
assert.equal(context.isEditableTarget(field("TEXTAREA", "")), true);
assert.equal(context.isEditableTarget(field("INPUT", "radio")), false);
assert.equal(context.isEditableTarget(field("INPUT", "checkbox")), false);

assert.equal(press("j", field("INPUT", "radio")), true, "A focused view choice keeps J");
assert.deepEqual(moves, [1]);
assert.equal(press("k", field("INPUT", "text")), false, "Typing never moves stories");

dialogOpen = true;
assert.equal(press("j"), false, "Stories behind a dialog stay put");
assert.deepEqual(moves, [1]);

dialogOpen = false;
assert.equal(press("k"), true);
assert.deepEqual(moves, [1, -1]);
assert.equal(press("o"), true);
assert.equal(press("s"), true);
assert.equal(press("f"), true);
assert.deepEqual(actions, ["original", "toggle_save", "toggle_star"]);
