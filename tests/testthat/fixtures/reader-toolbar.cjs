const assert = require("node:assert/strict");
const fs = require("node:fs");
const vm = require("node:vm");
const script = fs.readFileSync(process.argv[2], "utf8").replace(/\r\n/g, "\n");
const source = script.match(/  function handleAskRillEscape\(event\) \{.*?\n  }\n/s);
assert.ok(source, "Reader Escape handler must exist");

for (const state of ["copy", "menu", "focus", "modal", "ask"]) {
  const actions = [];
  const trigger = { focus: () => actions.push("focus menu") };
  const context = vm.createContext({
    document: {
      querySelector(selector) {
        if (selector === ".reading-copy-popover.show" && state === "copy") {
          return { querySelector: () => ({ click: () => actions.push("close copy") }) };
        }
        if (selector === ".article-copy-link") return { focus: () => actions.push("focus copy") };
        if (selector === ".article-menu.show" && state === "menu") return {};
        return null;
      },
      getElementById: () => trigger
    },
    window: { bootstrap: { Dropdown: { getOrCreateInstance: () => ({ hide: () => actions.push("close menu") }) } } },
    visibleDialogOwnsEscape: () => state === "modal",
    dialogOwnedEscapeEvents: { add: () => actions.push("modal owns Escape") },
    savedPaneLayout: state !== "ask" ? {} : null,
    focusPane: mode => actions.push(mode),
    readerAgentElements: () => ({ layout: { classList: { contains: () => false } } }),
    setSidebarExpanded: () => actions.push("close ask")
  });
  vm.runInContext(source[0], context);
  const event = { key: "Escape", preventDefault: () => actions.push("handled") };
  context.handleAskRillEscape(event);
  assert.deepEqual(actions, {
    copy: ["close copy", "focus copy", "handled"],
    menu: ["close menu", "focus menu", "handled"],
    focus: ["restore", "handled"],
    modal: ["modal owns Escape"],
    ask: ["close ask", "handled"]
  }[state]);
  for (const ignored of [{ key: "s" }, { key: "Escape", defaultPrevented: true }, { key: "Escape", isComposing: true }]) {
    actions.length = 0;
    context.handleAskRillEscape({ ...event, ...ignored });
    assert.deepEqual(actions, []);
  }
}
