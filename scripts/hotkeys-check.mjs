import assert from "node:assert/strict";
import fs from "node:fs";
import vm from "node:vm";

const listeners = new Map();
const clicks = [];

function storyCard(name, selected = false) {
  return {
    classList: {
      contains(className) {
        return className === "is-selected" && selected;
      }
    },
    click() {
      clicks.push(name);
    },
    scrollIntoView() {}
  };
}

const cards = [storyCard("first"), storyCard("second", true), storyCard("third")];
const originalLink = {
  click() {
    clicks.push("original");
  }
};
const readerActions = {
  toggle_save: {
    click() {
      clicks.push("save");
    }
  },
  toggle_star: {
    click() {
      clicks.push("star");
    }
  }
};
const document = {
  visibilityState: "visible",
  addEventListener(type, handler) {
    listeners.set(type, handler);
  },
  getElementById(id) {
    return readerActions[id] ?? null;
  },
  querySelector(selector) {
    if (selector === ".original-link") return originalLink;
    return null;
  },
  querySelectorAll(selector) {
    return selector === ".story-card" ? cards : [];
  }
};
const context = {
  console,
  document,
  MutationObserver: class {
    observe() {}
  },
  window: {
    crypto: { randomUUID: () => "event-id" },
    setInterval() {}
  }
};

vm.runInNewContext(fs.readFileSync("inst/app/www/app.js", "utf8"), context);
const keydown = listeners.get("keydown");
assert.equal(typeof keydown, "function");

function press(key, target = { closest: () => null, isContentEditable: false }) {
  let prevented = false;
  keydown({
    key,
    target,
    altKey: false,
    ctrlKey: false,
    metaKey: false,
    shiftKey: false,
    defaultPrevented: false,
    isComposing: false,
    preventDefault() {
      prevented = true;
    }
  });
  return prevented;
}

assert.equal(press("j"), true);
assert.deepEqual(clicks, ["third"]);

clicks.length = 0;
assert.equal(press("k"), true);
assert.deepEqual(clicks, ["first"]);

clicks.length = 0;
assert.equal(press("o"), true);
assert.deepEqual(clicks, ["original"]);

clicks.length = 0;
assert.equal(press("s"), true);
assert.deepEqual(clicks, ["save"]);

clicks.length = 0;
assert.equal(press("f"), true);
assert.deepEqual(clicks, ["star"]);

clicks.length = 0;
const input = { closest: () => input, isContentEditable: false };
assert.equal(press("j", input), false);
assert.deepEqual(clicks, []);

console.log("Keyboard shortcut check passed.");
